%%
%% Copyright (c) 2014-2015 Bas Wegh
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in all
%% copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.
%%

%% @private
-module(wamper_protocol).

-export([deserialize/2, serialize/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(JSONB_SEPARATOR, <<24>>).

deserialize(Buffer, Encoding) ->
  case lists:member(Encoding, [raw_msgpack, raw_json, msgpack_batched, raw_erlbin]) of
    true -> deserialize_binary(Buffer, [], Encoding);
    _ -> deserialize_text(Buffer, [], Encoding)
  end.

serialize(Erwa, Enc) ->
  WAMP = case {lists:member(Enc, [erlbin, raw_erlbin]), is_tuple(Erwa)} of
           {false, true} -> wamper_converter:to_wamp(Erwa);
           _ -> Erwa
         end,
  serialize_message(WAMP, Enc).


%% @private
-spec deserialize_text(Buffer :: binary(), Messages :: list(), Encoding :: atom()) ->
  {[Message :: term()], NewBuffer :: binary()}.
deserialize_text(Buffer, Messages, erlbin) ->
  Msg = binary_to_term(Buffer),
  true = wamper_validator:is_valid_message(Msg),
  {[Msg | Messages], <<"">>};
deserialize_text(Buffer, Messages, msgpack) ->
  case msgpack:unpack_stream(Buffer, [{format, map}]) of
    {error, incomplete} ->
      {to_erl_reverse(Messages), Buffer};
    {error, Reason} ->
      error(Reason);
    {Msg, NewBuffer} ->
      deserialize_text(NewBuffer, [Msg | Messages], msgpack)
  end;
deserialize_text(Buffer, Messages, json) ->
  %% is it possible to check the data here ?
  %% length and stuff, yet should not be needed
  {[wamper_converter:to_erl(jsx:decode(Buffer, [return_maps])) | Messages], <<"">>};
deserialize_text(Buffer, _Messages, json_batched) ->
  Wamps = binary:split(Buffer, [?JSONB_SEPARATOR], [global, trim]),
  {to_erl_reverse(lists:foldl(fun(M, List) -> [jsx:decode(M, [return_maps]) | List] end, [], Wamps)), <<"">>};
deserialize_text(Buffer, Messages, _) ->
  {to_erl_reverse(Messages), Buffer}.

%% @private
-spec deserialize_binary(Buffer :: binary(), Messages :: list(), Encoding :: atom()) ->
  {[Message :: term()], NewBuffer :: binary()}.
deserialize_binary(<<LenType:32/unsigned-integer-big, Data/binary>> = Buffer, Messages, Enc) ->
  <<Type:8, Len:24>> = <<LenType:32>>,
  case {Type, byte_size(Data) >= Len} of
    {0, true} ->
      <<EncMsg:Len/binary, NewBuffer/binary>> = Data,
      {ok, Msg} = case Enc of
                    raw_erlbin ->
                      DecMsg = binary_to_term(EncMsg),
                      true = wamper_validator:is_valid_message(DecMsg),
                      {ok, DecMsg};
                    raw_json ->
                      {ok, jsx:decode(EncMsg, [return_maps])};
                    _ ->
                      msgpack:unpack(EncMsg, [{format, map}])
                  end,
      deserialize_binary(NewBuffer, [Msg | Messages], Enc);
    {1, true} ->      %Ping
      <<Ping:Len/binary, NewBuffer/binary>> = Data,
      deserialize_binary(NewBuffer, [{ping, Ping} | Messages], Enc);
    {2, true} ->
      <<Pong:Len/binary, NewBuffer/binary>> = Data,
      deserialize_binary(NewBuffer, [{pong, Pong} | Messages], Enc);
    {_, false} ->  %Pong
      case Enc of
        raw_erlbin -> {lists:reverse(Messages), Buffer};
        _ -> {to_erl_reverse(Messages), Buffer}
      end
  end;
deserialize_binary(Buffer, Messages, Enc) ->
  case Enc of
    raw_erlbin -> {lists:reverse(Messages), Buffer};
    _ -> {to_erl_reverse(Messages), Buffer}
  end.

%% @private
serialize_message(Msg, msgpack) ->
  case msgpack:pack(Msg, [{format, map}]) of
    {error, Reason} ->
      error(wamper_msgpack, [Reason]);
    M ->
      M
  end;
serialize_message(Msg, erlbin) ->
  term_to_binary(Msg);
serialize_message(Msg, msgpack_batched) ->
  serialize(Msg, raw_msgpack);
serialize_message(Msg, json) ->
  jsx:encode(Msg);
serialize_message(Msg, json_batched) ->
  Enc = jsx:encode(Msg),
  <<Enc/binary, ?JSONB_SEPARATOR/binary>>;
serialize_message(Message, raw_erlbin) ->
  Enc = term_to_binary(Message),
  add_binary_frame(Enc);
serialize_message(Message, raw_msgpack) ->
  Enc = case msgpack:pack(Message, [{format, map}]) of
          {error, Reason} ->
            error(Reason);
          Msg ->
            Msg
        end,
  add_binary_frame(Enc);
serialize_message(Message, raw_json) ->
  Enc = jsx:encode(Message),
  add_binary_frame(Enc).

%% @private
add_binary_frame(Enc) ->
  Len = byte_size(Enc),
  <<0:8, Len:24/unsigned-integer-big, Enc/binary>>.

%% @private
to_erl_reverse(List) ->
  to_erl_reverse(List, []).

%% @private
to_erl_reverse([], List) -> List;
to_erl_reverse([H | T], Messages) ->
  to_erl_reverse(T, [wamper_converter:to_erl(H) | Messages]).