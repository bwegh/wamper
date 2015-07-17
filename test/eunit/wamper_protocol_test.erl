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
%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Jul 2015 13:51
%%%-------------------------------------------------------------------
-module(wamper_protocol_test).
-author("tihon").

-include("wamper_message_codes.hrl").
-include_lib("eunit/include/eunit.hrl").

hello_json_test() ->
  M = [?HELLO, <<"realm1">>, #{}],
  S = wamper_protocol:serialize(M, json),
  D = wamper_protocol:deserialize(S, json),
  D = {[{hello, <<"realm1">>, #{}}], <<"">>}.

hello_json_batched_test() ->
  M = [?HELLO, <<"realm1">>, #{}],
  S = wamper_protocol:serialize(M, json_batched),
  D = wamper_protocol:deserialize(S, json_batched),
  D = {[{hello, <<"realm1">>, #{}}], <<"">>}.

hello_msgpack_test() ->
  M = [?HELLO, <<"realm1">>, #{}],
  S = wamper_protocol:serialize(M, msgpack),
  D = wamper_protocol:deserialize(S, msgpack),
  D = {[{hello, <<"realm1">>, #{}}], <<"">>}.

hello_msgpack_batched_test() ->
  M = [?HELLO, <<"realm1">>, #{}],
  S = wamper_protocol:serialize(M, msgpack_batched),
  D = wamper_protocol:deserialize(S, msgpack_batched),
  D = {[{hello, <<"realm1">>, #{}}], <<"">>}.

roundtrip_test() ->
  Messages = [
    {hello, <<"realm1">>, #{roles => #{callee => #{features => #{}}, caller => #{features => #{}}}}},
    {welcome, 398475, #{}},
    {abort, #{}, invalid_argument},
    {goodbye, #{}, goodbye_and_out},
    {publish, 2343, #{exclude_me=>false}, <<"just_some_uri">>, undefined, undefined},
    {error, publish, 9873, #{}, invalid_argument, undefined, undefined},
    {published, 209384, 092834},
    {subscribe, 9834, #{}, <<"event_i_need">>},
    {error, subscribe, 9873, #{}, invalid_argument, undefined, undefined},
    {subscribed, 9283, 9898},
    {unsubscribe, 2333, 23400},
    {error, unsubscribe, 9873, #{}, invalid_argument, undefined, undefined},
    {unsubscribed, 28777},
    {event, 28882, 292839, #{publisher => 3980999}, [<<"some information">>], undefined},
    {call, 298374, #{disclose_me=>true}, <<"some_rpc">>, [3, true, <<"hello world!">>], #{<<"hello">> => <<"world">>}},
    {error, call, 982734, #{}, invalid_uri, undefined, undefined},
    {result, 98273, #{}, undefined, undefined},
    {register, 20938, #{}, <<"some_rpc">>},
    {error, register, 9873, #{}, invalid_argument, undefined, undefined},
    {registered, 9827988, 234},
    {unregister, 209384, 20938999},
    {error, unregister, 9873, #{}, invalid_argument, undefined, undefined},
    {unregistered, 9283000},
    {invocation, 328, 23444, #{}, [<<"Willi">>], undefined},
    {error, invocation, 9873, #{}, invalid_argument, undefined, undefined},
    {yield, 2987, #{}, undefined, undefined}
  ],
  Serializer =
    fun(Message, Res) ->
      Encodings = [json, msgpack, erlbin, raw_json, raw_msgpack, raw_erlbin, json_batched, msgpack_batched],
      ct:log("~p", [Message]),
      Check =
        fun(Enc, Bool) ->
          ct:log("   ~p: ", [Enc]),
          EncMsg = wamper_protocol:serialize(Message, Enc),
          ct:log("   <- ~p", [EncMsg]),
          DeEncMsg = wamper_protocol:deserialize(EncMsg, Enc),
          case DeEncMsg of
            {[Message], <<"">>} ->
              ct:log("   -> ~p, okay~n", [DeEncMsg]),
              Bool;
            _ ->
              ct:log("   -> ~p *** ERROR ***", [DeEncMsg]),
              false
          end
        end,
      Res and lists:foldl(Check, true, Encodings)
    end,
  true = lists:foldl(Serializer, true, Messages).


msgpack_error_test() ->
  M = [?HELLO, <<"realm1">>, [{<<"option_with">>, atom}]],
  try wamper_protocol:serialize(M, msgpack)
  catch _:_ ->
    ok
  end.