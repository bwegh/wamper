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

-export([deserialize/2]).
-export([serialize/2]).
-export([to_wamp/1]).
-export([to_erl/1]).

-export([is_valid_uri/1]).
-export([is_valid_uri/2]).
-export([is_valid_id/1]).
-export([is_valid_dict/1]).
-export([is_valid_dict/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(JSONB_SEPARATOR,<<24>>).

deserialize(Buffer,Encoding) ->
  case lists:member(Encoding,[raw_msgpack,raw_json,msgpack_batched,raw_erlbin]) of
    true -> deserialize_binary(Buffer,[],Encoding);
    _ -> deserialize_text(Buffer,[],Encoding)
  end.



-spec deserialize_text(Buffer :: binary(), Messages :: list(), Encoding :: atom() ) -> {[Message :: term()], NewBuffer :: binary()}.
deserialize_text(Buffer,Messages,erlbin) ->
  {[binary_to_term(Buffer)],<<"">>};
deserialize_text(Buffer,Messages,msgpack) ->
  case msgpack:unpack_stream(Buffer,[{format,map}]) of
    {error,incomplete} ->
      {to_erl_reverse(Messages),Buffer};
    {error,Reason} ->
      error(Reason);
    {Msg,NewBuffer} ->
      deserialize_text(NewBuffer,[Msg|Messages],msgpack)
  end;
deserialize_text(Buffer,Messages,json) ->
  %% is it possible to check the data here ?
  %% length and stuff, yet should not be needed
  {[to_erl(jsx:decode(Buffer,[return_maps]))|Messages],<<"">>};
deserialize_text(Buffer,_Messages,json_batched) ->
  Wamps = binary:split(Buffer,[?JSONB_SEPARATOR],[global,trim]),
  {to_erl_reverse(lists:foldl(fun(M,List) -> [jsx:decode(M,[return_maps])|List] end,[],Wamps)),<<"">>};
deserialize_text(Buffer,Messages,_) ->
  {to_erl_reverse(Messages),Buffer}.


-spec deserialize_binary(Buffer :: binary(), Messages :: list(), Encoding :: atom() ) -> {[Message :: term()], NewBuffer :: binary()}.
deserialize_binary(<<LenType:32/unsigned-integer-big,Data/binary>> = Buffer,Messages, Enc)  ->
  <<Type:8,Len:24>> = << LenType:32 >>,
  case {Type,byte_size(Data) >= Len}of
    {0,true} ->
      <<EncMsg:Len/binary,NewBuffer/binary>> = Data,
      {ok,Msg} = case Enc of
                   raw_erlbin -> {ok, binary_to_term(EncMsg)};
                   raw_json -> {ok,jsx:decode(EncMsg,[return_maps])};
                   _ -> msgpack:unpack(EncMsg,[{format,map}])
                 end,
      deserialize_binary(NewBuffer,[Msg|Messages],Enc);
    {1,true} ->
      %Ping
      <<Ping:Len/binary,NewBuffer/binary>> = Data,
      deserialize_binary(NewBuffer,[{ping,Ping}|Messages],Enc);
    {2,true} ->
      <<Pong:Len/binary,NewBuffer/binary>> = Data,
      deserialize_binary(NewBuffer,[{pong,Pong}|Messages],Enc);
      %Pong
    {_,false} ->
      case Enc of
        raw_erlbin -> {lists:reverse(Messages),Buffer};
        _ -> {to_erl_reverse(Messages),Buffer}
      end
  end;
deserialize_binary(Buffer,Messages,Enc) ->
  case Enc of
        raw_erlbin -> {lists:reverse(Messages),Buffer};
        _ -> {to_erl_reverse(Messages),Buffer}
  end.



serialize(Erwa,Enc) ->
  WAMP = case {lists:member(Enc,[erlbin,raw_erlbin]), is_tuple(Erwa)} of
           {false,true} -> to_wamp(Erwa);
           _ -> Erwa
         end,
  serialize_message(WAMP,Enc).

serialize_message(Msg,msgpack)  ->
  case msgpack:pack(Msg, [{format,map}]) of
    {error,Reason} ->
      error(wamper_msgpack,[Reason]);
    M ->
      M
  end;
serialize_message(Msg,erlbin)  ->
  term_to_binary(Msg);
serialize_message(Msg,msgpack_batched) ->
  serialize(Msg,raw_msgpack);
serialize_message(Msg,json)  ->
  jsx:encode(Msg);
serialize_message(Msg,json_batched) ->
  Enc = jsx:encode(Msg),
  <<Enc/binary, ?JSONB_SEPARATOR/binary >>;
serialize_message(Message,raw_erlbin) ->
  Enc = term_to_binary(Message),
  add_binary_frame(Enc);
serialize_message(Message,raw_msgpack) ->
  Enc = case msgpack:pack(Message, [{format,map}]) of
          {error,Reason} ->
            error(Reason);
          Msg ->
            Msg
        end,
  add_binary_frame(Enc);
serialize_message(Message,raw_json) ->
  Enc = jsx:encode(Message),
  add_binary_frame(Enc).


add_binary_frame(Enc) ->
  Len = byte_size(Enc),
  <<0:8,Len:24/unsigned-integer-big,Enc/binary>>.


to_erl_reverse(List)->
  to_erl_reverse(List,[]).

to_erl_reverse([],List) ->
  List;
to_erl_reverse([H|T],Messages) ->
  to_erl_reverse(T,[to_erl(H)|Messages]).

is_valid_uri(Uri) when is_binary(Uri) ->
  is_valid_uri(Uri,default).


is_valid_uri(Uri, _Type) when is_binary(Uri) ->
  true;
is_valid_uri(_, _) ->
  false.


is_valid_id(Id) when is_integer(Id), Id >= 0, Id < 9007199254740992 -> true;
is_valid_id(_) -> false.

is_valid_dict(Dict) ->
  is_valid_dict(Dict,default).

is_valid_dict(Dict,_Type) when is_map(Dict) -> true;
is_valid_dict(_,_) -> false.

is_valid_arguments(Arguments) when is_list(Arguments) -> true;
is_valid_arguments(undefined)  -> true;
is_valid_arguments(_)  -> false.

is_valid_argumentskw(ArgumentsKw) when is_map(ArgumentsKw) -> true;
is_valid_argumentskw(undefined)  -> true;
is_valid_argumentskw(_)  -> false.

-define(HELLO,1).
-define(WELCOME,2).
-define(ABORT,3).
-define(CHALLENGE,4).
-define(AUTHENTICATE,5).
-define(GOODBYE,6).
-define(HEARTBEAT,7).
-define(ERROR,8).

-define(PUBLISH,16).
-define(PUBLISHED,17).

-define(SUBSCRIBE,32).
-define(SUBSCRIBED,33).
-define(UNSUBSCRIBE,34).
-define(UNSUBSCRIBED,35).
-define(EVENT,36).

-define(CALL,48).
-define(CANCEL,49).
-define(RESULT,50).

-define(REGISTER,64).
-define(REGISTERED,65).
-define(UNREGISTER,66).
-define(UNREGISTERED,67).
-define(INVOCATION,68).
-define(INTERRUPT,69).
-define(YIELD,70).





to_erl([?HELLO,Realm,Details]) ->
  true = is_valid_uri(Realm),
  true = is_valid_dict(Details),
  {hello,Realm,hello_dict_to_erl(Details)};

to_erl([?WELCOME,SessionId,Details]) ->
  true = is_valid_id(SessionId),
  true = is_valid_dict(Details),
  {welcome,SessionId,dict_to_erl(Details)};

to_erl([?ABORT,Details,Reason]) ->
  true = is_valid_dict(Details),
  true = is_valid_uri(Reason),
  {abort,dict_to_erl(Details),try_error_to_erl(Reason)};


to_erl([?CHALLENGE,<<"wampcra">>,Extra]) ->
  to_erl([?CHALLENGE,wampcra,Extra]);
to_erl([?CHALLENGE,AuthMethod,Extra]) ->
  true = is_valid_dict(Extra),
  {challenge,AuthMethod,dict_to_erl(Extra)};

to_erl([?AUTHENTICATE,Signature,Extra]) ->
  true = is_valid_dict(Extra),
  {authenticate,Signature,dict_to_erl(Extra)};

to_erl([?GOODBYE,Details,Error]) when is_binary(Error) ->
  to_erl([?GOODBYE,Details,error_to_erl(Error)]);
to_erl([?GOODBYE,Details,Reason]) ->
  true = is_valid_dict(Details),
  {goodbye,dict_to_erl(Details),Reason};

to_erl([?HEARTBEAT,IncomingSeq,OutgoingSeq,_Discard]) ->
  to_erl([?HEARTBEAT,IncomingSeq,OutgoingSeq]);
to_erl([?HEARTBEAT,IncomingSeq,OutgoingSeq]) ->
  {heartbeat,IncomingSeq,OutgoingSeq};

to_erl([?ERROR,RequestType,RequestId,Details,Error]) ->
  to_erl([?ERROR,RequestType,RequestId,Details,Error,undefined,undefined]);
to_erl([?ERROR,RequestType,RequestId,Details,Error,Arguments]) ->
  to_erl([?ERROR,RequestType,RequestId,Details,Error,Arguments,undefined]);

to_erl([?ERROR,RequestType,RequestId,Details,Error,Arguments,ArgumentsKw]) when is_binary(Error) ->
  true = is_valid_id(RequestId),
  true = is_valid_dict(Details),
  true = is_valid_arguments(Arguments),
  true = is_valid_argumentskw(ArgumentsKw),
  ErlType = case RequestType of
              ?SUBSCRIBE -> subscribe;
              ?UNSUBSCRIBE -> unsubscribe;
              ?PUBLISH -> publish;
              ?REGISTER -> register;
              ?UNREGISTER -> unregister;
              ?CALL -> call;
              ?INVOCATION -> invocation
            end,
  {error,ErlType,RequestId,Details,try_error_to_erl(Error),Arguments,ArgumentsKw};

to_erl([?PUBLISH,RequestId,Options,Topic]) ->
  to_erl([?PUBLISH,RequestId,Options,Topic,undefined,undefined]);
to_erl([?PUBLISH,RequestId,Options,Topic,Arguments]) ->
  to_erl([?PUBLISH,RequestId,Options,Topic,Arguments,undefined]);
to_erl([?PUBLISH,RequestId,Options,Topic,Arguments,ArgumentsKw]) ->
  true = is_valid_id(RequestId),
  true = is_valid_dict(Options),
  true = is_valid_uri(Topic),
  true = is_valid_arguments(Arguments),
  true = is_valid_argumentskw(ArgumentsKw),
  {publish,RequestId,dict_to_erl(Options),Topic,Arguments,ArgumentsKw};

to_erl([?PUBLISHED,RequestId,PublicationId]) ->
  true = is_valid_id(RequestId),
  true = is_valid_id(PublicationId),
  {published,RequestId,PublicationId};

to_erl([?SUBSCRIBE,RequestId,Options,Topic]) ->
  true = is_valid_id(RequestId),
  true = is_valid_dict(Options),
  true = is_valid_uri(Topic),
  {subscribe,RequestId,dict_to_erl(Options),Topic};

to_erl([?SUBSCRIBED,RequestId,SubscriptionId]) ->
  true = is_valid_id(RequestId),
  true = is_valid_id(SubscriptionId),
  {subscribed,RequestId,SubscriptionId};

to_erl([?UNSUBSCRIBE,RequestId,SubscriptionId]) ->
  true = is_valid_id(RequestId),
  true = is_valid_id(SubscriptionId),
  {unsubscribe,RequestId,SubscriptionId};

to_erl([?UNSUBSCRIBED,RequestId]) ->
  true = is_valid_id(RequestId),
  {unsubscribed,RequestId};

to_erl([?EVENT,SubscriptionId,PublicationId,Details]) ->
  to_erl([?EVENT,SubscriptionId,PublicationId,Details,undefined,undefined]);
to_erl([?EVENT,SubscriptionId,PublicationId,Details,Arguments]) ->
  to_erl([?EVENT,SubscriptionId,PublicationId,Details,Arguments,undefined]);
to_erl([?EVENT,SubscriptionId,PublicationId,Details,Arguments,ArgumentsKw]) ->
  true = is_valid_id(SubscriptionId),
  true = is_valid_id(PublicationId),
  true = is_valid_dict(Details),
  true = is_valid_arguments(Arguments),
  true = is_valid_argumentskw(ArgumentsKw),
  {event,SubscriptionId,PublicationId,dict_to_erl(Details),Arguments,ArgumentsKw};

to_erl([?CALL,RequestId,Options,Procedure]) ->
  to_erl([?CALL,RequestId,Options,Procedure,undefined,undefined]);
to_erl([?CALL,RequestId,Options,Procedure,Arguments]) ->
  to_erl([?CALL,RequestId,Options,Procedure,Arguments,undefined]);
to_erl([?CALL,RequestId,Options,Procedure,Arguments,ArgumentsKw]) ->
  true = is_valid_id(RequestId),
  true = is_valid_dict(Options),
  true = is_valid_uri(Procedure),
  true = is_valid_arguments(Arguments),
  true = is_valid_argumentskw(ArgumentsKw),
  {call,RequestId,dict_to_erl(Options),Procedure,Arguments,ArgumentsKw};

to_erl([?CANCEL,RequestId,Options]) ->
  true = is_valid_dict(Options),
  {cancel,RequestId,dict_to_erl(Options)};

to_erl([?RESULT,RequestId,Details]) ->
  to_erl([?RESULT,RequestId,Details,undefined,undefined]);
to_erl([?RESULT,RequestId,Details,Arguments]) ->
  to_erl([?RESULT,RequestId,Details,Arguments,undefined]);
to_erl([?RESULT,RequestId,Details,Arguments,ArgumentsKw]) ->
  true = is_valid_id(RequestId),
  true = is_valid_dict(Details),
  true = is_valid_arguments(Arguments),
  true = is_valid_argumentskw(ArgumentsKw),
  {result,RequestId,dict_to_erl(Details),Arguments,ArgumentsKw};

to_erl([?REGISTER,RequestId,Options,Procedure]) ->
  true = is_valid_id(RequestId),
  true = is_valid_dict(Options),
  true = is_valid_uri(Procedure),
  {register,RequestId,dict_to_erl(Options),Procedure};

to_erl([?REGISTERED,RequestId,RegistrationId]) ->
  true = is_valid_id(RequestId),
  true = is_valid_id(RegistrationId),
  {registered,RequestId,RegistrationId};

to_erl([?UNREGISTER,RequestId,RegistrationId]) ->
  {unregister,RequestId,RegistrationId};

to_erl([?UNREGISTERED,RequestId]) ->
  true = is_valid_id(RequestId),
  {unregistered,RequestId};

to_erl([?INVOCATION,RequestId, RegistrationId, Details]) ->
  to_erl([?INVOCATION,RequestId, RegistrationId, Details, undefined, undefined]);
to_erl([?INVOCATION,RequestId, RegistrationId, Details, Arguments]) ->
  to_erl([?INVOCATION,RequestId, RegistrationId, Details, Arguments, undefined]);
to_erl([?INVOCATION,RequestId, RegistrationId, Details, Arguments, ArgumentsKw]) ->
  true = is_valid_id(RequestId),
  true = is_valid_id(RegistrationId),
  true = is_valid_dict(Details),
  true = is_valid_arguments(Arguments),
  true = is_valid_argumentskw(ArgumentsKw),
  {invocation,RequestId, RegistrationId, dict_to_erl(Details), Arguments, ArgumentsKw};

to_erl([?INTERRUPT,RequestId,Options]) ->
  true = is_valid_dict(Options),
  {interrupt,RequestId,dict_to_erl(Options)};

to_erl([?YIELD, RequestId, Options]) ->
  to_erl([?YIELD, RequestId, Options, undefined, undefined]);
to_erl([?YIELD, RequestId, Options, Arguments]) ->
  to_erl([?YIELD, RequestId, Options, Arguments, undefined]);
to_erl([?YIELD, RequestId, Options, Arguments, ArgumentsKw]) ->
  true = is_valid_id(RequestId),
  true = is_valid_dict(Options),
  true = is_valid_arguments(Arguments),
  true = is_valid_argumentskw(ArgumentsKw),
  {yield,RequestId,dict_to_erl(Options), Arguments,ArgumentsKw}.






to_wamp({hello,Realm,Details}) ->
  [?HELLO,Realm,hello_dict_to_wamp(Details)];

to_wamp({challenge,wampcra,Extra}) ->
   to_wamp({challenge,<<"wampcra">>,Extra});
to_wamp({challenge,AuthMethod,Extra}) ->
  [?CHALLENGE,AuthMethod,dict_to_wamp(Extra)];

to_wamp({authenticate,Signature,Extra}) ->
  [?AUTHENTICATE,Signature,dict_to_wamp(Extra)];

to_wamp({welcome,SessionId,Details}) ->
  [?WELCOME,SessionId,dict_to_wamp(Details)];

to_wamp({heartbeat,IncomingSeq,OutgoingSeq}) ->
  [?HEARTBEAT,IncomingSeq,OutgoingSeq];

to_wamp({abort,Details,Error}) when is_atom(Error) ->
  to_wamp({abort,Details,error_to_wamp(Error)});
to_wamp({abort,Details,Reason}) ->
  [?ABORT,dict_to_wamp(Details),Reason];


to_wamp({goodbye,Details,Error}) when is_atom(Error)->
  to_wamp({goodbye,Details,error_to_wamp(Error)});

to_wamp({goodbye,Details,Reason}) ->
  [?GOODBYE,dict_to_wamp(Details),Reason];


to_wamp({error,Origin,RequestId,Details,Error,Arguments,ArgumentsKw}) when is_atom(Error) ->
  to_wamp({error,Origin,RequestId,Details,error_to_wamp(Error),Arguments,ArgumentsKw});

to_wamp({error,subscribe,RequestId,Details,Error,Arguments,ArgumentsKw}) ->
  to_wamp({error,?SUBSCRIBE,RequestId,Details,Error,Arguments,ArgumentsKw});
to_wamp({error,unsubscribe,RequestId,Details,Error,Arguments,ArgumentsKw}) ->
  to_wamp({error,?UNSUBSCRIBE,RequestId,Details,Error,Arguments,ArgumentsKw});
to_wamp({error,publish,RequestId,Details,Error,Arguments,ArgumentsKw}) ->
  to_wamp({error,?PUBLISH,RequestId,Details,Error,Arguments,ArgumentsKw});
to_wamp({error,register,RequestId,Details,Error,Arguments,ArgumentsKw}) ->
  to_wamp({error,?REGISTER,RequestId,Details,Error,Arguments,ArgumentsKw});
to_wamp({error,unregister,RequestId,Details,Error,Arguments,ArgumentsKw}) ->
  to_wamp({error,?UNREGISTER,RequestId,Details,Error,Arguments,ArgumentsKw});
to_wamp({error,call,RequestId,Details,Error,Arguments,ArgumentsKw}) ->
  to_wamp({error,?CALL,RequestId,Details,Error,Arguments,ArgumentsKw});
to_wamp({error,invocation,RequestId,Details,Error,Arguments,ArgumentsKw}) ->
  to_wamp({error,?INVOCATION,RequestId,Details,Error,Arguments,ArgumentsKw});


to_wamp({error,Origin,RequestId,Details,Reason,undefined,undefined}) ->
  [?ERROR,Origin,RequestId,dict_to_wamp(Details),Reason];
to_wamp({error,Origin,RequestId,Details,Reason,Arguments,undefined}) ->
  [?ERROR,Origin,RequestId,dict_to_wamp(Details),Reason,Arguments];
to_wamp({error,Origin,RequestId,Details,Reason,Arguments,ArgumentsKw}) ->
  [?ERROR,Origin,RequestId,dict_to_wamp(Details),Reason,Arguments,ArgumentsKw];

to_wamp({publish,RequestId,Options,Topic,undefined,undefined}) ->
  [?PUBLISH,RequestId,dict_to_wamp(Options),Topic];
to_wamp({publish,RequestId,Options,Topic,Arguments,undefined}) ->
  [?PUBLISH,RequestId,dict_to_wamp(Options),Topic,Arguments];
to_wamp({publish,RequestId,Options,Topic,Arguments,ArgumentsKw}) ->
  [?PUBLISH,RequestId,dict_to_wamp(Options),Topic,Arguments,ArgumentsKw];

to_wamp({published,RequestId,PublicationId}) ->
  [?PUBLISHED,RequestId,PublicationId];

to_wamp({subscribe,RequestId,Options,Topic}) ->
  [?SUBSCRIBE,RequestId,dict_to_wamp(Options),Topic];

to_wamp({subscribed,RequestId,SubscriptionId}) ->
  [?SUBSCRIBED,RequestId,SubscriptionId];

to_wamp({unsubscribe,RequestId,SubscriptionId}) ->
  [?UNSUBSCRIBE,RequestId,SubscriptionId];

to_wamp({unsubscribed,RequestId}) ->
  [?UNSUBSCRIBED,RequestId];

to_wamp({event,SubscriptionId,PublicationId,Details,undefined,undefined}) ->
  [?EVENT,SubscriptionId,PublicationId,dict_to_wamp(Details)];
to_wamp({event,SubscriptionId,PublicationId,Details,Arguments,undefined}) ->
  [?EVENT,SubscriptionId,PublicationId,dict_to_wamp(Details),Arguments];
to_wamp({event,SubscriptionId,PublicationId,Details,Arguments,ArgumentsKw}) ->
  [?EVENT,SubscriptionId,PublicationId,dict_to_wamp(Details),Arguments,ArgumentsKw];

to_wamp({call,RequestId,Options,Procedure,undefined,undefined}) ->
  [?CALL,RequestId,dict_to_wamp(Options),Procedure];
to_wamp({call,RequestId,Options,Procedure,Arguments,undefined}) ->
  [?CALL,RequestId,dict_to_wamp(Options),Procedure,Arguments];
to_wamp({call,RequestId,Options,Procedure,Arguments,ArgumentsKw}) ->
  [?CALL,RequestId,dict_to_wamp(Options),Procedure,Arguments,ArgumentsKw];

to_wamp({cancel,RequestId,Options}) ->
  [?CANCEL,RequestId,dict_to_wamp(Options)];

to_wamp({result,RequestId,Details,undefined,undefined}) ->
  [?RESULT,RequestId,dict_to_wamp(Details)];
to_wamp({result,RequestId,Details,Arguments,undefined}) ->
  [?RESULT,RequestId,dict_to_wamp(Details),Arguments];
to_wamp({result,RequestId,Details,Arguments,ArgumentsKw}) ->
  [?RESULT,RequestId,dict_to_wamp(Details),Arguments,ArgumentsKw];

to_wamp({register,RequestId,Options,Procedure}) ->
  [?REGISTER,RequestId,dict_to_wamp(Options),Procedure];

to_wamp({registered,RequestId,RegistrationId}) ->
  [?REGISTERED,RequestId,RegistrationId];

to_wamp({unregister,RequestId,RegistrationId}) ->
  [?UNREGISTER,RequestId,RegistrationId];

to_wamp({unregistered,RequestId}) ->
  [?UNREGISTERED,RequestId];

to_wamp({invocation,RequestId,RegistrationId,Details,undefined,undefined}) ->
  [?INVOCATION,RequestId,RegistrationId,dict_to_wamp(Details)];
to_wamp({invocation,RequestId,RegistrationId,Details,Arguments,undefined}) ->
  [?INVOCATION,RequestId,RegistrationId,dict_to_wamp(Details),Arguments];
to_wamp({invocation,RequestId,RegistrationId,Details,Arguments,ArgumentsKw}) ->
  [?INVOCATION,RequestId,RegistrationId,dict_to_wamp(Details),Arguments,ArgumentsKw];

to_wamp({interrupt,RequestId,Options}) ->
  [?INTERRUPT,RequestId,dict_to_wamp(Options)];

to_wamp({yield,RequestId,Options,undefined,undefined}) ->
  [?YIELD,RequestId,dict_to_wamp(Options)];
to_wamp({yield,RequestId,Options,Arguments,undefined}) ->
  [?YIELD,RequestId,dict_to_wamp(Options),Arguments];
to_wamp({yield,RequestId,Options,Arguments,ArgumentsKw}) ->
  [?YIELD,RequestId,dict_to_wamp(Options),Arguments,ArgumentsKw];

to_wamp(noreply)  ->
  noreply;
to_wamp(shutdown)  ->
  shutdown.


try_error_to_erl(Error) ->
  case error_to_erl(Error) of
    {unknown_error,Error} ->
      Error;
    ErlError ->
      ErlError
  end.

error_to_erl(Error) ->
  convert_error(to_erl,Error).

error_to_wamp(Error) ->
  convert_error(to_wamp,Error).

-define(ERROR_MAPPING,[
                       {},
                       {goodbye_and_out,<<"wamp.error.goodbye_and_out">>},
                       {authorization_failed,<<"wamp.error.authorization_failed">>},
                       {canceled,<<"wamp.error.canceled">>},
                       {close_realm,<<"wamp.error.close_realm">>},
                       {disclose_not_allowed,<<"wamp.error.disclose_me.not_allowed">>},
                       {invalid_argument,<<"wamp.error.invalid_argument">>},
                       {invalid_uri,<<"wamp.error.invalid_uri">>},
                       {no_such_procedure,<<"wamp.error.no_such_procedure">>},
                       {no_such_realm,<<"wamp.error.no_such_realm">>},
                       {no_such_registration,<<"wamp.error.no_such_registration">>},
                       {no_such_role,<<"wamp.error.no_such_role">>},
                       {no_such_subscription,<<"wamp.error.no_such_subscription">>},
                       {not_authorized,<<"wamp.error.not_authorized">>},
                       {procedure_already_exists,<<"wamp.error.procedure_already_exists">>},
                       {system_shutdown,<<"wamp.error.system_shutdown">>}
                       ]).


convert_error(Direction,Error) ->
  KeyPos =
    case Direction of
      to_erl -> 2;
      to_wamp -> 1
    end,
  {ErlError,WampError} =
    case lists:keyfind(Error,KeyPos,?ERROR_MAPPING) of
      {EE,WE} -> {EE,WE};
      false -> {Error,Error}
    end,
  case Direction of
    to_erl ->
      case is_atom(ErlError) of
        true ->
          ErlError;
        false ->
          {unknown_error,WampError}
      end;
    to_wamp ->
      case is_atom(WampError) of
        true ->
          <<"wamp.error.internal">>;
        false ->
          WampError
      end
  end.








dict_to_erl(Dict) ->
  convert_dict(default,Dict,to_erl).

dict_to_wamp(Dict) ->
  convert_dict(default,Dict,to_wamp).

hello_dict_to_erl(Dict) ->
  convert_dict(hello,Dict,to_erl).

hello_dict_to_wamp(Dict) ->
  convert_dict(hello,Dict,to_wamp).

-define(HELLO_MAPPING, [
                      {agent,<<"agent">>,false},
                      {anonymous,<<"anonymous">>,false},
                      {authid,<<"authid">>,false},
                      {authmethod,<<"authmethod">>,false},
                      {authmethods,<<"authmethods">>,list},
                      {authprovider,<<"authprovider">>,false},
                      {authrole,<<"authrole">>,false},
                      {broker,<<"broker">>,dict},
                      {call_canceling,<<"call_canceling">>,false},
                      {call_timeout,<<"call_timeout">>,false},
                      {call_trustlevels,<<"call_trustlevels">>,false},
                      {callee,<<"callee">>,dict},
                      {callee_blackwhite_listing,<<"callee_blackwhite_listing">>,false},
                      {caller,<<"caller">>,dict},
                      {caller_exclusion,<<"caller_exclusion">>,false},
                      {caller_identification,<<"caller_identification">>,false},
                      {challenge,<<"challenge">>,false},
                      {dealer,<<"dealer">>,dict},
                      {disclose_me,<<"disclose_me">>,false},
                      {eligible,<<"eligible">>,false},
                      {event_history,<<"event_history">>,false},
                      {exclude,<<"exclude">>,false},
                      {exclude_me,<<"exclude_me">>,false},
                      {features,<<"features">>,dict},
                      {iterations,<<"iterations">>,false},
                      {keylen,<<"keylen">>,false},
                      {match,<<"match">>,value},
                      {partitioned_pubsub,<<"partitioned_pubsub">>,false},
                      {partitioned_rpc,<<"partitioned_rpc">>,false},
                      {pattern_based_registration,<<"pattern_based_registration">>,false},
                      {pattern_based_subscription,<<"pattern_based_subscription">>,false},
                      {prefix,<<"prefix">>,false},
                      {progress,<<"progress">>,false},
                      {progressive_call_results,<<"progressive_call_results">>,false},
                      {publication_trustlevels,<<"publication_trustlevels">>,false},
                      {publisher,<<"publisher">>,dict},
                      {publisher_exclusion,<<"publisher_exclusion">>,false},
                      {publisher_identification,<<"publisher_identification">>,false},
                      {receive_progress,<<"receive_progress">>,false},
                      {roles,<<"roles">>,dict},
                      {salt,<<"salt">>,false},
                      {subscriber,<<"subscriber">>,dict},
                      {subscriber_blackwhite_listing,<<"subscriber_blackwhite_listing">>,false},
                      {subscriber_list,<<"subscriber_list">>,false},
                      {subscriber_metaevents,<<"subscriber_metaevents">>,false},
                      {timeout,<<"timeout">>,false},
                      {wampcra,<<"wampcra">>,false},
                      {wildcard,<<"wildcard">>,false}
                      ]).

-define(DICT_MAPPING,[
                      {agent,<<"agent">>,false},
                      {anonymous,<<"anonymous">>,false},
                      {authid,<<"authid">>,false},
                      {authmethod,<<"authmethod">>,false},
                      {authmethods,<<"authmethods">>,list},
                      {authprovider,<<"authprovider">>,false},
                      {authrole,<<"authrole">>,false},
                      {broker,<<"broker">>,dict},
                      {call_canceling,<<"call_canceling">>,false},
                      {call_timeout,<<"call_timeout">>,false},
                      {call_trustlevels,<<"call_trustlevels">>,false},
                      {callee,<<"callee">>,false},
                      {callee_blackwhite_listing,<<"callee_blackwhite_listing">>,false},
                      {caller,<<"caller">>,false},
                      {caller_exclusion,<<"caller_exclusion">>,false},
                      {caller_identification,<<"caller_identification">>,false},
                      {challenge,<<"challenge">>,false},
                      {dealer,<<"dealer">>,dict},
                      {disclose_me,<<"disclose_me">>,false},
                      {eligible,<<"eligible">>,false},
                      {event_history,<<"event_history">>,false},
                      {exclude,<<"exclude">>,false},
                      {exclude_me,<<"exclude_me">>,false},
                      {features,<<"features">>,dict},
                      {iterations,<<"iterations">>,false},
                      {keylen,<<"keylen">>,false},
                      {match,<<"match">>,value},
                      {partitioned_pubsub,<<"partitioned_pubsub">>,false},
                      {partitioned_rpc,<<"partitioned_rpc">>,false},
                      {pattern_based_registration,<<"pattern_based_registration">>,false},
                      {pattern_based_subscription,<<"pattern_based_subscription">>,false},
                      {prefix,<<"prefix">>,false},
                      {progress,<<"progress">>,false},
                      {progressive_call_results,<<"progressive_call_results">>,false},
                      {publication_trustlevels,<<"publication_trustlevels">>,false},
                      {publisher,<<"publisher">>,false},
                      {publisher_exclusion,<<"publisher_exclusion">>,false},
                      {publisher_identification,<<"publisher_identification">>,false},
                      {receive_progress,<<"receive_progress">>,false},
                      {roles,<<"roles">>,dict},
                      {salt,<<"salt">>,false},
                      {subscriber,<<"subscriber">>,dict},
                      {subscriber_blackwhite_listing,<<"subscriber_blackwhite_listing">>,false},
                      {subscriber_list,<<"subscriber_list">>,false},
                      {subscriber_metaevents,<<"subscriber_metaevents">>,false},
                      {timeout,<<"timeout">>,false},
                      {wampcra,<<"wampcra">>,false},
                      {wildcard,<<"wildcard">>,false}
                      ]).

convert_dict(Type,PropList,Direction) when is_list(PropList) ->
  warning("the use of proplists is deprecated~nProplist: ~p~n",[PropList]),
  Map = maps:from_list(PropList),
  convert_dict(Type,Map,Direction);
convert_dict(Type,Map,Direction)  ->
  Mapping = case Type of
              hello -> ?HELLO_MAPPING;
              roles -> ?HELLO_MAPPING;
              _ -> ?DICT_MAPPING
            end,
  Folding = fun(Key, Value, InMap) ->
              {ConvKey,ConvValue} = convert_key_value(Direction,Key,Value,Mapping),
              maps:put(ConvKey,ConvValue,InMap)
            end,
  maps:fold(Folding,#{},Map).


convert_key_value(Direction,Key,Value,Mapping) ->
  KeyPos =
    case Direction of
      to_erl -> 2;
      to_wamp -> 1
    end,

  {ErlKey,WampKey,Deep} =
    case lists:keyfind(Key,KeyPos,Mapping) of
      {Ek,Wk,D} -> {Ek,Wk,D};
      false -> {Key,Key,false}
    end,
  ConvValue =
    case Deep of
      dict -> convert_dict(ErlKey,Value,Direction);
      list -> convert_list(Direction,Value,[],Mapping);
      value -> convert_value(Direction,Value,Mapping);
      _ -> Value
    end,
  ConvKey =
    case Direction of
      to_erl -> ErlKey;
      to_wamp -> WampKey
    end,
  {ConvKey,ConvValue}.


convert_list(_,[],[],_) ->
  [];
convert_list(_,[],Converted,_) ->
  lists:reverse(Converted);
convert_list(Direction,[Key|T],Converted,Mapping) ->
  KeyPos =
    case Direction of
      to_erl -> 2;
      to_wamp -> 1
    end,
  {ErlKey,WampKey} =
    case lists:keyfind(Key,KeyPos,Mapping) of
      {Ek,Wk,_} -> {Ek,Wk};
      false -> {Key,Key}
    end,
  ConvKey =
    case Direction of
      to_erl -> ErlKey;
      to_wamp -> WampKey
    end,
  convert_list(Direction,T,[ConvKey|Converted],Mapping).



convert_value(Direction,Value,Mapping) ->
  ValPos =
    case Direction of
      to_erl -> 2;
      to_wamp -> 1
    end,
  {ErlVal,WampVal} =
    case lists:keyfind(Value,ValPos,Mapping) of
      {EV,WV,_} -> {EV,WV};
      false -> {Value,Value}
    end,
  case Direction of
      to_erl -> ErlVal;
      to_wamp -> WampVal
  end.


-ifndef(TEST).

warning(Format,Data) ->
  io:format(Format,Data).

-else.

warning(Format,Data) ->
  ct:log(Format,Data),
  ct:print(Format,Data).


validation_test() ->
  true = is_valid_id(0),
  true = is_valid_id(9007199254740991),
  false = is_valid_id(9007199254740992),
  false = is_valid_id(-1),
  false = is_valid_id(0.1),

  true = is_valid_uri(<<"wamp.ws">>),
  false = is_valid_dict([]),
  true = is_valid_dict(#{}),

  true = is_valid_arguments([]),
  false = is_valid_argumentskw([]),
  true = is_valid_argumentskw(#{}),
  ok.



hello_json_test() ->
  M = [?HELLO,<<"realm1">>,#{}],
  S = serialize(M,json),
  D = deserialize(S,json),
  D = {[{hello,<<"realm1">>,#{}}],<<"">>}.

hello_json_batched_test() ->
  M = [?HELLO,<<"realm1">>,#{}],
  S = serialize(M,json_batched),
  D = deserialize(S,json_batched),
  D = {[{hello,<<"realm1">>,#{}}],<<"">>}.

hello_msgpack_test() ->
  M = [?HELLO,<<"realm1">>,#{}],
  S = serialize(M,msgpack),
  D = deserialize(S,msgpack),
  D = {[{hello,<<"realm1">>,#{}}],<<"">>}.

hello_msgpack_batched_test() ->
  M = [?HELLO,<<"realm1">>,#{}],
  S = serialize(M,msgpack_batched),
  D = deserialize(S,msgpack_batched),
  D = {[{hello,<<"realm1">>,#{}}],<<"">>}.


roundtrip_test() ->
  Messages = [
              {hello,<<"realm1">>,#{roles => #{callee => #{features => #{}}, caller => #{ features => #{}} } }},
              {welcome,398475,#{}},
              {abort,#{},invalid_argument},
              {goodbye,#{},goodbye_and_out},
              {publish,2343,#{exclude_me=>false},<<"just_some_uri">>,undefined,undefined},
              {error,publish,9873,#{},invalid_argument,undefined,undefined},
              {published,209384,092834},
              {subscribe,9834,#{},<<"event_i_need">>},
              {error,subscribe,9873,#{},invalid_argument,undefined,undefined},
              {subscribed,9283,9898},
              {unsubscribe,2333,23400},
              {error,unsubscribe,9873,#{},invalid_argument,undefined,undefined},
              {unsubscribed,28777},
              {event,28882,292839,#{publisher => 3980999},[<<"some information">>],undefined},
              {call,298374,#{disclose_me=>true},<<"some_rpc">>,[3,true,<<"hello world!">>],#{<<"hello">> => <<"world">>}},
              {error,call,982734,#{},invalid_uri,undefined,undefined},
              {result,98273,#{},undefined,undefined},
              {register,20938,#{},<<"some_rpc">>},
              {error,register,9873,#{},invalid_argument,undefined,undefined},
              {registered,9827988,234},
              {unregister,209384,20938999},
              {error,unregister,9873,#{},invalid_argument,undefined,undefined},
              {unregistered,9283000},
              {invocation,328,23444,#{},[<<"Willi">>],undefined},
              {error,invocation,9873,#{},invalid_argument,undefined,undefined},
              {yield,2987,#{},undefined,undefined}


              ],
  Serializer = fun(Message,Res) ->
                 Encodings = [json,msgpack,erlbin,raw_json,raw_msgpack,raw_erlbin,json_batched,msgpack_batched],
                 ct:log("~p",[Message]),
                 Check = fun(Enc,Bool) ->
                           ct:log("   ~p: ",[Enc]),
                           EncMsg = serialize(Message,Enc),
                           ct:log("   <- ~p",[EncMsg]),
                           DeEncMsg = deserialize(EncMsg,Enc) ,
                           case DeEncMsg of
                             {[Message],<<"">>} ->
                               ct:log("   -> ~p, okay~n",[DeEncMsg]),
                               Bool;
                             _ ->
                               ct:log("   -> ~p *** ERROR ***",[DeEncMsg]),
                               false
                           end
                         end,
                 Res and lists:foldl(Check,true,Encodings)
               end,
  true = lists:foldl(Serializer,true,Messages).


msgpack_error_test() ->
  M = [?HELLO,<<"realm1">>,[{<<"option_with">>,atom}]],
  try serialize(M,msgpack)
    catch _:_ ->
      ok
  end.


-endif.

