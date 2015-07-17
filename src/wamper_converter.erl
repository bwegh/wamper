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
%%% Created : 16. Jul 2015 13:56
%%%-------------------------------------------------------------------
-module(wamper_converter).
-author("tihon").

-include("wamper_header_mapping.hrl").
-include("wamper_message_codes.hrl").

-ifndef(TEST).
-define(LOG(F, A),
  ct:log(F, A),
  ct:print(F, A)).
-else.
-define(LOG(F, A), io:format(F, A)).
-endif.

%% API
-export([to_wamp/1, to_erl/1]).

to_wamp({hello, Realm, Details}) ->
  [?HELLO, Realm, hello_dict_to_wamp(Details)];
to_wamp({challenge, wampcra, Extra}) ->
  to_wamp({challenge, <<"wampcra">>, Extra});
to_wamp({challenge, AuthMethod, Extra}) ->
  [?CHALLENGE, AuthMethod, dict_to_wamp(Extra)];
to_wamp({authenticate, Signature, Extra}) ->
  [?AUTHENTICATE, Signature, dict_to_wamp(Extra)];
to_wamp({welcome, SessionId, Details}) ->
  [?WELCOME, SessionId, dict_to_wamp(Details)];
to_wamp({heartbeat, IncomingSeq, OutgoingSeq}) ->
  [?HEARTBEAT, IncomingSeq, OutgoingSeq];
to_wamp({abort, Details, Error}) when is_atom(Error) ->
  to_wamp({abort, Details, error_to_wamp(Error)});
to_wamp({abort, Details, Reason}) ->
  [?ABORT, dict_to_wamp(Details), Reason];
to_wamp({goodbye, Details, Error}) when is_atom(Error) ->
  to_wamp({goodbye, Details, error_to_wamp(Error)});
to_wamp({goodbye, Details, Reason}) ->
  [?GOODBYE, dict_to_wamp(Details), Reason];
to_wamp({error, Origin, RequestId, Details, Error, Arguments, ArgumentsKw}) when is_atom(Error) ->
  to_wamp({error, Origin, RequestId, Details, error_to_wamp(Error), Arguments, ArgumentsKw});
to_wamp({error, subscribe, RequestId, Details, Error, Arguments, ArgumentsKw}) ->
  to_wamp({error, ?SUBSCRIBE, RequestId, Details, Error, Arguments, ArgumentsKw});
to_wamp({error, unsubscribe, RequestId, Details, Error, Arguments, ArgumentsKw}) ->
  to_wamp({error, ?UNSUBSCRIBE, RequestId, Details, Error, Arguments, ArgumentsKw});
to_wamp({error, publish, RequestId, Details, Error, Arguments, ArgumentsKw}) ->
  to_wamp({error, ?PUBLISH, RequestId, Details, Error, Arguments, ArgumentsKw});
to_wamp({error, register, RequestId, Details, Error, Arguments, ArgumentsKw}) ->
  to_wamp({error, ?REGISTER, RequestId, Details, Error, Arguments, ArgumentsKw});
to_wamp({error, unregister, RequestId, Details, Error, Arguments, ArgumentsKw}) ->
  to_wamp({error, ?UNREGISTER, RequestId, Details, Error, Arguments, ArgumentsKw});
to_wamp({error, call, RequestId, Details, Error, Arguments, ArgumentsKw}) ->
  to_wamp({error, ?CALL, RequestId, Details, Error, Arguments, ArgumentsKw});
to_wamp({error, invocation, RequestId, Details, Error, Arguments, ArgumentsKw}) ->
  to_wamp({error, ?INVOCATION, RequestId, Details, Error, Arguments, ArgumentsKw});
to_wamp({error, Origin, RequestId, Details, Reason, undefined, undefined}) ->
  [?ERROR, Origin, RequestId, dict_to_wamp(Details), Reason];
to_wamp({error, Origin, RequestId, Details, Reason, Arguments, undefined}) ->
  [?ERROR, Origin, RequestId, dict_to_wamp(Details), Reason, Arguments];
to_wamp({error, Origin, RequestId, Details, Reason, Arguments, ArgumentsKw}) ->
  [?ERROR, Origin, RequestId, dict_to_wamp(Details), Reason, Arguments, ArgumentsKw];
to_wamp({publish, RequestId, Options, Topic, undefined, undefined}) ->
  [?PUBLISH, RequestId, dict_to_wamp(Options), Topic];
to_wamp({publish, RequestId, Options, Topic, Arguments, undefined}) ->
  [?PUBLISH, RequestId, dict_to_wamp(Options), Topic, Arguments];
to_wamp({publish, RequestId, Options, Topic, Arguments, ArgumentsKw}) ->
  [?PUBLISH, RequestId, dict_to_wamp(Options), Topic, Arguments, ArgumentsKw];
to_wamp({published, RequestId, PublicationId}) ->
  [?PUBLISHED, RequestId, PublicationId];
to_wamp({subscribe, RequestId, Options, Topic}) ->
  [?SUBSCRIBE, RequestId, dict_to_wamp(Options), Topic];
to_wamp({subscribed, RequestId, SubscriptionId}) ->
  [?SUBSCRIBED, RequestId, SubscriptionId];
to_wamp({unsubscribe, RequestId, SubscriptionId}) ->
  [?UNSUBSCRIBE, RequestId, SubscriptionId];
to_wamp({unsubscribed, RequestId}) ->
  [?UNSUBSCRIBED, RequestId];
to_wamp({event, SubscriptionId, PublicationId, Details, undefined, undefined}) ->
  [?EVENT, SubscriptionId, PublicationId, dict_to_wamp(Details)];
to_wamp({event, SubscriptionId, PublicationId, Details, Arguments, undefined}) ->
  [?EVENT, SubscriptionId, PublicationId, dict_to_wamp(Details), Arguments];
to_wamp({event, SubscriptionId, PublicationId, Details, Arguments, ArgumentsKw}) ->
  [?EVENT, SubscriptionId, PublicationId, dict_to_wamp(Details), Arguments, ArgumentsKw];
to_wamp({call, RequestId, Options, Procedure, undefined, undefined}) ->
  [?CALL, RequestId, dict_to_wamp(Options), Procedure];
to_wamp({call, RequestId, Options, Procedure, Arguments, undefined}) ->
  [?CALL, RequestId, dict_to_wamp(Options), Procedure, Arguments];
to_wamp({call, RequestId, Options, Procedure, Arguments, ArgumentsKw}) ->
  [?CALL, RequestId, dict_to_wamp(Options), Procedure, Arguments, ArgumentsKw];
to_wamp({cancel, RequestId, Options}) ->
  [?CANCEL, RequestId, dict_to_wamp(Options)];
to_wamp({result, RequestId, Details, undefined, undefined}) ->
  [?RESULT, RequestId, dict_to_wamp(Details)];
to_wamp({result, RequestId, Details, Arguments, undefined}) ->
  [?RESULT, RequestId, dict_to_wamp(Details), Arguments];
to_wamp({result, RequestId, Details, Arguments, ArgumentsKw}) ->
  [?RESULT, RequestId, dict_to_wamp(Details), Arguments, ArgumentsKw];
to_wamp({register, RequestId, Options, Procedure}) ->
  [?REGISTER, RequestId, dict_to_wamp(Options), Procedure];
to_wamp({registered, RequestId, RegistrationId}) ->
  [?REGISTERED, RequestId, RegistrationId];
to_wamp({unregister, RequestId, RegistrationId}) ->
  [?UNREGISTER, RequestId, RegistrationId];
to_wamp({unregistered, RequestId}) ->
  [?UNREGISTERED, RequestId];
to_wamp({invocation, RequestId, RegistrationId, Details, undefined, undefined}) ->
  [?INVOCATION, RequestId, RegistrationId, dict_to_wamp(Details)];
to_wamp({invocation, RequestId, RegistrationId, Details, Arguments, undefined}) ->
  [?INVOCATION, RequestId, RegistrationId, dict_to_wamp(Details), Arguments];
to_wamp({invocation, RequestId, RegistrationId, Details, Arguments, ArgumentsKw}) ->
  [?INVOCATION, RequestId, RegistrationId, dict_to_wamp(Details), Arguments, ArgumentsKw];
to_wamp({interrupt, RequestId, Options}) ->
  [?INTERRUPT, RequestId, dict_to_wamp(Options)];
to_wamp({yield, RequestId, Options, undefined, undefined}) ->
  [?YIELD, RequestId, dict_to_wamp(Options)];
to_wamp({yield, RequestId, Options, Arguments, undefined}) ->
  [?YIELD, RequestId, dict_to_wamp(Options), Arguments];
to_wamp({yield, RequestId, Options, Arguments, ArgumentsKw}) ->
  [?YIELD, RequestId, dict_to_wamp(Options), Arguments, ArgumentsKw];
to_wamp(noreply) ->
  noreply;
to_wamp(shutdown) ->
  shutdown.

to_erl([?HELLO, Realm, Details]) ->
  true = wamper_validator:is_valid_uri(Realm),
  true = wamper_validator:is_valid_dict(Details),
  {hello, Realm, hello_dict_to_erl(Details)};
to_erl([?WELCOME, SessionId, Details]) ->
  true = wamper_validator:is_valid_id(SessionId),
  true = wamper_validator:is_valid_dict(Details),
  {welcome, SessionId, dict_to_erl(Details)};
to_erl([?ABORT, Details, Reason]) ->
  true = wamper_validator:is_valid_dict(Details),
  true = wamper_validator:is_valid_uri(Reason),
  {abort, dict_to_erl(Details), try_error_to_erl(Reason)};
to_erl([?CHALLENGE, <<"wampcra">>, Extra]) ->
  to_erl([?CHALLENGE, wampcra, Extra]);
to_erl([?CHALLENGE, AuthMethod, Extra]) ->
  true = wamper_validator:is_valid_dict(Extra),
  {challenge, AuthMethod, dict_to_erl(Extra)};
to_erl([?AUTHENTICATE, Signature, Extra]) ->
  true = wamper_validator:is_valid_dict(Extra),
  {authenticate, Signature, dict_to_erl(Extra)};
to_erl([?GOODBYE, Details, Error]) when is_binary(Error) ->
  to_erl([?GOODBYE, Details, error_to_erl(Error)]);
to_erl([?GOODBYE, Details, Reason]) ->
  true = wamper_validator:is_valid_dict(Details),
  {goodbye, dict_to_erl(Details), Reason};
to_erl([?HEARTBEAT, IncomingSeq, OutgoingSeq, _Discard]) ->
  to_erl([?HEARTBEAT, IncomingSeq, OutgoingSeq]);
to_erl([?HEARTBEAT, IncomingSeq, OutgoingSeq]) ->
  {heartbeat, IncomingSeq, OutgoingSeq};
to_erl([?ERROR, RequestType, RequestId, Details, Error]) ->
  to_erl([?ERROR, RequestType, RequestId, Details, Error, undefined, undefined]);
to_erl([?ERROR, RequestType, RequestId, Details, Error, Arguments]) ->
  to_erl([?ERROR, RequestType, RequestId, Details, Error, Arguments, undefined]);
to_erl([?ERROR, RequestType, RequestId, Details, Error, Arguments, ArgumentsKw]) when is_binary(Error) ->
  true = wamper_validator:is_valid_id(RequestId),
  true = wamper_validator:is_valid_dict(Details),
  true = wamper_validator:is_valid_arguments(Arguments),
  true = wamper_validator:is_valid_argumentskw(ArgumentsKw),
  ErlType = case RequestType of
              ?SUBSCRIBE -> subscribe;
              ?UNSUBSCRIBE -> unsubscribe;
              ?PUBLISH -> publish;
              ?REGISTER -> register;
              ?UNREGISTER -> unregister;
              ?CALL -> call;
              ?INVOCATION -> invocation
            end,
  {error, ErlType, RequestId, Details, try_error_to_erl(Error), Arguments, ArgumentsKw};
to_erl([?PUBLISH, RequestId, Options, Topic]) ->
  to_erl([?PUBLISH, RequestId, Options, Topic, undefined, undefined]);
to_erl([?PUBLISH, RequestId, Options, Topic, Arguments]) ->
  to_erl([?PUBLISH, RequestId, Options, Topic, Arguments, undefined]);
to_erl([?PUBLISH, RequestId, Options, Topic, Arguments, ArgumentsKw]) ->
  true = wamper_validator:is_valid_id(RequestId),
  true = wamper_validator:is_valid_dict(Options),
  true = wamper_validator:is_valid_uri(Topic),
  true = wamper_validator:is_valid_arguments(Arguments),
  true = wamper_validator:is_valid_argumentskw(ArgumentsKw),
  {publish, RequestId, dict_to_erl(Options), Topic, Arguments, ArgumentsKw};
to_erl([?PUBLISHED, RequestId, PublicationId]) ->
  true = wamper_validator:is_valid_id(RequestId),
  true = wamper_validator:is_valid_id(PublicationId),
  {published, RequestId, PublicationId};
to_erl([?SUBSCRIBE, RequestId, Options, Topic]) ->
  true = wamper_validator:is_valid_id(RequestId),
  true = wamper_validator:is_valid_dict(Options),
  true = wamper_validator:is_valid_uri(Topic),
  {subscribe, RequestId, dict_to_erl(Options), Topic};
to_erl([?SUBSCRIBED, RequestId, SubscriptionId]) ->
  true = wamper_validator:is_valid_id(RequestId),
  true = wamper_validator:is_valid_id(SubscriptionId),
  {subscribed, RequestId, SubscriptionId};
to_erl([?UNSUBSCRIBE, RequestId, SubscriptionId]) ->
  true = wamper_validator:is_valid_id(RequestId),
  true = wamper_validator:is_valid_id(SubscriptionId),
  {unsubscribe, RequestId, SubscriptionId};
to_erl([?UNSUBSCRIBED, RequestId]) ->
  true = wamper_validator:is_valid_id(RequestId),
  {unsubscribed, RequestId};
to_erl([?EVENT, SubscriptionId, PublicationId, Details]) ->
  to_erl([?EVENT, SubscriptionId, PublicationId, Details, undefined, undefined]);
to_erl([?EVENT, SubscriptionId, PublicationId, Details, Arguments]) ->
  to_erl([?EVENT, SubscriptionId, PublicationId, Details, Arguments, undefined]);
to_erl([?EVENT, SubscriptionId, PublicationId, Details, Arguments, ArgumentsKw]) ->
  true = wamper_validator:is_valid_id(SubscriptionId),
  true = wamper_validator:is_valid_id(PublicationId),
  true = wamper_validator:is_valid_dict(Details),
  true = wamper_validator:is_valid_arguments(Arguments),
  true = wamper_validator:is_valid_argumentskw(ArgumentsKw),
  {event, SubscriptionId, PublicationId, dict_to_erl(Details), Arguments, ArgumentsKw};
to_erl([?CALL, RequestId, Options, Procedure]) ->
  to_erl([?CALL, RequestId, Options, Procedure, undefined, undefined]);
to_erl([?CALL, RequestId, Options, Procedure, Arguments]) ->
  to_erl([?CALL, RequestId, Options, Procedure, Arguments, undefined]);
to_erl([?CALL, RequestId, Options, Procedure, Arguments, ArgumentsKw]) ->
  true = wamper_validator:is_valid_id(RequestId),
  true = wamper_validator:is_valid_dict(Options),
  true = wamper_validator:is_valid_uri(Procedure),
  true = wamper_validator:is_valid_arguments(Arguments),
  true = wamper_validator:is_valid_argumentskw(ArgumentsKw),
  {call, RequestId, dict_to_erl(Options), Procedure, Arguments, ArgumentsKw};
to_erl([?CANCEL, RequestId, Options]) ->
  true = wamper_validator:is_valid_dict(Options),
  {cancel, RequestId, dict_to_erl(Options)};
to_erl([?RESULT, RequestId, Details]) ->
  to_erl([?RESULT, RequestId, Details, undefined, undefined]);
to_erl([?RESULT, RequestId, Details, Arguments]) ->
  to_erl([?RESULT, RequestId, Details, Arguments, undefined]);
to_erl([?RESULT, RequestId, Details, Arguments, ArgumentsKw]) ->
  true = wamper_validator:is_valid_id(RequestId),
  true = wamper_validator:is_valid_dict(Details),
  true = wamper_validator:is_valid_arguments(Arguments),
  true = wamper_validator:is_valid_argumentskw(ArgumentsKw),
  {result, RequestId, dict_to_erl(Details), Arguments, ArgumentsKw};
to_erl([?REGISTER, RequestId, Options, Procedure]) ->
  true = wamper_validator:is_valid_id(RequestId),
  true = wamper_validator:is_valid_dict(Options),
  true = wamper_validator:is_valid_uri(Procedure),
  {register, RequestId, dict_to_erl(Options), Procedure};
to_erl([?REGISTERED, RequestId, RegistrationId]) ->
  true = wamper_validator:is_valid_id(RequestId),
  true = wamper_validator:is_valid_id(RegistrationId),
  {registered, RequestId, RegistrationId};
to_erl([?UNREGISTER, RequestId, RegistrationId]) ->
  {unregister, RequestId, RegistrationId};
to_erl([?UNREGISTERED, RequestId]) ->
  true = wamper_validator:is_valid_id(RequestId),
  {unregistered, RequestId};
to_erl([?INVOCATION, RequestId, RegistrationId, Details]) ->
  to_erl([?INVOCATION, RequestId, RegistrationId, Details, undefined, undefined]);
to_erl([?INVOCATION, RequestId, RegistrationId, Details, Arguments]) ->
  to_erl([?INVOCATION, RequestId, RegistrationId, Details, Arguments, undefined]);
to_erl([?INVOCATION, RequestId, RegistrationId, Details, Arguments, ArgumentsKw]) ->
  true = wamper_validator:is_valid_id(RequestId),
  true = wamper_validator:is_valid_id(RegistrationId),
  true = wamper_validator:is_valid_dict(Details),
  true = wamper_validator:is_valid_arguments(Arguments),
  true = wamper_validator:is_valid_argumentskw(ArgumentsKw),
  {invocation, RequestId, RegistrationId, dict_to_erl(Details), Arguments, ArgumentsKw};
to_erl([?INTERRUPT, RequestId, Options]) ->
  true = wamper_validator:is_valid_dict(Options),
  {interrupt, RequestId, dict_to_erl(Options)};
to_erl([?YIELD, RequestId, Options]) ->
  to_erl([?YIELD, RequestId, Options, undefined, undefined]);
to_erl([?YIELD, RequestId, Options, Arguments]) ->
  to_erl([?YIELD, RequestId, Options, Arguments, undefined]);
to_erl([?YIELD, RequestId, Options, Arguments, ArgumentsKw]) ->
  true = wamper_validator:is_valid_id(RequestId),
  true = wamper_validator:is_valid_dict(Options),
  true = wamper_validator:is_valid_arguments(Arguments),
  true = wamper_validator:is_valid_argumentskw(ArgumentsKw),
  {yield, RequestId, dict_to_erl(Options), Arguments, ArgumentsKw}.


%% @private
dict_to_erl(Dict) ->
  convert_dict(default, Dict, to_erl).

%% @private
dict_to_wamp(Dict) ->
  convert_dict(default, Dict, to_wamp).

%% @private
hello_dict_to_erl(Dict) ->
  convert_dict(hello, Dict, to_erl).

%% @private
hello_dict_to_wamp(Dict) ->
  convert_dict(hello, Dict, to_wamp).

%% @private
try_error_to_erl(Error) ->
  case error_to_erl(Error) of
    {unknown_error, Error} ->
      Error;
    ErlError ->
      ErlError
  end.

%% @private
error_to_erl(Error) ->
  convert_error(to_erl, Error).

%% @private
error_to_wamp(Error) ->
  convert_error(to_wamp, Error).

%% @private
convert_error(Direction, Error) ->
  KeyPos =
    case Direction of
      to_erl -> 2;
      to_wamp -> 1
    end,
  {ErlError, WampError} =
    case lists:keyfind(Error, KeyPos, ?ERROR_MAPPING) of
      {EE, WE} -> {EE, WE};
      false -> {Error, Error}
    end,
  case Direction of
    to_erl ->
      case is_atom(ErlError) of
        true ->
          ErlError;
        false ->
          {unknown_error, WampError}
      end;
    to_wamp ->
      case is_atom(WampError) of
        true ->
          <<"wamp.error.internal">>;
        false ->
          WampError
      end
  end.

%% @private
convert_dict(Type, PropList, Direction) when is_list(PropList) ->
  ?LOG("the use of proplists is deprecated~nProplist: ~p~n", [PropList]),
  Map = maps:from_list(PropList),
  convert_dict(Type, Map, Direction);
convert_dict(Type, Map, Direction) ->
  Mapping = case Type of
              hello -> ?HELLO_MAPPING;
              roles -> ?HELLO_MAPPING;
              _ -> ?DICT_MAPPING
            end,
  Folding = fun(Key, Value, InMap) ->
    {ConvKey, ConvValue} = convert_key_value(Direction, Key, Value, Mapping),
    maps:put(ConvKey, ConvValue, InMap)
  end,
  maps:fold(Folding, #{}, Map).

%% @private
convert_key_value(Direction, Key, Value, Mapping) ->
  KeyPos =
    case Direction of
      to_erl -> 2;
      to_wamp -> 1
    end,
  {ErlKey, WampKey, Deep} =
    case lists:keyfind(Key, KeyPos, Mapping) of
      {Ek, Wk, D} -> {Ek, Wk, D};
      false -> {Key, Key, false}
    end,
  ConvValue =
    case Deep of
      dict -> convert_dict(ErlKey, Value, Direction);
      list -> convert_list(Direction, Value, [], Mapping);
      value -> convert_value(Direction, Value, Mapping);
      _ -> Value
    end,
  ConvKey =
    case Direction of
      to_erl -> ErlKey;
      to_wamp -> WampKey
    end,
  {ConvKey, ConvValue}.

%% @private
convert_list(_, [], [], _) -> [];
convert_list(_, [], Converted, _) -> lists:reverse(Converted);
convert_list(Direction, [Key | T], Converted, Mapping) ->
  KeyPos =
    case Direction of
      to_erl -> 2;
      to_wamp -> 1
    end,
  {ErlKey, WampKey} =
    case lists:keyfind(Key, KeyPos, Mapping) of
      {Ek, Wk, _} -> {Ek, Wk};
      false -> {Key, Key}
    end,
  ConvKey =
    case Direction of
      to_erl -> ErlKey;
      to_wamp -> WampKey
    end,
  convert_list(Direction, T, [ConvKey | Converted], Mapping).

%% @private
convert_value(Direction, Value, Mapping) ->
  ValPos =
    case Direction of
      to_erl -> 2;
      to_wamp -> 1
    end,
  {ErlVal, WampVal} =
    case lists:keyfind(Value, ValPos, Mapping) of
      {EV, WV, _} -> {EV, WV};
      false -> {Value, Value}
    end,
  case Direction of
    to_erl -> ErlVal;
    to_wamp -> WampVal
  end.