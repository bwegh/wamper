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
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(wamper_validator).

%% API
-export([
  is_valid_uri/1,
  is_valid_dict/1,
  is_valid_id/1,
  is_valid_arguments/1,
  is_valid_argumentskw/1,
  is_valid_message/1]).

-spec is_valid_message(term()) -> true | false.
is_valid_message({hello, Realm, Details}) ->
  true = is_valid_uri(Realm),
  true = is_valid_dict(Details),
  true;
is_valid_message({welcome, SessionId, Details}) ->
  true = is_valid_id(SessionId),
  true = is_valid_dict(Details),
  true;
is_valid_message({abort, Details, Reason}) ->
  true = is_valid_dict(Details),
  true = is_valid_uri(Reason, abort),
  true;
is_valid_message({challenge, AuthMethod, Extra}) ->
  true = is_binary(AuthMethod) or is_atom(AuthMethod),
  true = is_valid_dict(Extra),
  true;
is_valid_message({authenticate, Signature, Extra}) ->
  true = is_binary(Signature),
  true = is_valid_dict(Extra),
  true;
is_valid_message({goodbye, Details, Reason}) ->
  true = is_valid_uri(Reason, goodbye),
  true = is_valid_dict(Details),
  true;
is_valid_message({heartbeat, IncomingSeq, OutgoingSeq}) ->
  true = is_integer(IncomingSeq),
  true = is_integer(OutgoingSeq),
  true;
is_valid_message({error, MsgType, RequestId, Details, Error}) ->
  is_valid_message({error, MsgType, RequestId, Details, Error, undefined, undefined});
is_valid_message({error, MsgType, RequestId, Details, Error, Arguments}) ->
  is_valid_message({error, MsgType, RequestId, Details, Error, Arguments, undefined});
is_valid_message({error, MsgType, RequestId, Details, Error, Arguments, ArgumentsKw}) ->
  true = case MsgType of
           subscribe -> true;
           unsubscribe -> true;
           publish -> true;
           register -> true;
           unregister -> true;
           call -> true;
           invocation -> true;
           _ -> false
         end,
  true = is_valid_id(RequestId),
  true = is_valid_dict(Details),
  true = is_valid_uri(Error, error),
  true = are_valid_arguments(Arguments, ArgumentsKw),
  true;
is_valid_message({publish, RequestId, Options, Topic}) ->
  is_valid_message({publish, RequestId, Options, Topic, undefined, undefined});
is_valid_message({publish, RequestId, Options, Topic, Arguments}) ->
  is_valid_message({publish, RequestId, Options, Topic, Arguments, undefined});
is_valid_message({publish, RequestId, Options, Topic, Arguments, ArgumentsKw}) ->
  true = is_valid_id(RequestId),
  true = is_valid_dict(Options),
  true = is_valid_uri(Topic),
  true = are_valid_arguments(Arguments, ArgumentsKw),
  true;
is_valid_message({published, RequestId, PublicationId}) ->
  true = is_valid_id(RequestId),
  true = is_valid_id(PublicationId),
  true;
is_valid_message({subscribe, RequestId, Options, Topic}) ->
  true = is_valid_id(RequestId),
  true = is_valid_dict(Options),
  true = is_valid_uri(Topic),
  true;
is_valid_message({subscribed, RequestId, SubscriptionId}) ->
  true = is_valid_id(RequestId),
  true = is_valid_id(SubscriptionId),
  true;
is_valid_message({unsubscribe, RequestId, SubscriptionId}) ->
  true = is_valid_id(RequestId),
  true = is_valid_id(SubscriptionId),
  true;
is_valid_message({unsubscribed, RequestId}) ->
  true = is_valid_id(RequestId),
  true;
is_valid_message({event, SubscriptionId, PublicationId, Details}) ->
  is_valid_message({event, SubscriptionId, PublicationId, Details, undefined, undefined});
is_valid_message({event, SubscriptionId, PublicationId, Details, Arguments}) ->
  is_valid_message({event, SubscriptionId, PublicationId, Details, Arguments, undefined});
is_valid_message({event, SubscriptionId, PublicationId, Details, Arguments, ArgumentsKw}) ->
  true = is_valid_id(SubscriptionId),
  true = is_valid_id(PublicationId),
  true = is_valid_dict(Details),
  true = are_valid_arguments(Arguments, ArgumentsKw),
  true;
is_valid_message({call, RequestId, Options, Procedure}) ->
  is_valid_message({call, RequestId, Options, Procedure, undefined, undefined});
is_valid_message({call, RequestId, Options, Procedure, Arguments}) ->
  is_valid_message({call, RequestId, Options, Procedure, Arguments, undefined});
is_valid_message({call, RequestId, Options, Procedure, Arguments, ArgumentsKw}) ->
  true = is_valid_id(RequestId),
  true = is_valid_dict(Options),
  true = is_valid_uri(Procedure),
  true = are_valid_arguments(Arguments, ArgumentsKw),
  true;
is_valid_message({cancel, RequestId, Options}) ->
  true = is_valid_id(RequestId),
  true = is_valid_dict(Options),
  true;
is_valid_message({result, RequestId, Details}) ->
  is_valid_message({result, RequestId, Details, undefined, undefined});
is_valid_message({result, RequestId, Details, Arguments}) ->
  is_valid_message({result, RequestId, Details, Arguments, undefined});
is_valid_message({result, RequestId, Details, Arguments, ArgumentsKw}) ->
  true = is_valid_id(RequestId),
  true = is_valid_dict(Details),
  true = are_valid_arguments(Arguments, ArgumentsKw),
  true;
is_valid_message({register, RequestId, Options, Procedure}) ->
  true = is_valid_id(RequestId),
  true = is_valid_dict(Options),
  true = is_valid_uri(Procedure),
  true;
is_valid_message({registered, RequestId, RegistrationId}) ->
  true = is_valid_id(RequestId),
  true = is_valid_id(RegistrationId),
  true;
is_valid_message({unregister, RequestId, RegistrationId}) ->
  true = is_valid_id(RequestId),
  true = is_valid_id(RegistrationId),
  true;
is_valid_message({unregistered, RequestId}) ->
  true = is_valid_id(RequestId),
  true;
is_valid_message({invocation, RequestId, RegistrationId, Details}) ->
  is_valid_message({invocation, RequestId, RegistrationId, Details, undefined, undefined});
is_valid_message({invocation, RequestId, RegistrationId, Details, Arguments}) ->
  is_valid_message({invocation, RequestId, RegistrationId, Details, Arguments, undefined});
is_valid_message({invocation, RequestId, RegistrationId, Details, Arguments, ArgumentsKw}) ->
  true = is_valid_id(RequestId),
  true = is_valid_id(RegistrationId),
  true = is_valid_dict(Details),
  true = are_valid_arguments(Arguments, ArgumentsKw),
  true;
is_valid_message({interrupt, RequestId, Options}) ->
  true = is_valid_id(RequestId),
  true = is_valid_dict(Options),
  true;
is_valid_message({yield, RequestId, Options}) ->
  is_valid_message({yield, RequestId, Options, undefined, undefined});
is_valid_message({yield, RequestId, Options, Arguments}) ->
  is_valid_message({yield, RequestId, Options, Arguments, undefined});
is_valid_message({yield, RequestId, Options, Arguments, ArgumentsKw}) ->
  true = is_valid_id(RequestId),
  true = is_valid_dict(Options),
  true = are_valid_arguments(Arguments, ArgumentsKw),
  true;
is_valid_message({ping, Data}) ->
  true = is_binary(Data),
  true;
is_valid_message({pong, Data}) ->
  true = is_binary(Data),
  true;
is_valid_message(_) ->
  false.

is_valid_uri(Uri) when is_binary(Uri) ->
  is_valid_uri(Uri, default).

is_valid_id(Id) when is_integer(Id), Id >= 0, Id < 9007199254740992 -> true;
is_valid_id(_) -> false.

is_valid_dict(Dict) ->
  is_valid_dict(Dict, default).

is_valid_arguments(Arguments) when is_list(Arguments) -> true;
is_valid_arguments(undefined) -> true;
is_valid_arguments(_) -> false.

is_valid_argumentskw(ArgumentsKw) when is_map(ArgumentsKw) -> true;
is_valid_argumentskw(undefined) -> true;
is_valid_argumentskw(_) -> false.


%% @private
is_valid_uri(Uri, _Type) when is_binary(Uri) -> true;
is_valid_uri(Uri, _Type) when is_atom(Uri) -> true;
is_valid_uri(_, _) -> false.

%% @private
is_valid_dict(Dict, _Type) when is_map(Dict) -> true;
is_valid_dict(_, _) -> false.

%% @private
are_valid_arguments(undefined, undefined) ->
  true;
are_valid_arguments(Arguments, undefined) ->
  true = is_list(Arguments),
  true;
are_valid_arguments(Arguments, ArgumentsKw) ->
  true = is_list(Arguments),
  true = is_valid_dict(ArgumentsKw),
  true.
