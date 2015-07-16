%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Jul 2015 15:00
%%%-------------------------------------------------------------------
-module(wamper_auth_test).
-author("tihon").

-include_lib("eunit/include/eunit.hrl").

-define(CHALLENGE,
  <<<<"{\"nonce\": \"LHRTC9zeOIrt_9U3\", \"authprovider\": \"userdb\", \"authid\": \"peter\",\"timestamp\":">>/binary,
  <<" \"2015-01-29T20:36:25.448Z\", \"authrole\": \"user\",\"authmethod\": \"wampcra\", \"session\":">>/binary,
  <<"3251278072152162}">>/binary>>).

wamp_cra_test() ->
  Key = <<"secret1">>,
  Signature = <<"/h8nclt5hisNxpVobobQR7f8nL1IAZhsllT014mo/xg=">>,
  Signature = wamper_auth:wamp_cra(Key, ?CHALLENGE),
  ok.