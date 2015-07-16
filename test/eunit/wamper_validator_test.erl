%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Jul 2015 15:03
%%%-------------------------------------------------------------------
-module(wamper_validator_test).
-author("tihon").

-include_lib("eunit/include/eunit.hrl").

validation_test() ->
  true = wamper_validator:is_valid_id(0),
  true = wamper_validator:is_valid_id(9007199254740991),
  false = wamper_validator:is_valid_id(9007199254740992),
  false = wamper_validator:is_valid_id(-1),
  false = wamper_validator:is_valid_id(0.1),

  true = wamper_validator:is_valid_uri(<<"wamp.ws">>),
  false = wamper_validator:is_valid_dict([]),
  true = wamper_validator:is_valid_dict(#{}),

  true = wamper_validator:is_valid_arguments([]),
  false = wamper_validator:is_valid_argumentskw([]),
  true = wamper_validator:is_valid_argumentskw(#{}),
  ok.