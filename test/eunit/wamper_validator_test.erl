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