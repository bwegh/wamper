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
-author("tihon").

-define(HELLO, 1).
-define(WELCOME, 2).
-define(ABORT, 3).
-define(CHALLENGE, 4).
-define(AUTHENTICATE, 5).
-define(GOODBYE, 6).
-define(HEARTBEAT, 7).
-define(ERROR, 8).

-define(PUBLISH, 16).
-define(PUBLISHED, 17).

-define(SUBSCRIBE, 32).
-define(SUBSCRIBED, 33).
-define(UNSUBSCRIBE, 34).
-define(UNSUBSCRIBED, 35).
-define(EVENT, 36).

-define(CALL, 48).
-define(CANCEL, 49).
-define(RESULT, 50).

-define(REGISTER, 64).
-define(REGISTERED, 65).
-define(UNREGISTER, 66).
-define(UNREGISTERED, 67).
-define(INVOCATION, 68).
-define(INTERRUPT, 69).
-define(YIELD, 70).