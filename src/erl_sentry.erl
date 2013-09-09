-module(erl_sentry).

-export([
		start/0,
		stop/0,
		post/2
		]).

%% @doc start application.
start() ->
	application:start(?MODULE).

%% @doc stop applicaiton.
stop() ->
	application:stop(?MODULE).

%% @doc api method to send message.
post(Message, Opts) ->
	erl_sentry_post:send(Message, Opts).

