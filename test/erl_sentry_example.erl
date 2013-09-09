-module(erl_sentry_example).

-export([
		post/0
		]).

%% @doc A detail example to teach how to use.
post() ->
	Url = erl_sentry_util:build_url("http", "127.0.0.1:9001", "erl_sentry-1"),
	Header = erl_sentry_util:build_header("206340eaeea940fd95ad03fe822db7d6", "d31b136f2e8c477b8a6d810eb49849fb", 4),
	Level = [{level, "error"}],
	Msg = [
			{msg, "unkonw error"},
			{execption, [
						{time, "123"}
						]},
			{extra, [
					{time, "123"}
					]},
			{http, [
					{url, "http://localhost"},
					{method, "put"},
					{header, "[{Content-Type,\"application/json\"}]"}
					]},
			{user, [
					{id, "123456789"},
					{name, "lucas"}
					]}
		],
	Body = erl_sentry_util:build_body(Msg, "erl_sentry_test", Level),
	erl_sentry_util:req(post, Url, Header, Body).
