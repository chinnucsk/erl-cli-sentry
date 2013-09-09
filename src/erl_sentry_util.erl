-module(erl_sentry_util).

-export([
		build_url/3,
		build_header/3,
		build_body/3,
		req/4,
		to_b/1,
		lift/1,
		unlift/1
		]).

%% @doc build the url.
%% @spec build_url(list(),list(), list()) -> list().
-spec build_url(list(),list(), list()) -> list().
build_url(Protocol, Host, ProjectId) ->
	List= [Protocol, "://", Host, "/api/", ProjectId, "/store/"],
	lists:concat(List).	

%% @doc build the header.
%% @spec build_header(list(),list(), integer()) -> list().
-spec build_header(list(),list(), integer()) -> list().
build_header(PublicKey, SecretKey, Vsn) ->
	SentyAuth = get_sentry_auth(PublicKey, SecretKey, Vsn),
	Agent = get_agent(),
	ContentType = get_content_type(),
	[SentyAuth, Agent, ContentType].

%% @doc build the body.
%% @spec build_body(list(),list(), list()) -> list().
-spec build_body(list(),list(), list()) -> list().
build_body(Message, ServerName, Opts) ->
	EventId = get_eventid(),
	TimeStamp = get_timestamp(),
	{_, Level} = lists:keyfind(level, 1, Opts),
	[Msg, Exception, Extra, Http, User] = decode_msg(Message),
	Packat = [
			{event_id, EventId},
			{timestamp, to_b(TimeStamp)},
			{message, Msg},
			{level, to_b(Level)},
			{platform, <<"erlang">>},
			{server_name, to_b(ServerName)},
			{extra, Extra},
			{exception, Exception},
			{'sentry.interfaces.Http', Http},
			{'sentry.interfaces.User', User}
			],
	encode_body(Packat).

%% @doc make a req use lhttpc.
req(Method, Url, Hdrs, Body) ->
    lhttpc:request(Url, Method, Hdrs, Body, infinity).

%% @doc transfer data to binary.
to_b(Value) when is_binary(Value) ->
	Value;
to_b(Value) when is_integer(Value) ->
	to_b(integer_to_list(Value));
to_b(Value) when is_list(Value) ->
	list_to_binary(Value).

%% --------------------------------------------------------------
%% internal functions
%% --------------------------------------------------------------

%% @doc get auth header of sentry.
get_sentry_auth(PublicKey, SecretKey, Vsn) ->
	TimeStamp = get_timestamp(),
	List = [
			"Sentry sentry_version=", Vsn,",",
			"sentry_client=erl_sentry/", Vsn, ",",
			"sentry_timestamp=", TimeStamp, ",",
			"sentry_key=", PublicKey, ",",
			"sentry_secret=", SecretKey
			],
	{"X-Sentry-Auth", lists:concat(List)}.

%% @doc get user-agent header of sentry.
get_agent() ->
	{"User-Agent", "erl_sentry"}.

%% @doc get content-type header of sentry.
get_content_type() ->
	{"Content-Type", "application/octet-stream"}.

%% @doc get now current time in UTC.
get_timestamp() ->
	{{Y,M,D},{HH,MM,SS}} = calendar:local_time(),
	List = [integer_to_list(Y), "-", integer_to_list(M), "-", integer_to_list(D), "T", integer_to_list(HH), ":", integer_to_list(MM), ":", integer_to_list(SS)],
	lists:concat(List).

%% @doc encode the json args.
encode_body(Args) ->
    jsx:encode(Args).

%% @doc decode the json args.
decode_body(Args) ->
    jsx:decode(Args, [{labels, binary}]).

%% @doc use application uudi to produce a id.
get_eventid() ->
	<<A:128>> = uuid:uuid4(),
	list_to_binary(io_lib:format("~32.16.0b", [A])).

%% @doc decode body message, return a tuple list.
decode_msg(Message) ->
	Msg = decode_msg(msg, Message),
	Extra = decode_msg(extra, Message),
	Expection = decode_msg(expection, Message),
	Http = decode_msg(http, Message),
	User = decode_msg(user, Message),
	[Msg, Expection, Extra, Http, User].


%% @doc decode body accorrding the corresponding type.
decode_msg(Type, Message) ->
	decode_type_msg(Type, Message).

decode_type_msg(_Type, []) ->
	[];
decode_type_msg(msg, [{msg, Msg}|_Rest]) ->
	to_b(Msg);
decode_type_msg(Type, [{Type, Msg}|_Rest]) ->
	handle_content(Msg, []);
decode_type_msg(Type, [_|Rest]) ->
	decode_msg(Type, Rest).

%% @doc handle the content of detail value.
handle_content([], Acc) ->
	lists:reverse(Acc);
handle_content([{Atom, Value}|Rest], Acc) ->
	handle_content(Rest, [{Atom, to_b(Value)}|Acc]).

%% @doc Ensure that the result of F, which must be a thunk, is in
lift(F) ->
    try F() of
        ok                  -> ok;
        {ok, Res}           -> {ok, Res};
        error               -> {error, error};
        {error, Rsn}        -> {error, Rsn};
        Res                 -> {ok, Res}
    catch
        throw:{error, Rsn}  ->
            {error, Rsn};
        _:Exn ->
            {error, {lifted_exn, {Exn, erlang:get_stacktrace()}}}
    end.

-spec unlift(fun()) -> _.
%% @doc Ensure that the result of F, which must be a thunk, is not in
%% maybe().
unlift(F) ->
    case F() of
        ok           -> ok;
        {ok, Res}    -> Res;
        error        -> throw({error, error});
        {error, Rsn} -> throw({error, Rsn});
        Res          -> Res
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

util_test() ->

	%% build url test
	?assertEqual("http://127.0.0.1:9001/api/erl_sentry-1/store/", 
				build_url("http", "127.0.0.1:9001", "erl_sentry-1")),

	%% build header test
	?assertMatch([{_, _}, _, _], 
				build_header("206340eaeea940fd95ad03fe822db7d6", "d31b136f2e8c477b8a6d810eb49849fb", 4)).

-endif.
