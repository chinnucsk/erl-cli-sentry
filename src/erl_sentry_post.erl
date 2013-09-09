-module(erl_sentry_post).

-behaviour(gen_server).

-export([
		start_link/1,
		send/2
		]).

-export([
        init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3
        ]).

-include("erl_sentry.hrl").

-record(state,{protocol::list(),
				public_key::list(),
				secret_key::list(),
				host::list(),
				project_id::list(),
				server_name::list(),
				vsn::integer()
				}).

%% @sepc start_link(Args) -> pid().
%% @doc start gen_server
start_link(Args) ->
	gen_server:start_link(?MODULE, Args, []).

%% @doc send msessage to sentry.
%% @spec send(list(), list()) -> ok.
-spec send(list(), list()) -> ok.
send(Message, Opts) ->
	Worker = poolboy:checkout(?POOL),
	gen_server:cast(Worker,{save, Message, Opts}),
	poolboy:checkin(?POOL, Worker),
	ok.

%% call back init functions.
init([Protocol, PublicKey, SecretKey, Host, ProjectId, ServerName, Vsn]) ->
	process_flag(trap_exit, true),
	{ok,#state{protocol=Protocol, public_key=PublicKey, secret_key=SecretKey,
			host=Host, project_id=ProjectId, server_name=ServerName, vsn=Vsn}}.

%% call back call functions.
handle_call(_Info, _From, State) ->
	{ok, ok, State}.

%% call back cast functions.
handle_cast({save, Message, Opts}, State) ->
	post_message(Message, Opts, State),
	{noreply, State}.

%% call back info functions.
handle_info({'EXIT', From, Reason}, State) ->
	error_logger:error_msg("Accoure errors from ~p reason is ~p~n",[From, Reason]),
	{noreply, State};
handle_info(_Info, State) ->
	{noreply, State}.

%% call back terminate functions.
terminate(Reason, _State) ->
	error_logger:error_msg("Terminat ~p~n",[Reason]),
	ok.

%% call back code change functions.
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%% ---------------------------------------------------------------------
%% internal functions
%% ---------------------------------------------------------------------

%% @doc build the requests and parse the results.
post_message(Message, Opts, #state{protocol=Protocol, public_key=PublicKey, secret_key=SecretKey, host=Host, project_id=ProjectId, server_name=ServerName, vsn=Vsn}) ->
	Url = erl_sentry_util:build_url(Protocol, Host, ProjectId),
	Header = erl_sentry_util:build_header(PublicKey, SecretKey, Vsn),
	Body = erl_sentry_util:build_body(Message, ServerName, Opts),
	erl_sentry_util:req(post, Url, Header, Body).	
