
-module(erl_sentry_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-include("erl_sentry.hrl").

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	Procs = procs([poolboy],[]),
    {ok, { {one_for_one, 5, 10}, Procs} }.

%% @spec procs([module()|{sup, module()}], [supervisor:child_spec()])-> [supervisor:child_spec()]
%% @doc return a list with worker or sup infomations.
-spec procs([module()|{sup, module()}], [supervisor:child_spec()])
    -> [supervisor:child_spec()].
procs([], Acc) ->
    lists:reverse(Acc);
procs([{sup, Module}|Tail], Acc) ->
    procs(Tail, [sup(Module)|Acc]);
procs([Module|Tail], Acc) ->
    procs(Tail, [worker(Module)|Acc]).

%% @spec worker(M) -> {M, {M, start_link, F::function()}, permanent, 5000, worker, dynamic}
%% @doc define a worker infomation.
-spec worker(M) -> {M, {M, start_link, F::function()}, permanent, 5000, worker, [M]}.
worker(Module) ->
    {Module, {Module, start_link, get_args(Module)}, permanent, 5000, worker, [Module]}.

%% @spec sup(M) -> {M, {M, start_link, F::function()}, permanent, 5000, supervisor, [M]}
%% @doc define a supversion infomation.
-spec sup(M) -> {M, {M, start_link, F::function()}, permanent, 5000, supervisor, [M]}.
sup(Module) ->
    {Module, {Module, start_link, get_args(Module)}, permanent, 5000, supervisor, [Module]}.

%% @doc get the args for poolboy.
get_args(poolboy) ->
	PoolBoyArgs = poolboy_args(),
	WorkerArgs = worker_args(),
	[PoolBoyArgs, WorkerArgs];	
get_args(_) ->
	[].

%% @doc get the detail configs for poolboy.
poolboy_args() ->
	{ok, Size} = application:get_env(erl_sentry, size),
	{ok, MaxOverFlow} = application:get_env(erl_sentry, max_overflow),
	[
		{name, {local, ?POOL}},
		{worker_module, erl_sentry_post},
		{size, Size},
		{max_overflow, MaxOverFlow}
	].

%% @doc get the work module args.
worker_args() ->
	{ok, Protocol} = application:get_env(erl_sentry, protocol),
	{ok, PublicKey} = application:get_env(erl_sentry, public_key),
	{ok, SecretKey} = application:get_env(erl_sentry, secret_key),
	{ok, Host} = application:get_env(erl_sentry, host),
	{ok, ProjectId} = application:get_env(erl_sentry, project_id),
	{ok, ServerName} = application:get_env(erl_sentry, server_name),
	{ok, Vsn} = application:get_env(erl_sentry, vsn),
	[Protocol, PublicKey, SecretKey, Host, ProjectId, ServerName, Vsn].
