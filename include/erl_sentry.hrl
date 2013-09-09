-define(POOL, erl_sentry).

-define(lift(Expr), erl_sentry_util:lift(fun() -> Expr end)).
-define(unlift(Expr), erl_sentry_util:unlift(fun() -> Expr end)).

-define(handle(Res),
    case Res of
        {ok, {{200,_},_,_}} ->
                Res;
        {ok, {Code,_,Body}} ->
				error_logger:format("erl sentry errors code ~p body ~p", [Code, Body]),
                {error,{Code,Body}};
        {error, Reason} ->
				error_logger:format("erl sentry errors ~p", [Reason]),
                {error,Reason}
    end).
