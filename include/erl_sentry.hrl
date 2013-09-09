-define(POOL, erl_sentry).

-define(lift(Expr), erl_sentry_util:lift(fun() -> Expr end)).
-define(unlift(Expr), erl_sentry_util:unlift(fun() -> Expr end)).
