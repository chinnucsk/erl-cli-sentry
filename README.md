erl-cli-senty
=============

__Authors:__[`lucas`](mailto:564985699@qq.com).

__HomePage:__[`erl-cli-sentry`](http://lucas564985699.github.io/erl-cli-sentry).

Erl-cli-sentry is a erlang client for posting datas to sentry server, we call monitor our erlang server dymaticly.

If you know senty little, please read it first.
[`sentry`](http://sentry.readthedocs.org/en/latest/index.html).

Introduce 
-------------

Directory
* priv <br />
	+ `app.config`: config file <br />
* include <br />
	+ `erl_sentry.hrl`: header file <br />
* test <br />
	+ `erl_sentry_example.erl`: example file <br />
* rebar.config <br />
* start.sh <br />
* Makefile <br />
* README.md <br />
* src <br />
	+ `erl_sentry_app.erl`: application file <br />
	+ `erl_sentry.app.src`: application file <br />
	+ `erl_sentry_sup.erl`: application supervision file <br />
	+ `erl_sentry.erl`: api file <br />
	+ `erl_sentry_post.erl`: gen_server handle file <br />
	+ `erl_sentry_util.erl`: normal file <br />

Configure
---------------

example:
```javascript
[{erl_sentry,[
            {protocol, "http"},
            {public_key, "206340eaeea940fd95ad03fe822db7d6"},
            {secret_key, "d31b136f2e8c477b8a6d810eb49849fb"},
            {host, "127.0.0.1:9001"},
            {project_id, "erl_sentry-1"},
            {server_name, "erl_sentry"},
            {vsn, 4},
            {size, 10},
            {max_overflow, 100}
            ]
}].
```
* `protocol`: http or https <br />
* `public_key`: generated in creating a new project <br />
* `host`: the sentry ip address <br />
* `project_id`: the name of new project <br />
* `server_name`: the name you defined for identifing <br />
* `vsn`: the version of sentry client <br />
* `size`: poolboy size <br />
* `max_overflow`: poolboy max_overflow <br />

Example
--------------

Directory test exists erl_sentry_example.erl, you can know the usage accorrding it.

Doc
------------

Use edown to generate documents,configs locates in rebar.config(edoc_opts),doclet uses edown_doclet,layout uses edown_layout,other config please reference [`edown`](http://www.erlang.org/doc/apps/edoc/chapter.html) [`edoc`](http://www.erlang.org/doc/man/edoc.html).

useage:make doc

Rel
-----------

* create rel:make rel<br />
* clean rel:make relclean <br />

Dialyzer
-----------

To analysis software discrepancies such as definite type errors, code which has become dead or unreachable due to some programming error,unnecessary tests,etc.
<br />

* create plt:make build-plt
* dialyzer:make dialyzer

Xref
----------

A Cross Reference Tool for analyzing dependencies between functions, modules, applications and releases. <br />

* make xref

How To Use
------------

```javascript
%% @doc api method to send message.
post(Message, Opts) ->
    erl_sentry_post:send(Message, Opts).
```

Method has two parameters: 
* `Message :: list(tuple())`, including `msg`(required), `extra`, `exception`, `http`, `user` <br />
	+ `msg` is required, others you can choose <br /> 
	+ `extra` is used to adding other tags <br /> 
	+ `exception` is used to show things exception <br /> 
	+ `http` is used to show your http request, including `header`, `method`, `url`, `body`, `paras` and so on <br />
	+ `user` is used to show your user information ,you can define it ,for example, `id`, `data` <br />
* `Opts :: list(tuple())`, defines the level of report, for example `[{level, "error"}]`, you can define it accorrding your needs <br />


```javascript
	make && ./start.sh
	erl_sentry:post(Message, Opts).
```

Message example:
```javascript
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
        ]
```

Opts example:
```javascript
	[{level, "error"}]
```

Images
-------------
* show project
	![erl_sentry_project](/img/erl_sentry_project.png)
* show detail
	![erl_sentry_detail](/img/erl_sentry_detail.png)
	

