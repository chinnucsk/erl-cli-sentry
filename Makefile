REBAR := bin/rebar
GIT_SERVER := https://github.com/basho/rebar.git
DEPS := $(CURDIR)/deps
BIN := $(CURDIR)/bin
APP := erl_sentry

BUILD_PLT = $(APP).plt
BUILD_APPS = kernel ebin

DIALYZER = dialyzer

.PHONY: all deps doc clean

all:app

app:compile
	@$(REBAR) compile

compile:deps

deps:$(REBAR) 
	@$(REBAR) get-deps

doc:$(REBAR) app
	@$(REBAR) doc skip_deps=true

bin/%:
	mkdir -p $(DEPS) $(BIN)
	git clone $(GIT_SERVER) $(DEPS)/$*
	$(MAKE) -C $(DEPS)/$*
	cp $(DEPS)/$*/$* $(BIN)/$*

clean:$(REBAR)
	@$(REBAR) clean

ifndef SUITES
EUNIT_SUITEs = 
else
EUNIT_SUITES = suites=$(SUITES)
endif

test:$(REBAR) deps
	@$(REBAR) eunit skip_deps=true $(EUNIT_SUITES)

ct: $(REBAR) deps
	@CT_COMPILE=true $(REBAR) compile -D TEST
	@$(REBAR) ct skip_deps=true -v

rel: $(REBAR) deps
	@$(REBAR) generate

relclean: 
	@rm -rf rel/$(APP)

build-plt: 
	@$(DIALYZER) --build_plt --output_plt $(BUILD_PLT) --apps $(BUILD_APPS) 

dialyzer:
	@$(DIALYZER) --src -r src deps/*/src  --raw --statistics

xref: $(REBAR) clean app
	 @$(REBAR) xref skip_deps=true

lock-deps: $(REBAR) deps
	@$(REBAR) compile apps=rebar_lock_deps_plugin
	@$(REBAR) lock-deps skip_deps=true ignore=rebar skip_dirs=rel



