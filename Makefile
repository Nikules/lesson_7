PROJECT = cache_server
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0
DEPS = cowboy jsx
dep_cowboy_commit = 2.6.3
dep_jsx = git https://github.com/talentdeficit/jsx.git master
DEP_PLUGINS = cowboy


include erlang.mk
