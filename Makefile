PROJECT = ensafen
PROJECT_DESCRIPTION = Make a web endpoint safer by protecting it with this reverse proxy.
PROJECT_VERSION = 0.0.2
DEPS = cowboy gun
LOCAL_DEPS = inets
ESCRIPT_SYS_CONFIG ?= "config/sys.config"

include erlang.mk
