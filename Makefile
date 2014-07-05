PROJECT = ecoin

DEPS = ranch lager exometer epgsql

dep_ranch = https://github.com/extend/ranch.git master
dep_lager = https://github.com/basho/lager.git master
dep_exometer = https://github.com/Feuerlabs/exometer.git master
dep_epgsql = https://github.com/epgsql/epgsql.git master

ERLC_OPTS = "+{parse_transform, lager_transform}" "+debug_info"

include erlang.mk
