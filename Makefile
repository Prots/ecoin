PROJECT = ecoin

DEPS = ranch lager

dep_ranch = https://github.com/extend/ranch.git master
dep_lager = https://github.com/basho/lager.git master

ERLC_OPTS = "+{parse_transform, lager_transform}" "+debug_info"

include erlang.mk
