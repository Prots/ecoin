PROJECT = ecoin

DEPS = ranch lager

dep_ranch = https://github.com/extend/ranch.git master
dep_lager = https://github.com/basho/lager.git master

include erlang.mk
