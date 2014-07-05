#!/bin/bash

config_dir=profiles
config_name=$1
config=$config_dir/$config_name
config_file=$config.config

if [[ ! -f $config_file ]]; then
    echo "Usage: $0 config"
    echo "Available configurations:"
    for c in $(ls $config_dir/*.config)
    do
        echo "* $(basename -s ".config" $c)"
    done
    exit
fi

echo "Starting ecoin with configuration: $config_name"

erl -pz ebin/ deps/*/ebin/ -config $config -s ecoin start
