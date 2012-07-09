#!/bin/bash

colnum="\$1"
for proc in  `ps -u $(whoami) | awk  "/$1/ {print $colnum}"`
do
    kill -9 $proc &>/dev/null
done
sleep 2 # arbitrary wait to allow process teardown

