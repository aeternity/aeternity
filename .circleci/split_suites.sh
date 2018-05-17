#!/bin/bash

set -e

ls -1 apps/*/test/*SUITE.erl test/*SUITE.erl > suites.txt
cat suites.txt | sed 's/.*\///g' | sed 's/.erl//g' > classes.txt
circleci tests split --split-by=timings --timings-type=classname classes.txt > batch.txt
grep -f batch.txt suites.txt | paste -s -d ',' -
