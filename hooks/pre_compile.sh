#!/bin/bash

EPOCH_PROCESSES=$(ps -fea|grep "bin/epoch" | grep -v grep | wc -l)

COLOR_RED=`tput setaf 1`
COLOR_RESET=`tput sgr0`
if [ "$EPOCH_PROCESSES" -gt "0" ]; then
  echo "${COLOR_RED}WARNING: an Epoch node is already running${COLOR_RESET}"
fi
