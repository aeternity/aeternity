#!/bin/bash
set -e

# Using console with extra arguments because "foreground" does not handle SIGTERM/SIGQUIT
exec ./bin/epoch console -noshell -noinput $@
