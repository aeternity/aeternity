#!/bin/bash

set -e

git clone git@github.com:aeternity/api-docs.git /tmp/api-docs
cp apps/aehttp/priv/swagger.json /tmp/api-docs/
cd /tmp/api-docs/

git add swagger.json;
STATUS=`git status --porcelain`

if [ -z "$STATUS" ]; then
    echo "Nothing to commit, docs did not change";
else
    git config user.name "CircleCI"
    git config user.email "circleci@aeternity.com"
    git commit -a -m "Update aeternity version: $CIRCLE_TAG docs CircleCI";
    git push origin master
fi
