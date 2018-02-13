#!/bin/bash -ex
git clone git@github.com:aeternity/epoch-api-docs.git /tmp/epoch-api-docs
cp apps/aehttp/priv/swagger-docs/swagger.json /tmp/epoch-api-docs/
cd /tmp/epoch-api-docs/

git add swagger.json;

git config user.email "circleci@aeternity.com";
git config user.name "CircleCi Aeternity";

STATUS=`git status --porcelain`

if [ -z "$STATUS" ]; then
    echo "Nothing to commit, docs did not change";
else
    if [ -n "$CIRCLE_TAG" ]; then
        git commit -a -m 'Update epoch docs CircleCI';
        git push origin master
    else
        echo "no master, no tag, not commit!"
    fi
fi
