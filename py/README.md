Python user acceptance tests
==========

This document describes how to run them.

#### Python enviroment

Python tests are to be run on Python `2.7` and this is a prerequisite. Tests
dependencies are installed in an virtual enviroment. To install deps:
```
make python-env
```

#### Epoch nodes

Tests are ran on `dev1`, `dev2` and `dev3` epoch nodes. In order to build
them, please run

```
make multi-build
```

#### Running tests

After the python dependencies are installed and the nodes are build, tests can
be run

* execute all user acceptance tests: `make python-uats`

* execute a single acceptance tests group:
  * `make python-single-uat TEST_NAME=test_use_cases.py` would run `test_use_cases.py` tests
  * `make python-single-uat TEST_NAME=test_api_usage_contracts_counter.rst` would run `test_api_usage_contracts_counter.rst` test
