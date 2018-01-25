Python user acceptance tests
==========

In order to generate tests:
* prepare nosetests venv: `make nose-env`

* generate python client for the API: `make swagger`

In order to run tests first prepare the nodes:
```
make multi-build
```
Then:

* execute all user acceptance tests: `make python-uats`

* execute a singe acceptance tests group: `make python-single-uat TEST_NAME=use_cases` would run `test_use_cases.py` tests

