# Unit Testing

In Erlang you would typically call three things a unit in the context of
unit testing: a function, a module and a process (or rather the code a
process runs). A Unit test suite is a collection of tests that describe
which unit they address and then provide a reasonable coverage over that
unit. If the unit depends on other parts, such as file system, libraries,
etc, then those might be mocked (i.e., be replaced by code that is
predictable and repeatable).

We aim for having a unit test suite for each module, because very often our
functions are too little and simple to spend a complete unit test suite on
and our processes are mainly contained in one module with some library
modules to build upon.

**If a unit test fails, then we know that the error is in that module**

Best practice is when unit tests are side effect free. That is, when you
run the test, you always get the same result. That means that one may need
to mock the parts that can influence the outcome of a test. For side-effect
free, or pure, functions, this is no issue. For processes however, or
functions that interact with the operating system or other parts of the
system, this is highly relevant.


## Running unit tests

We place our unit tests in a directory called `test`, for example, unit
tests for modules in the `aecore` application are in
[`apps/aecore/test`](../tree/master/apps/aecore/test/).

Running all unit tests in the project can be done by

```shell
make eunit-latest
```

It will automatically search in all `apps/*/test` directories after files
named `*_tests.erl` which are considered containing Eunit tests. Note that
it is important to name the unit tests with the same name as the Erlang
module in order to have the tool chain recognize them. Thus an Erlang
module called M.erl in `apps/*/src/` shall have a module called M_tests.erl
in `apps/*/test/`.

Unit tests can be run per tests file, for example

```shell
make eunit-latest TEST=aec_peers
```

will run the unit tests in
[`apps/aecore/test/aec_peers_tests.erl`](../tree/master/apps/aecore/test/aec_peers_tests.erl).

Note that we also have a directory [`test`](../tree/master/test/) on the
top level. One should NOT put Eunit tests in there, but only test suites
run with common test that test interaction between the different
applications in `apps`. In short, this directory is meant for application
integration tests.


# Integration Testing

The purpose of integration testing is to ensure that newly added code does
not break the system. We run some basic tests on three Erlang nodes These
nodes should be able to perform the basic operations, such as mining
blocks, synchronizing blocks, handling transactions, etc.

For each application there might be some integration tests provided that
focus on the integration of processes in that application. The vision is to
have tests that integrate a number of different applications in the top
level test directory.

Integration tests are executed by common test and are therefore stored in
files named `*_SUITE.erl`. Running all integration tests is done by

```shell
make ct-latest
```

Since the complete collection of integration tests is run at each pull
request of new software, the tests have to be able to run in a reasonably
short time interval (aim for running the complete suite to completion in 15
minutes or less).

The API used in these tests are Erlang functions, typically the API of gen
servers and the like. We mock the mining algorithm or use one that can mine
very fast, such as lean16 or so.


## Running Specific Tests

To run a specific suite, group or test case, you can use the environment
variables `SUITE`, `GROUP` and `TEST`. Examples:

```shell
$ make ct-latest SUITE=apps/aecore/test/aecore_sync
$ make ct-latest SUITE=apps/aehttp/test/aehttp_integration GROUP=external_endpoints
$ make ct-latest SUITE=apps/aehttp/test/aehttp_integration GROUP=external_endpoints TEST=get_transaction
```

The environment variables can be combined to narrow the scope of which
tests to run, however `SUITE` must always be used when `GROUP` and/or
`TEST` is used. The prefix `_SUITE` is automatically added to the specified
suite name. Note that we need to specify the full path to the suite, as the
code is divided into apps in subfolders.

The `DIR` environment variable can be used to run all the tests in a
specific directory (or directories - most of the test related flags above
accept comma separated lists):

```shell
$ make ct-latest DIR=apps/aehttp/test
$ make ct-latest DIR=apps/aehttp/test,apps/aecore/test
```


## Typical Requirements to Test

1. Each node should be able to mine a block and add it to the chain of the
   system.

2. If a fork is created then discarded the fork with lowest PoW (exact
   requirements to be added)

3. We are able to post a spend transaction that is occurring in the chain.

4. We are able to pay a fee for a spend transaction.

5. We collect a transaction fee by mining.

6. We can start and restart the applications.


# System Testing

The purpose of system testing is to make sure the system as a whole works.
That means that we want at least 3 nodes running, preferably on different
platforms (OS and architecture). These nodes should be able to perform the
basic operations, such as mining blocks, synchronizing blocks, handling
transactions, etc.

We establish this using docker. First read the information on [how to work
with our docker
images](https://github.com/aeternity/epoch/blob/master/docs/docker.md).

```shell
$ make system-test-deps
```

The API used in these tests (apart from building and starting) is purely
the web interface. We use a mining algorithm that can mine very fast, such
as lean16 or so. In the [User Acceptance Test](User-Acceptance-testing) the
real miner will be tested.

How to run the system tests:

```shell
$ make system-test
```

How to keep the docker containers and networks created during the tests:

```shell
$ EPOCH_DISABLE_NODE_CLEANUP=1 make system-test
```


## Running Specific Tests

To run a specific suite, group or test case, you can use the environment
variables `SUITE`, `GROUP` and `TEST`. Examples:

```shell
$ make system-test SUITE=aest_sync
$ make system-test SUITE=aest_perf GROUP=long_chain
$ make system-test SUITE=aest_sync TEST=new_node_joins_network
```

The environment variables can be combined to narrow the scope of which
tests to run, however `SUITE` must always be used when `GROUP` and/or
`TEST` is used. The prefix `_SUITE` is automatically added to the specified
suite name.


## Running Special Groups of Test Suites

How to run a fast subset of the system tests:

```shell
$ make system-smoke-test-deps
$ make smoke-test-run
```

How to run the system tests meant to be run only locally i.e. not run by
CI:

```shell
$ make local-system-test
```


## Typical Requirements to Test

1. The nodes in the system should be able to connect to its peers.
   - If the node has been back-listed by the peers before, then we are able
     to re-connect after X

2. Each node should be able to mine a block and add it to the chain of the
   system.

3. If one node disconnects, a fork is created, that is:
   1. Discarded when re-connecting to the other nodes if other chain has
      more PoW (exact requirements to be added)
   2. Accepted by the other nodes if this fork has a larger PoW (exact
      requirements to be added)

4. Each node is able to:
   1. Post a spend transaction that is occurring in the chain.
   2. Pay a fee for a spend transaction.
   3. Collect a transaction fee by mining.

5. We can access all endpoints specified in the swagger.yaml file with
   expected results. (This needs further refinement)

6. A node that is stopped for 5 minutes (enough to create additional blocks
   on the chain) should be able to restart and satisfy 1, 2 and 5 above.

7. A node that sends invalid data to other nodes is black-listed.


## System Testing on Continuous Integration Infrastructure

On branch `master`:
* System tests (`make system-test`) are run twice a day.
* No system tests are run whenever a pull request is merged.

On "normal" branches (i.e. not `env/*`, not `system-tests`):
* System *smoke* tests are run (`make smoke-test-run`).

On branch `system-tests`:
* System tests (`make system-test`) are run.

On tags:
* No system tests are run.


# User Acceptance Testing

Although the terminology might be a bit confusing, we test system
requirements in the user acceptance testing phase (see Testing-general).

The purpose of this test is to make sure that when users download our
software, they can start working with it the way they expect. This also
includes that they are able to install it according to the release notes
and other documents describing what to do. However, automation of these
tasks is out of reach and therefore this is not done systematically.

For the user acceptance test we need an infrastructure with quite some
miners, but few enough to make sure we can contribute with a mined block
in, say, an hour. We refer to this as the uat-net. If we can connect our
updated miner software to a uat-net in a user acceptable way, then we
assume it will also work for the main net. It is important that the uat-net
has miners that run the same release software as miners on the main net as
well as some other versions (which is also possible on the main net).

The API used in these tests (apart from building and starting) is purely
the web interface.

Typical requirements we should test:

1. A user clone or pull of the master branch of the repository should build
   using `make` without errors.
2. A user clone or pull of the master branch of the repository should run
   `make test` without errors.

3. A user downloaded or created production package (`make prod-package`)
   that is correctly configured against the uat-net:
   1. Should be able to connect to the uat-net and show the correct top
      block of the uat-net.
   2. Should be able to mine a block and add it to the chain of the uat-net
      (within an hour).
   3. If disconnected from the uat-net, a fork is created, that is
      discarded when re-connecting to the uat-net (we assume the uat-net to
      have more mining power than a single miner).

4. A user downloaded or created production package is, by running for one
   hour, able to:
   1. Post a spend transaction that is occurring in the uat-net chain.
   2. Pay a fee for a spend transaction.
   3. Collect a transaction fee by mining.

5. A user can access all endpoints specified in the swagger.yaml file with
   expected results. (This needs further refinement)

6. A user should be able to stop the miner and restart it after 15 minutes
   with the effect that 3.i, 3.ii and 5 above hold.

7. A user should be able to update the software and continue with the saved
   chain as starting point. In that case 3, 4 and 5 above should be
   fulfilled.
