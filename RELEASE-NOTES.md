### About this release

[This release](https://github.com/aeternity/epoch/releases/tag/v0.2.0-good-peers) is focused on syncing two or more nodes, sharing blocks and building a distributed blockchain. 

The best way to see this in action is to [set up 3 local nodes](https://github.com/aeternity/epoch/blob/a9611e4/doc/CUSTOMIZATION.md) and see how they communicate. 

Please follow the instructions below and let us know if you have any problems by [opening a ticket](https://github.com/aeternity/epoch/issues).

### Downloading the binaries

Download the [release binary](https://github.com/aeternity/epoch/releases/tag/v0.2.0-good-peers) corresponding to your platform, e.g. `osx-10.12.5-epoch-0.2.0.tar.gz`. You would normally find the downloaded package in `~/Downloads`. 

Open a Terminal window or get to the command-line. 

Create a directory and unpack the downloaded package.

    cd /tmp
    mkdir node1
    tar xf ~/Downloads/osx-10.12.5-epoch-0.2.0.tar.gz

### Configuring

Make two copies of the resulting directory to correspond to the other 2 nodes

    cp -r node1 node2
    cp -r node1 node3

Adjust the configuration for each node.

#### Node #1

    cd node1
    sed -ibkp 's/-sname epoch/-sname epoch1/g' ./releases/0.2.0/vm.args

In the same directory (`node1`), create the config file `p1.config` with the following contents

    [
    {aecore,
     [
      {peers, ["http://localhost:9902/",
               "http://localhost:9903/"]}
     ]},
    {aehttp,
     [
      {swagger_port_external, 9900}
     ]}
    ].

Change to the parent directory 

    cd ..

#### Node #2

    cd node2
    sed -ibkp 's/-sname epoch/-sname epoch2/g' ./releases/0.2.0/vm.args

In the same directory (`node2`), create the config file `p2.config` with the following contents

    [
    {aecore,
     [
      {peers, []}
     ]},
    {aehttp,
     [
      {swagger_port_external, 9902}
     ]}
    ].

Change to the parent directory 

    cd ..

#### Node #3

    cd node3
    sed -ibkp 's/-sname epoch/-sname epoch3/g' ./releases/0.2.0/vm.args

In the same directory (`node3`), create the config file `p3.config` with the following contents

    [
    {aecore,
     [
      {peers, []}
     ]},
    {aehttp,
     [
      {swagger_port_external, 9903}
     ]}
    ].

Change to the parent directory 

    cd ..

### Starting nodes

    cd node1
    ERL_FLAGS="-config `pwd`/p1.config" bin/epoch start
    cd ../node2
    ERL_FLAGS="-config `pwd`/p2.config" bin/epoch start
    cd ../node3
    ERL_FLAGS="-config `pwd`/p3.config" bin/epoch start

### Verifying that nodes are running

Tail logs to see how nodes sync and exchange blocks (in any node directory)

    tail -f log/epoch.log

The relevant lines are those containing strings `aec_peers` or `aec_sync`.

If node 1 manages to contact nodes 2 and 3 you will see something like the following in the logs of node 1:

    2017-10-06 13:21:19.729 [debug] <0.669.0>@aec_sync:compare_ping_objects:104 Compare, Local: #{<<"best_hash">> => <<"u7RpHLzlIZ4qqp+nBVgbR+9BPIla9hcnCd3Xv/H4vUc=">>,<<"difficulty">> => 1.0,<<"genesis_hash">> => <<"u7RpHLzlIZ4qqp+nBVgbR+9BPIla9hcnCd3Xv/H4vUc=">>,<<"peers">> => [<<"http://localhost:9902/">>],<<"share">> => 32,<<"source">> => <<"http://vagrant-ubuntu-trusty-64:9900/">>}
    2017-10-06 13:21:19.729 [debug] <0.669.0>@aec_sync:compare_ping_objects:108 genesis blocks match
    2017-10-06 13:21:19.729 [debug] <0.669.0>@aec_sync:compare_ping_objects:113 same top blocks
    2017-10-06 13:21:19.729 [debug] <0.669.0>@aec_peers:ping_peer:402 ping result ({peer,"http://localhost:9903/",0}): {ok,#{<<"best_hash">> => <<"u7RpHLzlIZ4qqp+nBVgbR+9BPIla9hcnCd3Xv/H4vUc=">>,<<"difficulty">> => 1.0,<<"genesis_hash">> => <<"u7RpHLzlIZ4qqp+nBVgbR+9BPIla9hcnCd3Xv/H4vUc=">>,<<"peers">> => [],<<"pong">> => <<"pong">>,<<"share">> => 32,<<"source">> => <<"http://vagrant-ubuntu-trusty-64:9903/">>}}
    2017-10-06 13:21:19.729 [debug] <0.661.0>@aec_peers:handle_cast:263 R = {alias,"http://vagrant-ubuntu-trusty-64:9903/","http://localhost:9903/"}
    2017-10-06 13:21:19.729 [debug] <0.668.0>@aec_sync:compare_ping_objects:104 Compare, Local: #{<<"best_hash">> => <<"u7RpHLzlIZ4qqp+nBVgbR+9BPIla9hcnCd3Xv/H4vUc=">>,<<"difficulty">> => 1.0,<<"genesis_hash">> => <<"u7RpHLzlIZ4qqp+nBVgbR+9BPIla9hcnCd3Xv/H4vUc=">>,<<"peers">> => [<<"http://localhost:9903/">>],<<"share">> => 32,<<"source">> => <<"http://vagrant-ubuntu-trusty-64:9900/">>}
    2017-10-06 13:21:19.729 [debug] <0.668.0>@aec_sync:compare_ping_objects:108 genesis blocks match
    2017-10-06 13:21:19.729 [debug] <0.668.0>@aec_sync:compare_ping_objects:113 same top blocks
    2017-10-06 13:21:19.730 [debug] <0.668.0>@aec_peers:ping_peer:402 ping result ({peer,"http://localhost:9902/",0}): {ok,#{<<"best_hash">> => <<"u7RpHLzlIZ4qqp+nBVgbR+9BPIla9hcnCd3Xv/H4vUc=">>,<<"difficulty">> => 1.0,<<"genesis_hash">> => <<"u7RpHLzlIZ4qqp+nBVgbR+9BPIla9hcnCd3Xv/H4vUc=">>,<<"peers">> => [],<<"pong">> => <<"pong">>,<<"share">> => 32,<<"source">> => <<"http://vagrant-ubuntu-trusty-64:9902/">>}}
    2017-10-06 13:21:19.730 [debug] <0.661.0>@aec_peers:handle_cast:263 R = {alias,"http://vagrant-ubuntu-trusty-64:9902/","http://localhost:9902/"}

When a node manages to mine a block, you will see something like the following in the logs of that node...

    2017-10-06 14:49:33.184 [debug] <0.598.0>@aec_sync:do_forward_block:265 send_block Res ({peer,"http://localhost:9902/",1507296079730027018}): {ok,ok}
    2017-10-06 14:49:33.185 [debug] <0.599.0>@aec_sync:do_forward_block:265 send_block Res ({peer,"http://localhost:9903/",1507296079729714151}): {ok,ok}

... and something like the following in the logs of the other two nodes:

    2017-10-06 14:49:33.177 [info] <0.1491.0> Attempt to process operation: 'PostBlock'
    2017-10-06 14:49:33.177 [debug] <0.1491.0>@aehttp_dispatch_ext:handle_request:89 'PostBlock'; header hash: <<159,132,193,233,50,163,141,172,136,74,50,5,46,67,121,99,165,101,40,115,170,112,121,21,8,121,238,132,56,122,150,212>>
    2017-10-06 14:49:33.178 [debug] <0.1491.0>@aehttp_dispatch_ext:handle_request:100 write_block result: ok

### Checking mining progress

Tail mining logs to see mining progress

    tail -f log/epoch_mining.log

For example, as each attempt to mine the next block does not always succeed, each 10 attempts you shall see something like the following in the node logs:

    2017-10-06 13:36:36.768 [info] <0.667.0>@aec_miner:running:163 Failed to mine block in 10 attempts, retrying.

When a node mines a block, you will see something like this in the logs:

    2017-10-06 14:49:33.145 [info] <0.667.0>@aec_miner:running:148 Block inserted: Height = 1

When a node managed to mine a block but such block is already obsolete because the local view of the longest chain moved ahead, you shall see something like the following on that node:

    2017-10-06 14:55:38.833 [error] <0.667.0>@aec_miner:running:157 Header insertion failed: {previous_hash_is_not_top,{top_header,{header,1,<<187,180,105,28,188,229,33,158,42,170,159,167,5,88,27,71,239,65,60,137,90,246,23,39,9,221,215,191,241,248,189,71>>,<<191,174,187,89,108,27,140,42,105,221,167,30,148,41,174,167,139,170,47,89,148,212,116,169,233,137,155,145,130,127,55,35>>,<<92,56,191,177,109,146,202,185,102,235,250,36,241,141,84,92,100,4,227,220,239,5,184,47,28,115,207,92,232,216,81,30>>,553713663,1125596692,1507300094807,1,[21444,6586407,9734926,17062307,19214768,19457624,20388691,22871531,22915558,35146174,37582353,41094493,50512682,50948743,52048504,54359502,58783324,67452260,67905997,72675145,76864212,78278965,80449837,81328841,91880678,93765245,95533733,97720772,101588269,102818356,102936000,107595015,107815273,110989552,112066712,113045125,114172277,115303768,118573306,125832620,127431107,128332518]}}}.

### Stopping mining and nodes

Please remember to stop all 3 nodes by running the following commands in each of the `node1`, `node2` and `node3` directories

    bin/epoch stop

### Danger! Expert zone!

Do not [customize the configuration of this release](https://github.com/aeternity/epoch/blob/master/doc/CUSTOMIZATION.md) unless you know what you are doing!
