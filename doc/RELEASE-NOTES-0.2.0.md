# RELEASE NOTES for "Good Peers" sprint

The 3rd sprint of our development focused on the following issues:
* ready all objects in the system for network exchange: define canonical form and serialization
* build sync infrastructure
* further work on internal state structure: Authenticated Dictionaries
* further research on consensus architecture

The packages for the this release are available at the following location:
https://github.com/aeternity/epoch/releases/tag/v0.2.0-good-peers

To fully appreciate the newest developments you need to experience syncing nodes and sharing blocks.

The easiest way to achieve it, is to deploy 3 local nodes and see how they communicate.
Please find detailed information about custom configuration here:
https://github.com/aeternity/epoch/blob/a9611e4/doc/CUSTOMIZATION.md

1. Download packages for Good Peers release, unpack and enter the directory.

2. Make total 3 copies and adjust VM config

Please use following sed command - tested on OSX and on Ubuntu (as per Vagrant box "ubuntu/trusty64"):

* For node 1 (when in the top release dir):
  ```
  sed -ibkp 's/-sname epoch/-sname epoch1/g' ./releases/0.2.0/vm.args
  ```

* For node 2 (when in the top release dir):
  ```
  sed -ibkp 's/-sname epoch/-sname epoch2/g' ./releases/0.2.0/vm.args
  ```

* For node 3 (when in the top release dir):
  ```
  sed -ibkp 's/-sname epoch/-sname epoch3/g' ./releases/0.2.0/vm.args
  ```

3. Create following files in the top release dir:

In node 1, file:

p1.config:
```
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
```

In node 2, file:

p2.config
```
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
```

In node 3, file:

p3.config
```
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
```

4. Use commands supplied below to start nodes. Start nodes in the order 3 2 1 - i.e. start node 1 last.

  * In directory for node 3, use command: `ERL_FLAGS="-config <Absolute Path>/p3.config" bin/epoch start`
  * In directory for node 2, use command: `ERL_FLAGS="-config <Absolute Path>/p2.config" bin/epoch start`
  * In directory for node 1, use command: `ERL_FLAGS="-config <Absolute Path>/p1.config" bin/epoch start`

5. Tail logs to see how nodes sync and exchange blocks
    ```
    tail -f log/epoch.log
    ```

The relevant lines are those containing strings `aec_peers` or `aec_sync`.

For example, if node 1 does not manage to contact nodes 2 and 3 you will see something like the following in the log of node 1:
```
2017-10-06 13:05:54.632 [debug] <0.669.0>@aec_peers:ping_peer:402 ping result ({peer,"http://localhost:9903/",0}): {error,"A problem occured"}
2017-10-06 13:05:54.632 [debug] <0.668.0>@aec_peers:ping_peer:402 ping result ({peer,"http://localhost:9902/",0}): {error,"A problem occured"}
```
In this case, assuming nodes 2 and 3 are already up, stop node 1 (in directory for node 1: `bin/epoch stop`) and then start node 1.

In the case that node 1 manages to contact nodes 2 and 3 you will see something like the following in the log of node 1:
```
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
```

When a node manages to mine a block, you shall see something like the following in the logs of that node...
```
2017-10-06 14:49:33.184 [debug] <0.598.0>@aec_sync:do_forward_block:265 send_block Res ({peer,"http://localhost:9902/",1507296079730027018}): {ok,ok}
2017-10-06 14:49:33.185 [debug] <0.599.0>@aec_sync:do_forward_block:265 send_block Res ({peer,"http://localhost:9903/",1507296079729714151}): {ok,ok}
```
... and something like the following in the logs of the other two nodes:
```
2017-10-06 14:49:33.177 [info] <0.1491.0> Attempt to process operation: 'PostBlock'
2017-10-06 14:49:33.177 [debug] <0.1491.0>@aehttp_dispatch_ext:handle_request:89 'PostBlock'; header hash: <<159,132,193,233,50,163,141,172,136,74,50,5,46,67,121,99,165,101,40,115,170,112,121,21,8,121,238,132,56,122,150,212>>
2017-10-06 14:49:33.178 [debug] <0.1491.0>@aehttp_dispatch_ext:handle_request:100 write_block result: ok
```

6. Tail mining-specific logs to see mining progress
    ```
    tail -f log/epoch_mining.log
    ```

For example, as each attempt to mine the next block does not always succeed, each 10 attempts you shall see something like the following in any of the 3 nodes:
```
2017-10-06 13:36:36.768 [info] <0.667.0>@aec_miner:running:163 Failed to mine block in 10 attempts, retrying.
```

When a node manages to mine to block, you shall see something like the following on that node:
```
2017-10-06 14:49:33.145 [info] <0.667.0>@aec_miner:running:148 Block inserted: Height = 1
```

When a node managed to mine a block but such block is already obsolete because the local view of the longest chain moved ahead, you shall see something like the following on that node:
```
2017-10-06 14:55:38.833 [error] <0.667.0>@aec_miner:running:157 Header insertion failed: {previous_hash_is_not_top,{top_header,{header,1,<<187,180,105,28,188,229,33,158,42,170,159,167,5,88,27,71,239,65,60,137,90,246,23,39,9,221,215,191,241,248,189,71>>,<<191,174,187,89,108,27,140,42,105,221,167,30,148,41,174,167,139,170,47,89,148,212,116,169,233,137,155,145,130,127,55,35>>,<<92,56,191,177,109,146,202,185,102,235,250,36,241,141,84,92,100,4,227,220,239,5,184,47,28,115,207,92,232,216,81,30>>,553713663,1125596692,1507300094807,1,[21444,6586407,9734926,17062307,19214768,19457624,20388691,22871531,22915558,35146174,37582353,41094493,50512682,50948743,52048504,54359502,58783324,67452260,67905997,72675145,76864212,78278965,80449837,81328841,91880678,93765245,95533733,97720772,101588269,102818356,102936000,107595015,107815273,110989552,112066712,113045125,114172277,115303768,118573306,125832620,127431107,128332518]}}}.
```

7. Please remember to stop all 3 nodes when your experiments are done
    ```
    bin/epoch stop
    ```

