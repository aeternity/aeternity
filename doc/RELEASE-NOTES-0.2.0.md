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
https://github.com/aeternity/epoch/blob/4d1baf36207dfeff2d2f1a5256d37285574b7457/doc/CUSTOMIZATION.md

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
In this case, assuming nodes 2 and 3 are already up, stop and start node 1.

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

6. Tail mining-specific logs to see mining progress
    ```
    tail -f log/epoch_mining.log
    ```

For example, as each attempt to mine the next block does not always succeed, each 10 attempts you shall see something like the following in any of the 3 nodes:
```
2017-10-06 13:36:36.768 [info] <0.667.0>@aec_miner:running:163 Failed to mine block in 10 attempts, retrying.
```

7. Please remember to stop all 3 nodes when your experiments are done
    ```
    bin/epoch stop
    ```
