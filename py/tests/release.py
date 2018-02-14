import os
import getopt
import sys
import time
import unittest
import argparse
import urllib3
import shutil
import pystache
import logging
from waiting import wait

import swagger_client
from swagger_client.rest import ApiException
from swagger_client.api.external_api import ExternalApi
from swagger_client.api_client import ApiClient
from swagger_client.configuration import Configuration

# these are executed for every node
NODE_SETUP_COMMANDS = [
        "sed -ibkp 's/-sname epoch/-sname {{ name }}/g' ./releases/{{ version }}/vm.args"
        ]
# node's setup
SETUP = {
        "node1": {
            "host": "localhost:9813/v2",
            "name": "epoch1",
            "user_config": '''
---
peers:
    - "http://localhost:9823/"
    - "http://localhost:9833/"

http:
    external:
        port: 9813
    internal:
        port: 9913

websocket:
    internal:
        port: 9914
        acceptors: 100

mining:
    cuckoo:
        miner:
            executable: mean16s-generic
            extra_args: "-t 5"
            node_bits: 16

chain:
    persist: true
    db_path: ./my_db
''',
            "config": '''[
{aecore,
 [
  {expected_mine_rate, 100}
 ]},
{aehttp,
 [
  {internal, [
      {websocket, [ {tasks, 200}
                ]}
    ]}
 ]}
].
'''
                },
        "node2": {
            "host": "localhost:9823/v2",
            "name": "epoch2",
            "user_config": '''
---
http:
    external:
        port: 9823
    internal:
        port: 9923

websocket:
    internal:
        port: 9924
        acceptors: 100

mining:
    cuckoo:
        miner:
            executable: mean16s-generic
            extra_args: "-t 5"
            node_bits: 16

chain:
    persist: true
    db_path: ./my_db
''',
            "config": '''[
{aecore,
 [
  {expected_mine_rate, 100}
 ]},
{aehttp,
 [
  {internal, [
      {websocket, [ {tasks, 200}
                ]}
    ]}
 ]}
].
'''
                },
        "node3": {
            "host": "localhost:9833/v2",
            "name": "epoch3",
            "user_config": '''
---
http:
    external:
        port: 9833
    internal:
        port: 9933

websocket:
    internal:
        port: 9934
        acceptors: 100

mining:
    cuckoo:
        miner:
            executable: mean16s-generic
            extra_args: "-t 5"
            node_bits: 16

chain:
    persist: true
    db_path: ./my_db
''',
            "config": '''[
{aecore,
 [
  {expected_mine_rate, 100}
 ]},
{aehttp,
 [
  {internal, [
      {websocket, [ {tasks, 200}
                ]}
    ]}
 ]}
].
'''
                }
        }

def node_is_online(api):
    try:
        top = api.get_top()
        return top.height > -1
    except Exception as e:
        return False

def wait_all_nodes_are_online(apis):
    wait(lambda: all([node_is_online(api) for api in apis]), timeout_seconds=30, sleep_seconds=0.5)

def executable(temp_dir):
    return os.path.join(temp_dir, "bin", "epoch")

def extract_tarball(tarball_name, temp_dir):
    print("Extracting tar to " + temp_dir)
    os.system("tar xC " + temp_dir + " -f " + tarball_name)

def stop_node(temp_dir):
    print("Stopping " + temp_dir)
    os.system(executable(temp_dir) + " stop")

def start_node(temp_dir):
    binary = executable(temp_dir) 
    assert os.path.isfile(binary)
    assert os.access(binary, os.X_OK)
    print("Starting " + binary)
    os.chdir(temp_dir)
    os.system('ERL_FLAGS="-config `pwd`/p.config" bin/epoch start')

def eval_on_node(temp_dir, quoted_code):
    binary = executable(temp_dir)
    assert os.path.isfile(binary)
    assert os.access(binary, os.X_OK)
    cmd = binary + " eval " + quoted_code
    print("Evaluating " + cmd)
    os.chdir(temp_dir)
    return os.system(cmd)

def existing_empty_dir(s):
    if s == "":
        msg = "{} is not a non-empty directory path".format(s)
        raise argparse.ArgumentTypeError(msg)
    v = os.path.abspath(s)
    if not os.path.isdir(v):
        msg = ("Path {} is not an existing directory "
               "(path absolutized from {})").format(v, s)
        raise argparse.ArgumentTypeError(msg)
    ls = os.listdir(v)
    if ls:
        msg = ("Path {} is not an empty directory "
               "because it contains {} entries i.e. {}"
               "(path absolutized from {})").format(v, len(ls), str(ls), s)
        raise argparse.ArgumentTypeError(msg)
    return v

def read_argv(argv):
    parser = argparse.ArgumentParser(description='Integration test a potential release')
    parser.add_argument('--workdir', type=existing_empty_dir, required=True,
                        help='Working directory for testing. It must exist and be empty.')
    parser.add_argument('--blocks', type=int, default=10,
                        help='Number of blocks to mine')
    parser.add_argument('--tarball', required=True, 
                        help='Release package tarball')

    parser.add_argument('--version', required=True, 
                        help='Release package version')

    args = parser.parse_args()
    tar_file_name = args.tarball
    blocks = args.blocks
    return (args.workdir, tar_file_name, blocks, args.version)

def tail_logs(temp_dir, log_name):
    n = 200 # last 200 lines
    f = os.path.join(temp_dir, "log", log_name)
    stdin, stdout = os.popen2("tail -n "+ str(n) + " " + f)
    stdin.close()
    lines = "\n".join(stdout.readlines())
    stdout.close()
    return lines


def setup_node(node, path, version):
    os.chdir(path)
    for command in NODE_SETUP_COMMANDS: 
        os.system(pystache.render(command, {"version": version, \
                                            "name": SETUP[node]["name"]}))
    ucfg = open(os.path.join(path, "epoch.yaml"), "w")
    ucfg.write(SETUP[node]["user_config"])
    ucfg.close()
    cfg = open(os.path.join(path, "p.config"), "w")
    cfg.write(SETUP[node]["config"])
    cfg.close()

def main(argv):
    logging.getLogger("urllib3").setLevel(logging.ERROR)
    root_dir, tar_file_name, blocks_to_mine, version = read_argv(argv)
    temp_dir_dev1 = os.path.join(root_dir, "node1") 
    os.makedirs(temp_dir_dev1)

    temp_dir_dev2 = os.path.join(root_dir, "node2") 
    temp_dir_dev3 = os.path.join(root_dir, "node3") 


    print("Tar name: " + tar_file_name)
    extract_tarball(tar_file_name, temp_dir_dev1)
    shutil.copytree(temp_dir_dev1, temp_dir_dev2)
    shutil.copytree(temp_dir_dev1, temp_dir_dev3)

    node_names = ["node1", "node2", "node3"]
    node_dirs = [temp_dir_dev1, temp_dir_dev2, temp_dir_dev3]
    [setup_node(n, d, version) for n, d in zip(node_names, node_dirs)]
    [start_node(d) for d in node_dirs]


    empty_config = Configuration()
    node_objs = []
    for n in node_names:
        empty_config.host = SETUP[n]["host"]
        node_objs.append(ExternalApi(ApiClient(configuration=empty_config)))

    wait_all_nodes_are_online(node_objs)

    top = node_objs[0].get_top()
    height = top.height
    max_height = blocks_to_mine + height
    test_failed = False
    try:
        print("Will mine till block " +  str(max_height))
        while height < max_height:
            time.sleep(1) # check every second
            for name, node in zip(node_names, node_objs):
                top = node.get_top() # node is alive and mining
                print("[" + name + "] height=" + str(top.height))
                height = max(height, top.height)
            print("")
    except ApiException as e:
        test_failed = True
        print("node died")
    except urllib3.exceptions.MaxRetryError as e:
        test_failed = True
        print("node died")
    [stop_node(d) for d in node_dirs]

    if not test_failed:
        print("Checking that nodes are able to start with persisted non-empty DB")
        [start_node(d) for d in node_dirs]
        wait_all_nodes_are_online(node_objs)
        [stop_node(d) for d in node_dirs]

    if not test_failed:
        print("Checking that emergency patching of OTP modules works: `mnesia:index_read`")
        [start_node(d) for d in node_dirs]
        wait_all_nodes_are_online(node_objs)
        if 0 != eval_on_node(temp_dir_dev1, "'aec_db:transactions_by_account(<<\"FakeAccountPublicKey\">>).'"):
            test_failed = True
            print("Check on `mnesia:index_read` failed")
            [stop_node(d) for d in node_dirs]

    if test_failed:
        for name, node_dir in zip(node_names, node_dirs):
            print(name + " logs:")
            print(tail_logs(node_dir, "epoch.log"))
            print("\n")
    if test_failed:
        sys.exit("FAILED")      

if __name__ == "__main__":
    main(sys.argv)
