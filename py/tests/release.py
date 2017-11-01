import os
import getopt
import sys
import tempfile
import time
import unittest
import argparse
import urllib3
import shutil

import swagger_client
from swagger_client.rest import ApiException
from swagger_client.apis.external_api import ExternalApi
from swagger_client.api_client import ApiClient

SETUP = {
        "node1": {
            "host": "localhost:9900/v1",
            "commands": [
                "sed -ibkp 's/-sname epoch/-sname epoch1/g' ./releases/0.2.0/vm.args"
                ],
            "config": '''[
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
'''
                },
        "node2": {
            "host": "localhost:9902/v1",
            "commands": [
                "sed -ibkp 's/-sname epoch/-sname epoch2/g' ./releases/0.2.0/vm.args"
                ],
            "config": '''[
{aecore,
 [
  {peers, []}
 ]},
{aehttp,
 [
  {swagger_port_external, 9902}
 ]}
].
'''
                },
        "node3": {
            "host": "localhost:9903/v1",
            "commands": [
                "sed -ibkp 's/-sname epoch/-sname epoch3/g' ./releases/0.2.0/vm.args"
                ],
            "config": '''[
{aecore,
 [
  {peers, []}
 ]},
{aehttp,
 [
  {swagger_port_external, 9903}
 ]}
].
'''
                }
        }

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

def read_argv(argv):
    parser = argparse.ArgumentParser(description='Integration test a potential release')
    parser.add_argument('--maxheight', type=int, default=10,
                        help='Number of blocks to mine')
    parser.add_argument('--tarball', required=True, 
                        help='Release package tarball')

    args = parser.parse_args()
    tar_file_name = args.tarball
    max_height = args.maxheight
    return (tar_file_name, max_height)

def tail_logs(temp_dir, log_name):
    n = 200 # last 200 lines
    f = os.path.join(temp_dir, "log", log_name)
    stdin, stdout = os.popen2("tail -n "+ str(n) + " " + f)
    stdin.close()
    lines = "\n".join(stdout.readlines())
    stdout.close()
    return lines


def setup_node(node, path):
    os.chdir(path)
    for command in SETUP[node]["commands"]:
        os.system(command)
    file_obj = open(os.path.join(path, "p.config"), "w")
    file_obj.write(SETUP[node]["config"])
    file_obj.close()

def main(argv):
    tar_file_name, max_height = read_argv(argv)
    root_dir = tempfile.mkdtemp()
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
    [setup_node(n, d) for n, d in zip(node_names, node_dirs)]
    [start_node(d) for d in node_dirs]

    time.sleep(1)
    node_objs = [ExternalApi(ApiClient(host=SETUP[n]["host"])) for n in node_names]
    height = 0
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
    if test_failed:
        for name, node_dir in zip(node_names, node_dirs):
            print(name + " logs:")
            print(tail_logs(node_dir, "epoch.log"))
            print("\n")
    shutil.rmtree(root_dir)
    if test_failed:
        sys.exit("FAILED")	

if __name__ == "__main__":
    main(sys.argv)
