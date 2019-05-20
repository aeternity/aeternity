import argparse
import functools
import logging
import os
import pystache
import shutil
import signal
import subprocess
import sys
import time
import urllib3
from waiting import wait

from swagger_client.rest import ApiException
from swagger_client.api.external_api import ExternalApi
from swagger_client.api_client import ApiClient
from swagger_client.configuration import Configuration

# these are executed for every node
NODE_SETUP_COMMANDS = [
        "sed -ibkp 's/-sname aeternity/-sname {{ name }}/g' {{ package_basepath }}/releases/{{ version }}/vm.args"
        ]
# node's setup
SETUP = {
        "node1": {
            "host": "localhost:9813/v2",
            "listen_address": "0.0.0.0:9813",
            "name": "aeternity1",
            "user_config": '''
---
keys:
    dir: "keys"
    peer_password: "top secret"

sync:
    port: 9815
    ping_interval: 15000

peers:
    - aenode://pp_28uQUgsPcsy7TQwnRxhF8GMKU4ykFLKsgf4TwDwPMNaSCXwWV8@localhost:9825
    - aenode://pp_Dxq41rJN33j26MLqryvh7AnhuZywefWKEPBiiYu2Da2vDWLBq@localhost:9835

http:
    external:
        port: 9813
    internal:
        port: 9913

websocket:
    channel:
        port: 9814
        acceptors: 100

mining:
    autostart: true
    beneficiary: "ak_RShHyLiaQJF8AZ7Thi4Sgjm6ncHhqguqBBqCzQRG3fyjvKj6V"
    cuckoo:
        edge_bits: 15
        miners:
            - executable: mean15-generic
              extra_args: ""

chain:
    persist: true
    db_path: ./my_db

fork_management:
    network_id: ae_smoke_test
''',
            "config": [
                "-aecore expected_mine_rate 100"
                ]
                },
        "node2": {
            "host": "localhost:9823/v2",
            "listen_address": "0.0.0.0:9823",
            "name": "aeternity2",
            "user_config": '''
---
keys:
    dir: "keys"
    peer_password: "top secret"

sync:
    port: 9825
    ping_interval: 15000

peers:
    - aenode://pp_Dxq41rJN33j26MLqryvh7AnhuZywefWKEPBiiYu2Da2vDWLBq@localhost:9835

http:
    external:
        port: 9823
    internal:
        port: 9923

websocket:
    channel:
        port: 9824
        acceptors: 100

mining:
    beneficiary: "ak_2WPFUrtoxvdpaMySJUfyhGeBg5o725y6wFJTWAdv9YQ7pJMHjT"
    cuckoo:
        edge_bits: 15
        miners:
            - executable: mean15-generic
              extra_args: ""

chain:
    persist: true
    db_path: ./my_db

fork_management:
    network_id: ae_smoke_test
''',
            "config": [
                "-aecore expected_mine_rate 100"
                ]
                },
        "node3": {
            "host": "localhost:9833/v2",
            "listen_address": "0.0.0.0:9833",
            "name": "aeternity3",
            "user_config": '''
---
keys:
    dir: "keys"
    peer_password: "top secret"

sync:
    port: 9835
    ping_interval: 15000

peers:
    - aenode://pp_28uQUgsPcsy7TQwnRxhF8GMKU4ykFLKsgf4TwDwPMNaSCXwWV8@localhost:9825

http:
    external:
        port: 9833
    internal:
        port: 9933

websocket:
    channel:
        port: 9834
        acceptors: 100

mining:
    beneficiary: "ak_uDBX3LjznjmtoFzVmVWBnAaMXhvsReKYkxMrA1QMSxudhbjuf"
    cuckoo:
        edge_bits: 15
        miners:
            - executable: mean15-generic
              extra_args: ""

chain:
    persist: true
    db_path: ./my_db

fork_management:
    network_id: ae_smoke_test
''',
            "config": [
                "-aecore expected_mine_rate 100"
                ]
                }
        }

def node_is_online(api):
    try:
        top = api.get_top_block()
        return top.key_block.height > -1
    except Exception as e:
        return False

def wait_all_nodes_are_online(apis, timeout_seconds):
    wait(lambda: all([node_is_online(api) for api in apis]), timeout_seconds, sleep_seconds=1)

def wait_all_nodes_are_offline(apis, timeout_seconds):
    wait(lambda: not any([node_is_online(api) for api in apis]), timeout_seconds, sleep_seconds=1)

def executable(temp_dir):
    filename = os.path.join(temp_dir, package_basepath(), "bin", "aeternity")
    if sys.platform == "win32":
        return filename + ".cmd"
    return filename

def extract_package(package_name, temp_dir):
    ext = os.path.splitext(package_name)[1]
    if ext == ".zip":
        extract_ziparchive(package_name, temp_dir)
    elif ext == ".gz":
        extract_tarball(package_name, temp_dir)
    else:
        sys.exit("ERROR: Unsupported package file format " + ext)

def extract_tarball(tarball_name, temp_dir):
    print("Extracting tar to " + temp_dir)
    subprocess.call("tar -xC " + temp_dir + " -f " + tarball_name, shell=True)

def extract_ziparchive(zip_name, temp_dir):
    print("Extracting zip to " + temp_dir)
    subprocess.call("unzip -q " + zip_name + " -d " + temp_dir, shell=True)

def find_pid(line, listen_address, acc):
    parts = [p for p in line.split(' ') if not p == '']
    if not len(parts) == 5:
        return acc
    if not parts[1] == listen_address:
        return acc
    return parts[4]

def stop_node(node, temp_dir):
    binary = executable(temp_dir)
    assert os.path.isfile(binary), "Not a file: " + binary
    assert os.access(binary, os.X_OK), "Can't access file: " + binary
    print("Stopping " + binary)
    os.chdir(temp_dir)
    if sys.platform == "win32":
        # We need to find the PID listening on the right port
        listen_address = SETUP[node]["listen_address"]
        result_lines = subprocess.check_output("netstat -ano -p TCP").decode("utf-8").split("\r\n")
        pid = functools.reduce(lambda acc, r: find_pid(r, listen_address, acc), result_lines, "")
        os.kill(int(pid), signal.SIGTERM)
    else:
        return subprocess.call(binary + " stop", shell=True)

def start_node(node, temp_dir):
    binary = executable(temp_dir)
    assert os.path.isfile(binary), "Not a file: " + binary
    assert os.access(binary, os.X_OK), "Can't access file: " + binary
    print("Starting " + binary)
    os.chdir(temp_dir)
    erl_flags = " ".join(SETUP[node]["config"])
    if sys.platform == "win32":
        sub_env = os.environ
        sub_env["ERL_FLAGS"] = erl_flags
        subprocess.Popen([binary, "console"], env=sub_env)
    else:
        subprocess.call('ERL_FLAGS="' + erl_flags + '" ' + binary + ' start', shell=True)

def eval_on_node(temp_dir, quoted_code):
    binary = executable(temp_dir)
    assert os.path.isfile(binary), "Not a file: " + binary
    assert os.access(binary, os.X_OK), "Can't access file: " + binary
    cmd = binary + " eval " + quoted_code
    print("Evaluating " + cmd)
    os.chdir(temp_dir)
    return subprocess.call(cmd, shell=True)

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
    parser.add_argument('--package', required=True,
                        help='Release package archive (.tar.gz for linux and macos, .zip for win32)')

    parser.add_argument('--version', required=True,
                        help='Release package version')

    args = parser.parse_args()
    package_file_name = args.package
    blocks = args.blocks
    return (args.workdir, package_file_name, blocks, args.version)

def tail_logs(temp_dir, log_name):
    n = 200 # last 200 lines
    f = os.path.join(temp_dir, "log", log_name)
    stdin, stdout = os.popen2("tail -n "+ str(n) + " " + f)
    stdin.close()
    lines = "\n".join(stdout.readlines())
    stdout.close()
    return lines


def setup_node(node, path, test_dir, version):
    os.chdir(path)
    os.makedirs("keys")
    shutil.copy(os.path.join(test_dir, "tests", "peer_keys", node, "peer_key"), "keys")
    shutil.copy(os.path.join(test_dir, "tests", "peer_keys", node, "peer_key.pub"), "keys")

    for command in NODE_SETUP_COMMANDS:
        subprocess.call(pystache.render(command, {"version": version, \
                                                  "package_basepath": package_basepath(), \
                                                  "name": SETUP[node]["name"]}), shell=True)
    ucfg = open(os.path.join(path, "aeternity.yaml"), "w")
    ucfg.write(SETUP[node]["user_config"])
    ucfg.close()

def package_basepath():
    if sys.platform == "win32":
        return os.path.join("usr", "lib", "aeternity")
    return "."

def main(argv):
    logging.getLogger("urllib3").setLevel(logging.ERROR)
    root_dir, package_file_name, blocks_to_mine, version = read_argv(argv)
    curr_dir = os.getcwd()
    temp_dir_dev1 = os.path.join(root_dir, "node1")
    os.makedirs(temp_dir_dev1)

    temp_dir_dev2 = os.path.join(root_dir, "node2")
    temp_dir_dev3 = os.path.join(root_dir, "node3")


    print("Package name: " + package_file_name)
    extract_package(package_file_name, temp_dir_dev1)
    shutil.copytree(temp_dir_dev1, temp_dir_dev2)
    shutil.copytree(temp_dir_dev1, temp_dir_dev3)

    node_names = ["node1", "node2", "node3"]
    node_dirs = [temp_dir_dev1, temp_dir_dev2, temp_dir_dev3]
    [setup_node(n, d, curr_dir, version) for n, d in zip(node_names, node_dirs)]
    [start_node(n, d) for n, d in zip(node_names, node_dirs)]

    node_objs = []
    for n in node_names:
        empty_config = Configuration()
        empty_config.host = SETUP[n]["host"]
        node_objs.append(ExternalApi(ApiClient(configuration=empty_config)))

    wait_all_nodes_are_online(node_objs, 30)

    top = node_objs[0].get_top_block()
    height = top.key_block.height
    max_height = blocks_to_mine + height
    test_failed = False
    try:
        print("Will mine (at least) until block " +  str(max_height))
        sync_height = 0
        while sync_height < max_height:
            time.sleep(1) # check every second
            sync_height = max_height
            for name, node in zip(node_names, node_objs):
                top = node.get_top_block() # node is alive and mining
                print("[" + name + "] height=" + str(top.key_block.height))
                sync_height = min(sync_height, top.key_block.height)
            print("")
    except ApiException as e:
        test_failed = True
        print("node died")
    except urllib3.exceptions.MaxRetryError as e:
        test_failed = True
        print("node died")
    [stop_node(n, d) for n, d in zip(node_names, node_dirs)]
    wait_all_nodes_are_offline(node_objs, 10)

    if not test_failed:
        print("Checking that nodes are able to start with persisted non-empty DB")
        [start_node(n, d) for n, d in zip(node_names, node_dirs)]
        wait_all_nodes_are_online(node_objs, 60)
        [stop_node(n, d) for n, d in zip(node_names, node_dirs)]
        wait_all_nodes_are_offline(node_objs, 10)

    if test_failed:
        for name, node_dir in zip(node_names, node_dirs):
            print(name + " logs:")
            print(tail_logs(node_dir, "aeternity.log"))
            print("\n")
    if test_failed:
        sys.exit("FAILED")

if __name__ == "__main__":
    main(sys.argv)
