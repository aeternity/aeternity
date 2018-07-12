# coding: utf-8

from __future__ import absolute_import

import tempfile
import os
import shutil
import logging

from swagger_client.rest import ApiException
from swagger_client.api.external_api import ExternalApi
from swagger_client.api.internal_api import InternalApi
from swagger_client.api_client import ApiClient
from swagger_client.models.balance import Balance
from swagger_client.configuration import Configuration

from nose.tools import assert_equals
from testconfig import config
from waiting import wait

import base58
import rlp

EXT_API = {}
for node, node_config in config['nodes'].iteritems():
    empty_config = Configuration()
    empty_config.host = node_config['host'] + ':' + str(node_config['ports']['external_api']) + '/v2'
    EXT_API[node] = ExternalApi(ApiClient(empty_config))

INT_API = {}
for node, node_config in config['nodes'].iteritems():
    empty_config = Configuration()
    empty_config.host = node_config['host'] + ':' + str(node_config['ports']['internal_api']) + '/v2'
    INT_API[node] = InternalApi(ApiClient(empty_config))

def external_api(name):
    return EXT_API[name]

def internal_api(name):
    return INT_API[name]

def node_online(ext_api, int_api):
    def is_ext_online():
        try:
            top = ext_api.get_top_block()
            return top.height > -1
        except Exception as e:
            return False
    def is_int_online():
        try:
            key = int_api.get_pub_key()
            return key.pub_key is not None
        except Exception as e:
            return False
    return is_ext_online() and is_int_online()

def setup_node(node):
    # prepare a dir to hold the configs and the keys
    root_dir = tempfile.mkdtemp()

    # setup the dir with non-mining node
    user_config = make_no_mining_user_config(root_dir, "epoch.yaml")
    start_node(node, user_config)
    api = external_api(node)

    return (root_dir, node, api)

def setup_node_with_tokens(node, blocks_to_mine):
    # prepare a dir to hold the configs and the keys
    root_dir = tempfile.mkdtemp()

    key_dir = _copy_sign_keys(root_dir, node)

    # setup the dir with mining node
    user_config = make_mining_user_config(root_dir, key_dir, "epoch.yaml")
    start_node(node, user_config)
    api = external_api(node)

    # populate the chain so node had mined some blocks and has tokens
    # to spend
    wait_until_height(api, blocks_to_mine)
    top = api.get_top_block()
    assert_equals(top.height >= blocks_to_mine, True)
    # Now the node has at least blocks_to_mine blocks mined

    return (root_dir, node, api, top)

def _copy_sign_keys(root_dir, keys):
    # Copy the right keys
    curr_dir = os.getcwd()
    key_dir  = os.path.join(root_dir, keys)
    os.makedirs(key_dir)
    shutil.copy(os.path.join(curr_dir, "tests", "sign_keys", keys, "sign_key"), key_dir)
    shutil.copy(os.path.join(curr_dir, "tests", "sign_keys", keys, "sign_key.pub"), key_dir)
    return key_dir

def install_user_config(root_dir, file_name, conf):
    user_config = os.path.join(root_dir, file_name)
    f = open(user_config, "w")
    f.write(conf)
    f.close()
    return user_config

def make_no_mining_user_config(root_dir, file_name):
    conf = """\
---
chain:
    hard_forks:
        "17": 0

mining:
    autostart: false
    expected_mine_rate: 100
    beneficiary: "ak$2QLChDdERfod9QajLkCTsJnYP3RNqZJmAFWQWQZWr99fSrC55h"
    cuckoo:
        miner:
            executable: mean16s-generic
            extra_args: "-t 5"
            node_bits: 16
"""
    return install_user_config(root_dir, file_name, conf)

def make_mining_user_config(root_dir, key_dir, file_name):
    conf = """\
---
chain:
    hard_forks:
        "17": 0
keys:
    dir: "{}"

mining:
    autostart: true
    expected_mine_rate: 100
    # Beneficiary matches pubkey from sign_keys/dev1/sign_key.pub
    beneficiary: "ak$28qVPdhuiaKZTtSgqovgLCvHDZoLxv8PpdVy1cfcAo71Uw5Nva"
    cuckoo:
        miner:
            executable: mean16s-generic
            extra_args: "-t 5"
            node_bits: 16
""".format(key_dir)
    return install_user_config(root_dir, file_name, conf)

def start_node(name, config_filename=None):
    if should_start_node(name):
        print("\nNode " + name + " starting")
        config_prefix = ""
        if config_filename != None:
            if config_filename[0] == "/": # absolute path
                config_prefix =  'EPOCH_CONFIG="' + config_filename + '" '
            else:
                config_prefix =  'EPOCH_CONFIG="`pwd`/' + config_filename + '" '

        print("Starting node with config prefix " + config_prefix)
        p = os.popen("(cd .. && " + config_prefix + "make " + name + "-start;)","r")
        while 1:
            line = p.readline()
            if not line: break
        ext_api = external_api(name)
        int_api = internal_api(name)
        wait(lambda: node_online(ext_api, int_api), timeout_seconds=10, sleep_seconds=0.5)

def stop_node(name):
    if should_start_node(name):
        print("Node " + name + " stopping")
        p = os.popen("(cd .. && make " + name + "-stop;)","r")
        while 1:
            line = p.readline()
            if not line: break

def coinbase_reward():
    return config["coinbase_reward"]

def should_start_node(name):
    return config['nodes'][name]['start']

def node_config(name):
    return config['nodes'][name]

def test_settings(test_name):
    return config['tests'][test_name]

def tool_settings(test_name):
    return config['tools'][test_name]

def genesis_hash(api):
    top = api.get_top_block()
    if top.height == 0:
        return top.hash
    block = api.get_block_by_hash(top.hash)
    while block.height != 1:
        block = api.get_block_by_hash(block.prev_hash)
    return block.prev_hash

def wait_until_height(api, height):
    wait(lambda: api.get_top_block().height >= height, timeout_seconds=120, sleep_seconds=0.25)

def get_account_balance(api, int_api, pub_key=None):
    return _balance_from_get_account_balance(
        lambda: api.get_account_balance(_node_pub_key(int_api, pub_key)))

def get_account_balance_at_height(api, int_api, height, pub_key=None):
    return _balance_from_get_account_balance(
        lambda: api.get_account_balance(_node_pub_key(int_api, pub_key),
                                        height=height))

def _node_pub_key(int_api, k):
    return k if k is not None else int_api.get_pub_key().pub_key

def _balance_from_get_account_balance(get_account_balance_fun):
    balance = Balance(balance=0)
    try:
        balance = get_account_balance_fun()
    except ApiException as e:
        assert_equals(e.status, 404) # no account yet
    return balance

def base58_decode(encoded):
    if encoded[2] != '$':
        raise ValueError('Invalid hash')
    return base58.b58decode_check(encoded[3:])

def encode_signed_tx(encoded_tx, signatures):
    tag = bytearray([11])
    vsn = bytearray([1])
    payload = rlp.encode([tag, vsn, signatures, encoded_tx])
    return "tx$" + base58.b58encode_check(payload)

def decode_unsigned_tx(encoded_tx):
    decoded = rlp.decode(encoded_tx)
    tag, vsn, fields = decoded[0], decoded[1], decoded[2:]
    # This is minimally what we need for now
    if (tag == bytes(bytearray([42])) and vsn == bytes(bytearray([1]))):
        ownerid = decode_id(fields[0])
        return {'type': 'contract_create_tx',
                'owner': ownerid['pubkey'],
                'nonce': bytes_to_int(fields[1]),
                'code': fields[2],
                'vm_version': bytes_to_int(fields[3]),
                'fee': bytes_to_int(fields[4]),
                'ttl': bytes_to_int(fields[5]),
                'deposit': bytes_to_int(fields[6]),
                'amount': bytes_to_int(fields[7]),
                'gas': bytes_to_int(fields[8]),
                'gas_price': bytes_to_int(fields[9]),
                'call_data': fields[10]
        }

def decode_id(encoded):
    return { 'type': encoded[0],
             'pubkey': encoded[1:]
    }

def bytes_to_int(x):
    return int(x.encode('hex'), 16)

def encode_pubkey(pubkey):
    str = base58.b58encode_check(pubkey)
    return "ak$" + str

def encode_name(name):
    str = base58.b58encode_check(name)
    return "nm$" + str

def encode_tx_hash(txhash):
    str = base58.b58encode_check(txhash)
    return "th$" + str

logging.getLogger("urllib3").setLevel(logging.ERROR)
