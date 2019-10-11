# coding: utf-8

from __future__ import absolute_import

import tempfile
import os
import shutil
import logging
import base64
import nacl.encoding
import subprocess

from bravado.client import SwaggerClient
from bravado.exception import HTTPNotFound
from requests.exceptions import ConnectionError

from nose.tools import assert_equals
from nose.tools import assert_true
from testconfig import config
from waiting import wait

from nacl.hash import sha256

import base58
import rlp
import integration.keys

EXT_API = {}
INT_API = {}

def create_client(node_name, port):
    node_config = config['nodes'][node_name]
    url = 'http://' + node_config['host'] + ':' + str(node_config['ports'][port]) + '/api'
    client_config = {'validate_responses': False}
    return SwaggerClient.from_url(url, config=client_config)

def external_api(name):
    if not name in EXT_API:
        client = create_client(name, 'external_api')
        api = client.external
        def get_model(model):
            return client.get_model(model)

        api.get_model = get_model
        EXT_API[name] = api

    return EXT_API[name]

def internal_api(name):
    if not name in INT_API:
        client = create_client(name, 'internal_api')
        api = client.internal
        def get_model(model):
            return client.get_model(model)

        api.get_model = get_model
        INT_API[name] = api

    return INT_API[name]

def node_online(name):
    def is_ext_online():
        try:
            ext_api = external_api(name)
            top = ext_api.GetCurrentKeyBlock().response().result
            return top.height > -1
        except ConnectionError as e:
            return False
    def is_int_online():
        try:
            int_api = internal_api(name)
            key = int_api.GetNodePubkey().response().result
            return key.pub_key is not None
        except ConnectionError as e:
            return False
    return is_ext_online() and is_int_online()

def setup_node(node):
    # prepare a dir to hold the configs
    root_dir = tempfile.mkdtemp()

    # setup the dir with non-mining node
    user_config = make_no_mining_user_config(root_dir, "aeternity.yaml")
    start_node(node, user_config)
    ext_api = external_api(node)
    int_api = internal_api(node)

    return (root_dir, node, ext_api, int_api)

def setup_node_with_tokens(node, beneficiary, blocks_to_mine):
    # prepare a dir to hold the configs
    root_dir = tempfile.mkdtemp()

    # setup the dir with mining node
    user_config = make_mining_user_config(root_dir, beneficiary, "aeternity.yaml")
    start_node(node, user_config)
    ext_api = external_api(node)
    int_api = internal_api(node)

    top0 = ext_api.GetCurrentKeyBlock().response().result
    bal0 = get_account_balance(ext_api, beneficiary['enc_pubk'])

    # populate the chain so node had mined some blocks and has tokens
    # to spend
    wait_until_height(ext_api, top0.height + blocks_to_mine)
    top1 = ext_api.GetCurrentKeyBlock().response().result
    assert_true(top1.height >= top0.height)
    assert_true(top1.height >= blocks_to_mine)

    # Now the node has at least blocks_to_mine blocks mined

    bal1 = get_account_balance(ext_api, beneficiary['enc_pubk'])
    # The node received the reward for at least blocks_to_mine blocks
    assert_true(bal1 > bal0)

    return (root_dir, ext_api, int_api, top1)

def install_user_config(root_dir, file_name, conf):
    user_config = os.path.join(root_dir, file_name)
    f = open(user_config, "w")
    f.write(conf)
    f.close()
    return user_config

def make_no_mining_user_config(root_dir, file_name):
    conf = """\
---
mining:
    autostart: false
    expected_mine_rate: 100

    beneficiary: "ak_2QLChDdERfod9QajLkCTsJnYP3RNqZJmAFWQWQZWr99fSrC55h"
    beneficiary_reward_delay: 2
    cuckoo:
        edge_bits: 15
        miners:
            - executable: mean15-generic
              extra_args: ""
"""
    return install_user_config(root_dir, file_name, conf)

def make_mining_user_config(root_dir, beneficiary, file_name):
    conf = """\
---
mining:
    autostart: true
    expected_mine_rate: 100
    beneficiary: "{}"
    beneficiary_reward_delay: 2
    cuckoo:
        edge_bits: 15
        miners:
            - executable: mean15-generic
              extra_args: ""
""".format(beneficiary['enc_pubk'])
    return install_user_config(root_dir, file_name, conf)

def start_node(name, config_filename):
    if should_start_node(name):
        print("\nNode " + name + " starting")
        config_prefix = ""
        if config_filename[0] == "/": # absolute path
            config_prefix =  'AETERNITY_CONFIG="' + config_filename + '" '
        else:
            config_prefix =  'AETERNITY_CONFIG="`pwd`/' + config_filename + '" '

        print("Starting node with config prefix " + config_prefix)
        p = os.popen("(cd ../.. && " + config_prefix + "make " + name + "-start;)","r")
        while 1:
            line = p.readline()
            if not line: break

        wait(lambda: node_online(name), timeout_seconds=30, sleep_seconds=0.5)

def stop_node(name):
    if should_start_node(name):
        print("Node " + name + " stopping")
        p = os.popen("(cd ../.. && make " + name + "-stop;)","r")
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

def wait_until_height(ext_api, height):
    wait(lambda: ext_api.GetCurrentKeyBlock().response().result.height >= height, timeout_seconds=120, sleep_seconds=0.25)

def post_transaction(ext_api, signed_tx):
    Tx = ext_api.get_model('Tx')
    tx_object = Tx(tx=signed_tx)
    return ext_api.PostTransaction(body=tx_object).response().result.tx_hash

def ensure_transaction_posted(ext_api, signed_tx, min_confirmations=1):
    tx_hash = post_transaction(ext_api, signed_tx)
    ensure_transaction_confirmed(ext_api, tx_hash, min_confirmations)

def ensure_transaction_confirmed(ext_api, tx_hash, min_confirmations):
    wait(lambda: is_tx_confirmed(ext_api, tx_hash, min_confirmations), timeout_seconds=20, sleep_seconds=0.25)

def is_tx_confirmed(ext_api, tx_hash, min_confirmations):
    res = ext_api.GetCurrentKeyBlockHeight().response().result
    top_key_height = res['height']
    tx = ext_api.GetTransactionByHash(hash=tx_hash).response().result
    if "none" == tx.block_hash:
        return False
    return (top_key_height - tx.block_height) >= min_confirmations

def get_account_balance(api, pub_key):
    AccountModel = api.get_model('Account')
    return _balance_from_get_account(AccountModel, lambda: api.GetAccountByPubkey(pubkey=pub_key).response().result, pub_key)

def send_tokens(sender, address, tokens, fee, ext_api, int_api):
    SpendTx = int_api.get_model('SpendTx')
    spend_tx_obj = SpendTx(
        sender_id=sender['enc_pubk'],
        recipient_id=address,
        amount=tokens,
        fee=fee,
        ttl=100,
        payload="sending tokens"
    )
    spend_tx = int_api.PostSpend(body=spend_tx_obj).response().result.tx
    unsigned_tx = api_decode(spend_tx)
    signed_tx = integration.keys.sign_encode_tx(unsigned_tx, sender['privk'])
    return post_transaction(ext_api, signed_tx)

def ensure_send_tokens(sender, address, tokens, fee, ext_api, int_api, min_confirmations):
    tx_hash = send_tokens(sender, address, tokens, fee, ext_api, int_api)
    ensure_transaction_confirmed(ext_api, tx_hash, min_confirmations)

def _balance_from_get_account(AccountModel, get_account_fun, pub_key):
    account = AccountModel(id=pub_key, balance=0, nonce=0)
    try:
        account = get_account_fun()
    except HTTPNotFound as e:
        assert_equals(e.status_code, 404) # no account yet
    return account.balance

def api_decode(encoded):
    if encoded[2] != '_':
        raise ValueError('Invalid hash')
    prefix = encoded[0:2]
    if api_encode_type(prefix) == 64:
        return base64decode_check(encoded[3:])
    return base58.b58decode_check(encoded[3:])

def api_encode(prefix, decoded):
    if api_encode_type(prefix) == 64:
        return prefix + '_' + base64encode_check(decoded)
    return prefix + '_' + base58.b58encode_check(decoded)

def api_encode_type(prefix):
    if len(prefix) != 2:
        raise ValueError('Invalid prefix: ' + prefix)
    base64Prefixes = {'cb', 'tx', 'ov', 'or', 'st', 'pi', 'ss'}
    if prefix in base64Prefixes:
        return 64
    return 58

def base64decode_check(encoded):
    decoded = base64.b64decode(encoded)
    check = decoded[-4:]
    body = decoded[:-4]
    shaHash = sha256(sha256(body, encoder=nacl.encoding.RawEncoder), encoder=nacl.encoding.RawEncoder)
    if shaHash[0:4] != check:
        raise ValueError('Invalid hash')
    return body

def base64encode_check(decoded):
    shaHash = sha256(sha256(decoded, encoder=nacl.encoding.RawEncoder), encoder=nacl.encoding.RawEncoder)
    return base64.b64encode(decoded + shaHash[0:4])

def hexstring_to_contract_bytearray(hexstring):
    if (hexstring.startswith("0x")):
        hexstring = hexstring[2:]
    return "cb_" + base64encode_check(hexstring.decode("hex"))

def encode_signed_tx(encoded_tx, signatures):
    tag = bytearray([11])
    vsn = bytearray([1])
    payload = rlp.encode([tag, vsn, signatures, encoded_tx])
    return api_encode("tx", payload)

def decode_unsigned_tx(encoded_tx):
    decoded = rlp.decode(encoded_tx)
    tag, vsn, fields = decoded[0], decoded[1], decoded[2:]
    # This is minimally what we need for now
    if (tag == bytes(bytearray([42])) and vsn == bytes(bytearray([1]))):
        ownerid = decode_id(fields[0])
        return {'type': 'contract_create_tx',
                'owner_id': ownerid['pubkey'],
                'nonce': bytes_to_int(fields[1]),
                'code': fields[2],
                'vm_version': bytes_to_int(fields[3]) >> 16,
                'abi_version': bytes_to_int(fields[3]) & 65535,
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
    return api_encode("ak", pubkey)

def encode_name(name):
    return api_encode("nm", name)

def encode_tx_hash(txhash):
    return api_encode("th", txhash)

def setup_beneficiary():
    ben_priv = integration.keys.new_private()
    ben_pub = integration.keys.public_key(ben_priv)
    beneficiary = {'privk': ben_priv, 'pubk': ben_pub,
           'enc_pubk': integration.keys.address(ben_pub)}
    return beneficiary

def compile_contract(file):
    compiler_cmd = os.path.join(os.getcwd(), 'integration', 'aesophia_cli')
    bytecode = subprocess.check_output([compiler_cmd, file]).splitlines()[-1]
    return bytecode

def encode_calldata(file, function, args):
    compiler_cmd = os.path.join(os.getcwd(), 'integration', 'aesophia_cli')
    calldata = subprocess.check_output([compiler_cmd, '--create_calldata', file, '--calldata_fun',
                                        function, '--calldata_args', args]).splitlines()[-1]
    return calldata

def call_result(source_file, fun, data):
    compiler_cmd = os.path.join(os.getcwd(), 'integration', 'aesophia_cli')
    value = subprocess.check_output([compiler_cmd, '--call_result', data,
                                     '--call_result_fun', fun, source_file]).splitlines()[-1]
    return value
