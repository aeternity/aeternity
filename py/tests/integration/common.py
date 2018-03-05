# coding: utf-8

from __future__ import absolute_import

import os
import sys
import unittest
import datetime
import time
import base64
import logging

from swagger_client.rest import ApiException
from swagger_client.api.external_api import ExternalApi
from swagger_client.api.internal_api import InternalApi
from swagger_client.api_client import ApiClient
from swagger_client.models.block import Block
from swagger_client.models.balance import Balance 
from swagger_client.models.pub_key import PubKey
from swagger_client.configuration import Configuration

from nose.tools import assert_equals
from testconfig import config
from waiting import wait

import msgpack
import base58

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

def node_online(api):
    try:
        top = api.get_top()
        return top.height > -1
    except Exception as e:
        return False

def start_node(name, config_filename=None):
    if should_start_node(name):
        print("\nNode " + name + " starting")
        config_prefix = ""
        if config_filename != None:
            if config_filename[0] == "/": # absolute path
                config_prefix =  'ERL_FLAGS="-config ' + config_filename + '" ' 
            else:
                config_prefix =  'ERL_FLAGS="-config `pwd`/' + config_filename + '" ' 

        p = os.popen("(cd .. && " + config_prefix + "make " + name + "-start;)","r")
        while 1:
            line = p.readline()
            if not line: break
        api = external_api(name)
        wait(lambda: node_online(api), timeout_seconds=10, sleep_seconds=0.5)

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
    top = api.get_top()
    if top.height == 0:
        return top.hash
    block = api.get_block_by_hash(top.hash)
    while block.height != 1:
        block = api.get_block_by_hash(block.prev_hash)
    return block.prev_hash

def wait_until_height(api, height):
    wait(lambda: api.get_top().height >= height, timeout_seconds=120, sleep_seconds=0.25)

def get_account_balance(int_api, pub_key=None):
    balance = Balance(balance=0)
    try:
        if pub_key == None:
            pub_key_obj = int_api.get_pub_key()
            balance = int_api.get_account_balance(pub_key_obj.pub_key)
        else:
            balance = int_api.get_account_balance(pub_key)
    except ApiException as e:
        assert_equals(e.status, 404) # Alice has no account yet
    return balance

def base58_decode(encoded):
    if encoded[2] != '$':
        raise ValueError('Invalid hash')
    return base58.b58decode_check(encoded[3:])

def encode_signed_tx(encoded_tx, signatures):
    str = base58.b58encode_check(msgpack.packb(["sig_tx", 1, encoded_tx, signatures], use_bin_type=True))
    return "tx$" + str

def encode_pubkey(pubkey):
    str = base58.b58encode_check(pubkey)
    return "ak$" + str

def encode_name(name):
    str = base58.b58encode_check(name)
    return "nm$" + str

def unpack_tx(tx):
    return msgpack.unpackb(tx)

def parse_tx(unpacked_tx):
    tx = {}
    for elem in unpacked_tx:
        tx.update(elem)
    return tx

logging.getLogger("urllib3").setLevel(logging.ERROR)
