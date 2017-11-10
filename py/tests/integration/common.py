# coding: utf-8

from __future__ import absolute_import

import os
import sys
import unittest
import datetime
import time
import base64

from py.tests import swagger_client
from py.tests.swagger_client.rest import ApiException
from py.tests.swagger_client.apis.external_api import ExternalApi
from py.tests.swagger_client.apis.internal_api import InternalApi
from py.tests.swagger_client.api_client import ApiClient
from py.tests.swagger_client.models.block import Block
from py.tests.swagger_client.models.signed_tx import SignedTx
from py.tests.swagger_client.models.coinbase_tx import CoinbaseTx
from py.tests import chain_downloader

from nose.tools import assert_equals
from testconfig import config
from waiting import wait

import logging
logging.getLogger("urllib3").setLevel(logging.ERROR)
EXT_API = {}
for node, node_config in config['nodes'].iteritems():
    EXT_API[node] = ExternalApi(ApiClient(host=node_config['host'] + ':'
                + str(node_config['ports']['external_api']) + '/v1'))

INT_API = {}
for node, node_config in config['nodes'].iteritems():
    INT_API[node] = InternalApi(ApiClient(host=node_config['host'] + ':'
                + str(node_config['ports']['internal_api']) + '/v1'))

def external_api(name):
    return EXT_API[name]

def internal_api(name):
    return INT_API[name]

def node_online(api):
    try:
        return api.get_top() > -1
    except Exception as e:
        return False

def start_node(name, config_filename=None):
    config_prefix = ""
    if config_filename != None:
        if config_filename[0] == "/": # absolute path
            config_prefix =  'ERL_FLAGS="-config ' + config_filename + '" ' 
        else:
            config_prefix =  'ERL_FLAGS="-config `pwd`/' + config_filename + '" ' 
    os.system(config_prefix + "make " + name + "-start")
    api = external_api(name)
    wait(lambda: node_online(api), timeout_seconds=10, sleep_seconds=0.5)

def stop_node(name):
    os.system("make " + name + "-stop")
    time.sleep(3)

def should_start_node(name):
    return config['nodes'][name]['start']

def test_settings(test_name):
    return config['tests'][test_name]

def tool_settings(test_name):
    return config['tools'][test_name]

def post_fake_block(api, chain_file_name):
    top = api.get_top()
    block_height = top.height + 1
    premined_blocks = chain_downloader.load_from_file(chain_file_name)
    assert(len(premined_blocks) > block_height)
    block = list(filter(lambda b: b.height == block_height, premined_blocks))[0]
    res = api.post_block(block)
    return res

def genesis_hash(api):
    top = api.get_top()
    if top.height == 0:
        return top.hash
    block = api.get_block_by_hash(top.hash)
    while block.height != 1:
        block = api.get_block_by_hash(block.prev_hash)
    return block.prev_hash
