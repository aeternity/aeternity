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

def start_node(name):
    os.system("make " + name + "-start")
    api = external_api(name)
    wait(lambda: node_online(api), timeout_seconds=3, sleep_seconds=0.5)

def stop_node(name):
    os.system("make " + name + "-stop")

def should_start_node(name):
    return config['nodes'][name]['start']

def test_settings(test_name):
    return config['tests'][test_name]

## will be obsolete once we have a downloaded chain
def signed_coinbase_tx():
    account = "BOmMCqJhVEExaiRCXhEj6c8CuLtZYOzJQea9b++3IVTcdgyvuXbG9r8SNoHqvrtQlifWLzjGHDnSNdpopI2FCEQ="
    coinbase = CoinbaseTx(pubkey = account)
    return SignedTx(data = coinbase, type = "coinbase",
            signatures =
            ["MEUCIEdDPJU2swc6WsuopA42WpygsjtqgCbAikA+kdCivBowAiEApaYByYyWCplLoUQjiNRLU+Qm6voe19jFWA+YRJp+uD8="])

def utc_now():
    d = datetime.datetime.utcnow()
    epoch = datetime.datetime(1970,1,1)
    return int((d - epoch).total_seconds())


## TODO: premine and download a chain to use here
def post_fake_block(api):
    top = api.get_top()
    block_height = top.height + 1
    block = Block(height = block_height,
            prev_hash = top.hash,
            ## temporary
            state_hash = "pMwJahZJDsHOiJvzuSGlORtevROpcyXjzCZJ+lAMRJ4=",
            ## temporary
            txs_hash = "6ZbhNbx+q39yS8EzyCikQ+6KjANwLVVjMaw0w4sINL8=",
            ## temporary
            target = 553713663,
            ## temporary
            nonce = 1360986163,
            time = 1510149537578,
            version = 1,
            ## temporary
            pow = [
                2344444,2873496,23874225,26039777,31520081,35302217,38813967,46358072,48293190,53744832,53997324,54498726,55605598,58489038,59201564,60098152,61303671,70682656,74754278,83224299,83889767,85718841,88764282,89000483,94071693,101178223,106148408,106923001,107072719,107250361,107263946,111029776,111490519,113694971,114155485,115514202,120663832,121513102,124456475,126343779,126480179,129129131
                ],
            ## temporary
            transactions=[signed_coinbase_tx()]
            )
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
