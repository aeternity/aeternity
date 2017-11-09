# coding: utf-8

from nose.tools import assert_equals, assert_not_equals, with_setup
import common
import time

from py.tests.swagger_client.models.ping import Ping 

settings = common.test_settings(__name__)
node = settings['node_to_test']

def setup_func():
    if common.should_start_node(node):
        print("Node " + node + " starting")
        common.start_node(node)

def teardown_func():
    if common.should_start_node(node):
        print("Node " + node + " stopping")
        common.stop_node(node)

@with_setup(setup_func, teardown_func)
def test_get_top():
    """
    Testing of GET top HTTP endpoint
    """
    api = common.external_api(node)
    top = api.get_top()

@with_setup(setup_func, teardown_func)
def test_ping():
    """
    Testing of ping HTTP endpoint
    """
    api = common.external_api(node)
    top = api.get_top()
    gen_hash = common.genesis_hash(api)
    ping_obj = Ping(source="localhost",
                    genesis_hash=gen_hash,
                    best_hash=top.hash,
                    difficulty=top.difficulty,
                    share=32,
                    peers=[])
    ping = api.ping(ping_obj)
    #assert_not_equals(len(ping.peers), 0, "No peers")


@with_setup(setup_func, teardown_func)
def test_genesis():
    """
    Test case for getting genesis block
    """
    api = common.external_api(node)
    block = api.get_block_by_height(0)
    assert_equals(block.height, 0)
    assert_equals(block.pow, None)
    assert_equals(block.prev_hash, None)
    assert_equals(block.transactions, None)
    assert_equals(block.txs_hash, None)
    assert_equals(block.version, 1)

@with_setup(setup_func, teardown_func)
def test_block_post():
    """
    Test case for posting a new block
    """
    api = common.external_api(node)
    common.post_fake_block(api)
    time.sleep(1)
    top = api.get_top()
    assert_equals(top.height, 1)

