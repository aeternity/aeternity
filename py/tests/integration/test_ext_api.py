# coding: utf-8

from nose.tools import assert_equals, assert_not_equals, with_setup
import common
import time

from py.tests.swagger_client.models.ping import Ping 
from py.tests import chain_downloader

settings = common.test_settings(__name__.split(".")[-1])
node = settings['node_to_test']
chain_data_file = settings['chain_data']

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
    top = api.get_top()
    ## ensure there is only genesis block
    assert_equals(top.height, 0)

    premined_blocks = chain_downloader.load_from_file(chain_data_file)
    block = chain_downloader.get_block(premined_blocks, height=1)
    res = api.post_block(block)
    time.sleep(1)
    top = api.get_top()
    assert_equals(top.height, 1)
    assert_equals(top.pow, str(block.pow))
    assert_equals(top.state_hash, block.state_hash)
    assert_equals(top.txs_hash, block.txs_hash)
    assert_equals(top.version, block.version)

@with_setup(setup_func, teardown_func)
def test_block_post_3():
    """
    Test case for posting 3 blocks
    """
    api = common.external_api(node)
    top = api.get_top()
    ## ensure there is only genesis block
    assert_equals(top.height, 0)

    premined_blocks = chain_downloader.load_from_file(chain_data_file)
    for i in range(1, 4):
        block = chain_downloader.get_block(premined_blocks, height=i)
        api.post_block(block)
        time.sleep(1)
        top = api.get_top()
        # ensure block is accepted
        assert_equals(top.height, i)
        assert_equals(top.pow, str(block.pow))
        assert_equals(top.state_hash, block.state_hash)
        assert_equals(top.txs_hash, block.txs_hash)
        assert_equals(top.version, block.version)

@with_setup(setup_func, teardown_func)
def test_block_post_wrong_pow():
    """
    Test rejecting a block with wrong PoW 
    """
    api = common.external_api(node)
    top = api.get_top()
    ## ensure there is only genesis block
    assert_equals(top.height, 0)

    premined_blocks = chain_downloader.load_from_file(chain_data_file)
    block1 = chain_downloader.get_block(premined_blocks, height=1)
    block2 = chain_downloader.get_block(premined_blocks, height=2)

    # modify the pow of block1 to be invalid against the header
    block1.pow = block2.pow
    res = api.post_block(block1)
    time.sleep(1)
    top = api.get_top()
    assert_equals(top.height, 0)

