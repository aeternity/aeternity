# coding: utf-8

import tempfile
import os
import shutil
import time
from nose.tools import assert_equals, assert_not_equals, with_setup
import common
from waiting import wait
from py.tests import chain_downloader
from py.tests.swagger_client.models.ping import Ping 

settings = common.test_settings(__name__.split(".")[-1])

def test_syncing():
    """
    Test node syncing:
    Alice should be able to connect to peers on startup and download the
    blockchain up to the current height.
    """
    test_settings = settings["test_syncing"]
    chain_data_file = test_settings["chain_data"]
    # start Bob's node
    bob_node = test_settings["nodes"]["bob"]
    common.start_node(bob_node)
    bob_api = common.external_api(bob_node)

    # Insert some blocks in Bob's chain
    blocks_to_post = test_settings["blocks_to_post"]
    post_blocks(bob_api, blocks_to_post, chain_data_file)
    bob_top = bob_api.get_top()
    assert_equals(bob_top.height, blocks_to_post)
    # Now Bob has blocks_to_post blocks

    # start Alice's node and let it connect with Bob's
    alice_node = test_settings["nodes"]["alice"]
    common.start_node(alice_node)
    alice_api = common.external_api(alice_node)
    wait(lambda: alice_api.get_top().height == blocks_to_post, timeout_seconds=3, sleep_seconds=0.5)
    alice_top = alice_api.get_top()
    assert_equals(alice_top.height, blocks_to_post)
    # Alice has blocks_to_post blocks, too

    # ensure that both nodes have the same hash on top 
    assert_equals(alice_top.hash, bob_top.hash)

    # stop both nodes
    common.stop_node(bob_node)
    common.stop_node(alice_node)

def test_persistence():
    """
    Test persistence:
    Bob's downloaded blockchain should persist between restarts. He should
    only download updates to his blockchain when his node starts.
    """
    test_settings = settings["test_persistence"]
    chain_data_file = test_settings["chain_data"]

    # prepare a dir to hold the config and DB files 
    root_dir = tempfile.mkdtemp()
    sys_config = os.path.join(root_dir, "sys.config")
    f = open(sys_config, "w")

    # this should be moved to an YAML once we have YAML node configuration
    f.write('[{aecore, [{db_path, "' + root_dir + '"},{persist, true}]}].')
    f.close()

    bob_node = test_settings["nodes"]["bob"]
    common.start_node(bob_node, sys_config)
    bob_api = common.external_api(bob_node)
    
    # there is no old persisted data
    bob_top = bob_api.get_top()
    assert_equals(bob_top.height, 0)

    # Insert some blocks in Bob's chain
    blocks_to_post = test_settings["blocks_to_post"]
    post_blocks(bob_api, blocks_to_post, chain_data_file)
    bob_top = bob_api.get_top()
    assert_equals(bob_top.height, blocks_to_post)
    # Now Bob has blocks_to_post blocks

    common.stop_node(bob_node)

    common.start_node(bob_node, sys_config)
    bob_new_top = bob_api.get_top()
    assert_equals(bob_new_top.height, blocks_to_post)
    assert_equals(bob_top.hash, bob_new_top.hash)

    shutil.rmtree(root_dir)
    common.stop_node(bob_node)

def test_node_discovery():
    """
    Test node discovery
    Assuming Carol's node only knows about Bob upon startup and that Bob's
    node knows Alice, Carol's node should be able to discover Alice and
    sync with her node.
    """
    test_settings = settings["test_node_discovery"]
    alice_node = test_settings["nodes"]["alice"]
    bob_node = test_settings["nodes"]["bob"]
    carol_node = test_settings["nodes"]["carol"]
    chain_data_file = test_settings["chain_data"]

    alice_peer_url = node_peer_url(alice_node)
    bob_peer_url = node_peer_url(bob_node)
    carol_peer_url = node_peer_url(carol_node)

    # prepare a dir to hold the configs
    root_dir = tempfile.mkdtemp()

    # Alice's config: no peers 
    alice_sys_config = make_peers_config(root_dir, "alice.config",
                            alice_peer_url, [])
    # Bob's config: only peer is Alice
    bob_sys_config = make_peers_config(root_dir, "bob.config",
                            bob_peer_url, [alice_peer_url])
    # Carol's config: only peer is Bob
    carol_sys_config = make_peers_config(root_dir, "carol.config",
                            carol_peer_url, [bob_peer_url])

    # start Alice's node
    common.start_node(alice_node, alice_sys_config)
    alice_api = common.external_api(alice_node)

    # Insert some blocks in Alice's chain
    blocks_to_post = test_settings["blocks_to_post"]
    post_blocks(alice_api, blocks_to_post, chain_data_file)
    alice_top = alice_api.get_top()
    assert_equals(alice_top.height, blocks_to_post)
    # Now Alice has blocks_to_post blocks

    # start the other nodes
    common.start_node(bob_node, bob_sys_config)
    common.start_node(carol_node, carol_sys_config)

    time.sleep(1) # give some time for the data to propagate

    carol_api = common.external_api(carol_node)
    carol_top = carol_api.get_top()
    gen_hash = common.genesis_hash(carol_api)
    ping_obj = Ping(source="localhost",
                    genesis_hash=gen_hash,
                    best_hash=carol_top.hash,
                    difficulty=carol_top.difficulty,
                    share=32,
                    peers=[])
    ping = carol_api.ping(ping_obj)

    carol_peers = ping.peers
    synced = len(list(filter(lambda peer: peer == alice_peer_url, carol_peers))) == 1

    assert_equals(synced, True) # for larger peer lists this might be too fragile
    assert_equals(carol_top.height, blocks_to_post)
    assert_equals(carol_top.hash, alice_top.hash)
    # cleanup
    common.stop_node(alice_node)
    common.stop_node(bob_node)
    common.stop_node(carol_node)

    shutil.rmtree(root_dir)

def post_blocks(api, blocks_cnt, chain_data_file):
    premined_blocks = chain_downloader.load_from_file(chain_data_file)
    for i in range(1, blocks_cnt + 1):
        block = chain_downloader.get_block(premined_blocks, height=i)
        api.post_block(block)
        wait(lambda: api.get_top().height == i, timeout_seconds=3, sleep_seconds=0.25)

def make_peers_config(root_dir, file_name, node_url, peers0):
    sys_config = os.path.join(root_dir, file_name)
    f = open(sys_config, "w")
    peers = map(lambda url: '"' + url + '"', peers0)
    conf ='[{aecore, [{peers, [' + (", ").join(peers) + ']}]},' +\
          '{aehttp, [{local_peer_address, "' + node_url + '"}]}].'
    f.write(conf)
    f.close()
    return sys_config

def node_peer_url(node_name):
    host = common.node_config(node_name)["host"]
    port = common.node_config(node_name)["ports"]["external_api"]
    return "http://" + host + ":" + str(port) + "/"
