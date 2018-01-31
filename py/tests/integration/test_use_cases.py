# coding: utf-8

import tempfile
import os
import shutil
import time
from nose.tools import assert_equals, assert_not_equals, with_setup
import common
from waiting import wait
from swagger_client.models.ping import Ping 
from swagger_client.models.spend_tx import SpendTx

settings = common.test_settings(__name__.split(".")[-1])

def test_syncing():
    """
    Test node syncing:
    Alice should be able to connect to peers on startup and download the
    blockchain up to the current height.
    """
    test_settings = settings["test_syncing"]

    root_dir = tempfile.mkdtemp()
    mining_sys_config = make_fast_mining_config(root_dir, "mining_sys.config")
    no_mining_sys_config = make_no_mining_config(root_dir, "no_mining_sys.config")

    # start Bob's node
    bob_node = test_settings["nodes"]["bob"]
    common.start_node(bob_node, mining_sys_config)
    bob_api = common.external_api(bob_node)

    # Insert some blocks in Bob's chain
    blocks_to_mine = test_settings["blocks_to_mine"]
    print("Bob is mining")
    common.wait_until_height(bob_api, blocks_to_mine)
    bob_top = bob_api.get_top()
    assert_equals(bob_top.height >= blocks_to_mine, True)
    # Now Bob has at least blocks_to_mine blocks
    print("Bob has mined " + str(bob_top.height) + " blocks")

    # start Alice's node and let it connect with Bob's
    # note: Alice doesn't mine blocks
    alice_node = test_settings["nodes"]["alice"]
    common.start_node(alice_node, no_mining_sys_config)
    print("Alice is not mining")
    alice_api = common.external_api(alice_node)
    common.wait_until_height(alice_api, blocks_to_mine)
    alice_top = alice_api.get_top()
    assert_equals(alice_top.height >= blocks_to_mine, True)
    if alice_top.height > bob_top.height: # bob had mined more blocks
        bob_block = bob_api.get_block_by_hash(alice_top.hash) # this block is presnet
        assert_equals(bob_block.height, alice_top.height)
    else:
        assert_equals(alice_top.height, bob_top.height)
        assert_equals(alice_top.hash, bob_top.hash)

    print("Alice's had synced with Bob and now Alice's top has height " + str(alice_top.height))
    # stop both nodes
    common.stop_node(bob_node)
    common.stop_node(alice_node)

    shutil.rmtree(root_dir)

def test_persistence():
    """
    Test persistence:
    Bob's downloaded blockchain should persist between restarts. He should
    only download updates to his blockchain when his node starts.
    """
    test_settings = settings["test_persistence"]

    # prepare a dir to hold the config and DB files 
    root_dir = tempfile.mkdtemp()
    persistance_mining_sys_config = os.path.join(root_dir, "p_m_sys.config")
    only_persistance_sys_config = os.path.join(root_dir, "p_sys.config")
    f = open(persistance_mining_sys_config, "w")

    # this should be moved to an YAML once we have YAML node configuration
    f.write('[{aecore, [{db_path, "' + root_dir + '"},' + \
                      ' {persist, true},' + \
                      ' {autostart, true},' + \
                      ' {expected_mine_rate, 100},' + \
                      ' {aec_pow_cuckoo, {"mean16s-generic", "-t 5", 16}}' + \
                      ']}].')
    f.close()

    f = open(only_persistance_sys_config, "w")

    # this should be moved to an YAML once we have YAML node configuration
    f.write('[{aecore, [{db_path, "' + root_dir + '"},' + \
                      ' {persist, true}' + \
                      ']}].')
    f.close()

    bob_node = test_settings["nodes"]["bob"]
    common.start_node(bob_node, persistance_mining_sys_config)
    bob_api = common.external_api(bob_node)

    # Insert some blocks in Bob's chain
    blocks_to_mine = test_settings["blocks_to_mine"]
    common.wait_until_height(bob_api, blocks_to_mine)
    bob_top = bob_api.get_top()
    assert_equals(bob_top.height >= blocks_to_mine, True)
    # Now Bob has at least blocks_to_mine blocks

    common.stop_node(bob_node)

    common.start_node(bob_node, only_persistance_sys_config)
    bob_new_top = bob_api.get_top()
    if(bob_new_top.height > bob_top.height):
        # Bob's node had mined another block(s) before being stopped
        bob_block = bob_api.get_block_by_hash(bob_top.hash) # this block is presnet
        assert_equals(bob_block.height, bob_top.height)
    else:
        assert_equals(bob_new_top.height, bob_top.height)
        assert_equals(bob_top.hash, bob_new_top.hash)

    common.stop_node(bob_node)
    shutil.rmtree(root_dir)

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

    alice_peer_url = node_peer_url(alice_node)
    bob_peer_url = node_peer_url(bob_node)
    carol_peer_url = node_peer_url(carol_node)

    # prepare a dir to hold the configs
    root_dir = tempfile.mkdtemp()

    # Alice's config: no peers 
    alice_sys_config = make_peers_config(root_dir, "alice.config",
                            alice_peer_url, [], mining=True)
    print("\nAlice has address " + alice_peer_url + " and no peers")
    # Bob's config: only peer is Alice
    bob_sys_config = make_peers_config(root_dir, "bob.config",
                            bob_peer_url, [alice_peer_url], mining=False)
    print("Bob has address " + bob_peer_url + " and peers [" + alice_peer_url + "]")
    # Carol's config: only peer is Bob
    carol_sys_config = make_peers_config(root_dir, "carol.config",
                            carol_peer_url, [bob_peer_url], mining=False)
    print("Carol has address " + carol_peer_url + " and peers [" + bob_peer_url + "]")

    # start Alice's node
    common.start_node(alice_node, alice_sys_config)
    alice_api = common.external_api(alice_node)

    # Insert some blocks in Alice's chain
    blocks_to_mine = test_settings["blocks_to_mine"]
    common.wait_until_height(alice_api, blocks_to_mine)
    alice_top = alice_api.get_top()
    assert_equals(alice_top.height >= blocks_to_mine, True)
    # Now Alice has at least blocks_to_mine blocks

    # start the other nodes
    common.start_node(bob_node, bob_sys_config)
    common.start_node(carol_node, carol_sys_config)

    time.sleep(1) # give some time for the data to propagate

    carol_api = common.external_api(carol_node)
    carol_top = carol_api.get_top()
    gen_hash = common.genesis_hash(carol_api)
    ping_obj = Ping(source="http://localhost:1234",
                    genesis_hash=gen_hash,
                    best_hash=carol_top.hash,
                    difficulty=1,
                    share=32,
                    peers=[])
    ping = carol_api.ping(ping_obj)

    carol_peers = [peer.encode('utf-8') + "/" for peer in ping.peers]
    synced = alice_peer_url in carol_peers

    print("Carol now has peers " + str(carol_peers))
    assert_equals(synced, True) # for larger peer lists this might be too fragile
    assert_equals(carol_top.height >= blocks_to_mine, True)
    if carol_top.height > alice_top.height: # Alice had mined some more blocks
        alice_block = alice_api.get_block_by_hash(carol_top.hash) # this block is presnet
        assert_equals(alice_block.height, carol_top.height)
    else:
        assert_equals(alice_top.height, carol_top.height)

    # cleanup
    common.stop_node(alice_node)
    common.stop_node(bob_node)
    common.stop_node(carol_node)

    shutil.rmtree(root_dir)

def make_peers_config(root_dir, file_name, node_url, peers0, mining=False):
    sys_config = os.path.join(root_dir, file_name)
    mining_str = ""
    if mining:
        mining_str = ' {autostart, true},' + \
                     ' {expected_mine_rate, 100},' + \
                     ' {aec_pow_cuckoo, {"mean16s-generic", "-t 5", 16}}'
    else: 
        mining_str = ' {autostart, false}'
    f = open(sys_config, "w")
    peers = map(lambda url: '"' + url + '"', peers0)
    conf ='[{aecore, [{peers, [' + (", ").join(peers) + ']},' +\
                        mining_str + ']},' +\
          '{aehttp, [{local_peer_address, "' + node_url + '"}]}].'
    f.write(conf)
    f.close()
    return sys_config

def node_peer_url(node_name):
    host = common.node_config(node_name)["host"]
    port = common.node_config(node_name)["ports"]["external_api"]
    return "http://" + host + ":" + str(port) + "/"

def make_fast_mining_config(root_dir, file_name):
    sys_config = os.path.join(root_dir, file_name)
    f = open(sys_config, "w")
    # if autostart is not true - there will be no miner
    conf ='[{aecore, [{autostart, true},' + \
                    ' {expected_mine_rate, 100},' + \
                    ' {aec_pow_cuckoo, {"mean16s-generic", "-t 5", 16}}]}].'
    f.write(conf)
    f.close()
    return sys_config

def make_no_mining_config(root_dir, file_name):
    sys_config = os.path.join(root_dir, file_name)
    f = open(sys_config, "w")
    # if autostart is not true - there will be no miner
    conf ='[{aecore, [{autostart, false},' + \
                    ' {expected_mine_rate, 100},' + \
                    ' {aec_pow_cuckoo, {"mean16s-generic", "-t 5", 16}}]}].'
    f.write(conf)
    f.close()
    return sys_config
