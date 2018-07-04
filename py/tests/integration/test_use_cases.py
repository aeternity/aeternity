# coding: utf-8

import tempfile
import os
import shutil
from nose.tools import assert_equals, assert_not_equals, assert_true, with_setup
import common
from waiting import wait
from swagger_client.models.peers import Peers

settings = common.test_settings(__name__.split(".")[-1])

def test_syncing():
    """
    Test node syncing:
    Alice should be able to connect to peers on startup and download the
    blockchain up to the current height.
    """
    test_settings = settings["test_syncing"]

    root_dir = tempfile.mkdtemp()
    mining_user_config = make_fast_mining_user_config(root_dir, "mining_epoch.yaml")
    no_mining_user_config = make_no_mining_user_config(root_dir, "no_mining_epoch.yaml")

    # start Bob's node
    bob_node = test_settings["nodes"]["bob"]
    common.start_node(bob_node, mining_user_config)
    bob_api = common.external_api(bob_node)

    # Insert some blocks in Bob's chain
    blocks_to_mine = test_settings["blocks_to_mine"]
    print("Bob is mining")
    common.wait_until_height(bob_api, blocks_to_mine)
    bob_top = bob_api.get_top_block()
    assert_equals(bob_top.height >= blocks_to_mine, True)
    # Now Bob has at least blocks_to_mine blocks
    print("Bob has mined " + str(bob_top.height) + " blocks")

    # start Alice's node and let it connect with Bob's
    # note: Alice doesn't mine blocks
    alice_node = test_settings["nodes"]["alice"]
    common.start_node(alice_node, no_mining_user_config)
    print("Alice is not mining")
    alice_api = common.external_api(alice_node)
    common.wait_until_height(alice_api, blocks_to_mine)
    alice_top = alice_api.get_top_block()
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
    p_m_conf = """\
---
chain:
    persist: true
    db_path: \"""" + root_dir + """\"

mining:
    autostart: true
    expected_mine_rate: 100
    beneficiary: "ak$2QLChDdERfod9QajLkCTsJnYP3RNqZJmAFWQWQZWr99fSrC55h"
    cuckoo:
        miner:
            executable: mean16s-generic
            extra_args: "-t 5"
            node_bits: 16
"""
    p_conf = """\
---
chain:
    persist: true
    db_path: \"""" + root_dir + """\"

mining:
    beneficiary: "ak$2QLChDdERfod9QajLkCTsJnYP3RNqZJmAFWQWQZWr99fSrC55h"
"""
    persistence_mining_user_config = common.install_user_config(root_dir, "p_m_epoch.yaml", p_m_conf)
    minimal_user_config_with_persistence = common.install_user_config(root_dir, "p_epoch.yaml", p_conf)

    bob_node = test_settings["nodes"]["bob"]
    common.start_node(bob_node, persistence_mining_user_config)
    bob_api = common.external_api(bob_node)

    # Insert some blocks in Bob's chain
    blocks_to_mine = test_settings["blocks_to_mine"]
    common.wait_until_height(bob_api, blocks_to_mine)
    bob_top = bob_api.get_top_block()
    assert_equals(bob_top.height >= blocks_to_mine, True)
    # Now Bob has at least blocks_to_mine blocks

    common.stop_node(bob_node)

    common.start_node(bob_node, minimal_user_config_with_persistence)
    bob_new_top = bob_api.get_top_block()
    if(bob_new_top.height > bob_top.height):
        # Bob's node had mined another block(s) before being stopped
        bob_block = bob_api.get_block_by_hash(bob_top.hash) # this block is presnet
        assert_equals(bob_block.height, bob_top.height)
    else:
        assert_equals(bob_new_top.height, bob_top.height)
        assert_equals(bob_top.hash, bob_new_top.hash)

    common.stop_node(bob_node)
    shutil.rmtree(root_dir)

def test_node_discovery_transitively():
    """
    Test node discovery (transitively)
    Assuming Carol's node only knows about Bob upon startup and that Bob's
    node knows Alice, Carol's node should be able to discover Alice and
    sync with her node.
    """
    test_settings = settings["test_node_discovery_transitively"]
    alice_node = test_settings["nodes"]["alice"]
    bob_node = test_settings["nodes"]["bob"]
    carol_node = test_settings["nodes"]["carol"]

    # prepare a dir to hold the configs
    root_dir = tempfile.mkdtemp()

    # Alice's config: no peers
    alice_peers = "peers: []"
    alice_user_config = make_peers_user_config(root_dir, "alice_epoch.yaml",
                            "node1", "3015", alice_peers, "true")
    # Bob's config: only peer is Alice
    bob_peers = """\
peers:
    - "aenode://pp$HdcpgTX2C1aZ5sjGGysFEuup67K9XiFsWqSPJs4RahEcSyF7X@localhost:3015"
"""
    bob_user_config = make_peers_user_config(root_dir, "bob_epoch.yaml",
                            "node2", "3025", bob_peers, "false")
    # Carol's config: only peer is Bob
    carol_peers = """\
peers:
    - "aenode://pp$28uQUgsPcsy7TQwnRxhF8GMKU4ykFLKsgf4TwDwPMNaSCXwWV8@localhost:3025"
"""
    carol_user_config = make_peers_user_config(root_dir, "carol_epoch.yaml",
                            "node3", "3035", carol_peers, "false")

    # start Alice's node
    common.start_node(alice_node, alice_user_config)
    alice_api = common.external_api(alice_node)

    # Insert some blocks in Alice's chain
    blocks_to_mine = test_settings["blocks_to_mine"]
    common.wait_until_height(alice_api, blocks_to_mine)
    alice_top = alice_api.get_top_block()
    assert_true(alice_top.height >= blocks_to_mine)
    # Now Alice has at least blocks_to_mine blocks

    # start the other nodes
    common.start_node(bob_node, bob_user_config)
    common.start_node(carol_node, carol_user_config)

    # Check that Carol syncs with Alice's chain
    carol_api = common.external_api(carol_node)
    common.wait_until_height(carol_api, alice_top.height)
    assert_equals(carol_api.get_block_by_hash(alice_top.hash).height, alice_top.height)

    # Check that Carol discovers Alice as a peer
    carol_int_api = common.internal_api(carol_node)
    wait(lambda: 'aenode://pp$HdcpgTX2C1aZ5sjGGysFEuup67K9XiFsWqSPJs4RahEcSyF7X@localhost:3015' in get_peers(carol_int_api), timeout_seconds=20, sleep_seconds=1)

    # cleanup
    common.stop_node(alice_node)
    common.stop_node(bob_node)
    common.stop_node(carol_node)

    shutil.rmtree(root_dir)

def test_node_discovery_from_common_friend():
    """
    Test node discovery (from common friend peer)
    Assuming Carol's node only knows about Bob upon startup and that Alice's
    node knows Bob, Carol's node should be able to discover Alice and
    sync with her node.
    """
    test_settings = settings["test_node_discovery_from_common_friend"]
    alice_node = test_settings["nodes"]["alice"]
    bob_node = test_settings["nodes"]["bob"]
    carol_node = test_settings["nodes"]["carol"]

    # prepare a dir to hold the configs
    root_dir = tempfile.mkdtemp()

    # Alice's config: only peer is Bob
    alice_peers = """\
peers:
    - "aenode://pp$28uQUgsPcsy7TQwnRxhF8GMKU4ykFLKsgf4TwDwPMNaSCXwWV8@localhost:3025"
"""
    alice_user_config = make_peers_user_config(root_dir, "alice_epoch.yaml",
                            "node1", "3015", alice_peers, "true")
    # Bob's config: no peers
    bob_peers = "peers: []"
    bob_user_config = make_peers_user_config(root_dir, "bob_epoch.yaml",
                            "node2", "3025", bob_peers, "false")
    # Carol's config: only peer is Bob
    carol_peers = """\
peers:
    - "aenode://pp$28uQUgsPcsy7TQwnRxhF8GMKU4ykFLKsgf4TwDwPMNaSCXwWV8@localhost:3025"
"""
    carol_user_config = make_peers_user_config(root_dir, "carol_epoch.yaml",
                            "node3", "3035", carol_peers, "false")

    # start Alice's node
    common.start_node(alice_node, alice_user_config)
    alice_api = common.external_api(alice_node)

    # Insert some blocks in Alice's chain
    blocks_to_mine = test_settings["blocks_to_mine"]
    common.wait_until_height(alice_api, blocks_to_mine)
    alice_top = alice_api.get_top_block()
    assert_true(alice_top.height >= blocks_to_mine)
    # Now Alice has at least blocks_to_mine blocks

    # start the other nodes
    common.start_node(bob_node, bob_user_config)
    common.start_node(carol_node, carol_user_config)

    # Check that Carol syncs with Alice's chain
    carol_api = common.external_api(carol_node)
    common.wait_until_height(carol_api, alice_top.height)
    assert_equals(carol_api.get_block_by_hash(alice_top.hash).height, alice_top.height)

    # Check that Carol discovers Alice as a peer
    carol_int_api = common.internal_api(carol_node)
    wait(lambda: 'aenode://pp$HdcpgTX2C1aZ5sjGGysFEuup67K9XiFsWqSPJs4RahEcSyF7X@127.0.0.1:3015' in get_peers(carol_int_api), timeout_seconds=20, sleep_seconds=1)

    # cleanup
    common.stop_node(alice_node)
    common.stop_node(bob_node)
    common.stop_node(carol_node)

    shutil.rmtree(root_dir)

def copy_peer_keys(root_dir, keys):
    # Copy the right keys
    curr_dir = os.getcwd()
    key_dir  = os.path.join(root_dir, keys)
    os.makedirs(key_dir)
    shutil.copy(os.path.join(curr_dir, "tests", "peer_keys", keys, "peer_key"), key_dir)
    shutil.copy(os.path.join(curr_dir, "tests", "peer_keys", keys, "peer_key.pub"), key_dir)
    return key_dir

def make_peers_user_config(root_dir, file_name, keys, sync_port, peers, mining):
    key_dir = copy_peer_keys(root_dir, keys)
    conf = """\
---
""" + peers + """

sync:
    port: {}

keys:
    dir: "{}"
    password: "top secret"

mining:
    autostart: {}
    expected_mine_rate: 100
    beneficiary: "ak$2QLChDdERfod9QajLkCTsJnYP3RNqZJmAFWQWQZWr99fSrC55h"
    cuckoo:
        miner:
            executable: mean16s-generic
            extra_args: "-t 5"
            node_bits: 16
""".format(sync_port, key_dir, mining)
    return common.install_user_config(root_dir, file_name, conf)

def make_fast_mining_user_config(root_dir, file_name):
    key_dir = copy_peer_keys(root_dir, "node1")
    conf = """\
---
peers:
    - "aenode://pp$28uQUgsPcsy7TQwnRxhF8GMKU4ykFLKsgf4TwDwPMNaSCXwWV8@localhost:3025"

sync:
    port: 3015

keys:
    dir: \"""" + key_dir + """\"
    password: "top secret"

chain:
    hard_forks:
        "17": 0

mining:
    autostart: true
    expected_mine_rate: 100
    beneficiary: "ak$2QLChDdERfod9QajLkCTsJnYP3RNqZJmAFWQWQZWr99fSrC55h"
    cuckoo:
        miner:
            executable: mean16s-generic
            extra_args: "-t 5"
            node_bits: 16
"""
    return common.install_user_config(root_dir, file_name, conf)

def make_no_mining_user_config(root_dir, file_name):
    key_dir = copy_peer_keys(root_dir, "node2")
    conf = """\
---
peers:
    - "aenode://pp$HdcpgTX2C1aZ5sjGGysFEuup67K9XiFsWqSPJs4RahEcSyF7X@localhost:3015"

sync:
    port: 3025

keys:
    dir: \"""" + key_dir + """\"
    password: "top secret"

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
    return common.install_user_config(root_dir, file_name, conf)

def get_peers(int_api):
    peers = int_api.get_peers().peers
    print("Peers: " + str(peers))
    return peers
