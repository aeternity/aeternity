# coding: utf-8

from nose.tools import assert_equals, assert_not_equals, with_setup
import common
from waiting import wait

def test_syncing():
    """
    Test node syncing:
    Alice should be able to connect to peers on startup and download the
    blockchain up to the current height.
    """
    # start Bob's node
    bob_node = "dev1"
    common.start_node(bob_node)
    bob_api = common.external_api(bob_node)

    # Insert a block in Bob's chain
    common.post_fake_block(bob_api)
    wait(lambda: bob_api.get_top().height == 1, timeout_seconds=3, sleep_seconds=0.5)
    bob_top = bob_api.get_top()
    assert_equals(bob_top.height, 1)
    # Now Bob has 2 blocks: with height 0 (genesis) and 1

    # start Alice's node and let it connect with Bob's
    alice_node = "dev2"
    common.start_node(alice_node)
    alice_api = common.external_api(alice_node)
    wait(lambda: alice_api.get_top().height == 1, timeout_seconds=3, sleep_seconds=0.5)
    alice_top = alice_api.get_top()
    assert_equals(alice_top.height, 1)
    # Alice has 2 blocks, too

    # ensure that both nodes have the same hash on block with height=1
    assert_equals(alice_top.hash, bob_top.hash)

    # stop both nodes
    common.stop_node(bob_node)
    common.stop_node(alice_node)
