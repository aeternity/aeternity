# coding: utf-8

import tempfile
import os
import shutil
import time
from nose.tools import assert_equals, assert_not_equals, with_setup
import common
from waiting import wait
from py.tests.swagger_client.models.ping import Ping 
from py.tests.swagger_client.models.spend_tx import SpendTx
from py.tests.swagger_client.rest import ApiException

settings = common.test_settings(__name__.split(".")[-1])

def test_successful():
    # Alice should be able to create a spend transaction to send tokens to
    # Bob. In a controlled environment, Alice should see her transaction
    # appear in the next block or two that are added to the blockchain. 
    #
    # Once that happens, Alice should see her account debited and Bob's
    # account credited with the same number of tokens.
    #
    # The debit/credit should not happen until the transaction is confirmed,
    # e.g. included in at least one block in the chain.
    test_settings = settings["test_successful"]
    coinbase_reward = common.coinbase_reward() 
    (root_dir, alice_node, alice_api, alice_top) = setup_node_with_tokens(test_settings, "alice") 

    spend_tx_amt = test_settings["spend_tx"]["amount"]
    spend_tx_fee = test_settings["spend_tx"]["fee"]
    alice_balance = alice_api.get_account_balance()
    alice_has_enough_tokens = alice_balance.balance >= spend_tx_amt + spend_tx_fee
    assert_equals(alice_has_enough_tokens, True)
    print("Alice initial balance is " + str(alice_balance.balance))

    bob_balance0 = {"balance":0} 
    try:
        bob_balance0 = alice_api.get_account_balance(pub_key=test_settings["spend_tx"]["bob_pubkey"])
        assert_equals(bob_balance1.balance, bob_balance0.balance)

    except ApiException as e:
        assert_equals(e.status, 404) # Bob has no account yet
    # Alice sends some tokens to Bob 
    alice_internal_api = common.internal_api(alice_node)
    spend_tx_obj = SpendTx(
        recipient_pubkey=test_settings["spend_tx"]["bob_pubkey"],
        amount=spend_tx_amt,
        fee=spend_tx_fee)
    print("Alice's spend_tx is " + str(spend_tx_obj))
    alice_internal_api.post_spend_tx(spend_tx_obj)
    print("Transaction sent")

    # ensure Alice balance had not changed
    alice_balance1 = alice_api.get_account_balance()
    assert_equals(alice_balance.balance, alice_balance1.balance)

    # ensure Bob balance had not changed
    try:
        bob_balance1 = alice_api.get_account_balance(pub_key=test_settings["spend_tx"]["bob_pubkey"])
        assert_equals(bob_balance1.balance, bob_balance0.balance)
    except ApiException as e:
        assert_equals(e.status, 404) # Bob has no account yet

    # wait for a block to be mined
    print("Waiting for a next block to be mined")
    common.wait_until_height(alice_api, alice_top.height + 1)

    alice_new_top = alice_api.get_top()
    alice_new_balance = alice_api.get_account_balance()
    
    blocks_mined = alice_new_top.height - alice_top.height

    # Since Alice had mined the block she is receiving the fee and the
    # coinbase_reward
    # Alice should have
    # tokens = old_balance - spent_amt - spend_tx + spent_fee + coinbase_reward
    expected_balance = alice_balance.balance - spend_tx_amt  + coinbase_reward * blocks_mined
    assert_equals(alice_new_balance.balance, expected_balance)

    bob_balance = alice_api.get_account_balance(pub_key=test_settings["spend_tx"]["bob_pubkey"])
    print("Coinbase reward is " + str(coinbase_reward) + ", had mined " +
            str(blocks_mined) + " blocks")
    print("Alice's balance (with a coinbase reward) is now " + str(alice_new_balance.balance))
    print("Bob's balance is now " + str(bob_balance.balance))
    assert_equals(bob_balance.balance, spend_tx_amt)
    # stop node
    common.stop_node(alice_node)
    shutil.rmtree(root_dir)

def make_mining_config(root_dir, file_name):
    sys_config = os.path.join(root_dir, file_name)
    f = open(sys_config, "w")
    # if autostart is not true - there will be no miner
    conf ='[{aecore, [{autostart, true},' + \
                    ' {aec_pow_cuckoo, {"lean16", "-t 5", 16}}]}].' 
    f.write(conf)
    f.close()
    return sys_config

def setup_node_with_tokens(test_settings, node_name):
    # prepare a dir to hold the configs and the keys
    root_dir = tempfile.mkdtemp()

    # setup the dir with Alice's node mining
    node = test_settings["nodes"][node_name]
    sys_config = make_mining_config(root_dir, "sys.config")
    common.start_node(node, sys_config)
    api = common.external_api(node)

    # populate the chain so Alice had mined some blocks and has tokens
    # to spend
    blocks_to_mine = test_settings["blocks_to_mine"]
    common.wait_until_height(api, blocks_to_mine)
    top = api.get_top()
    assert_equals(top.height >= blocks_to_mine, True)
    # Now the node has at least blocks_to_mine blocks mined by Alice 

    return (root_dir, node, api, top)


