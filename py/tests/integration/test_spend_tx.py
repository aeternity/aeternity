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
    (root_dir, alice_node, alice_api, alice_top) = setup_node_with_tokens(test_settings) 

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
    # NB: as long as we're using a big graph Cuckoo PoW - this will take some
    # time
    print("Waiting for a next block to be mined")
    wait(lambda: alice_api.get_top().height == alice_top.height + 1, \
                timeout_seconds=60*60,\
                sleep_seconds=1)

    alice_new_balance = alice_api.get_account_balance()

    # Since Alice had mined the block she is receiving the fee and the
    # coinbase_reward
    # Alice should have
    # tokens = old_balance - spent_amt - spend_tx + spent_fee + coinbase_reward
    expected_balance = alice_balance.balance - spend_tx_amt  + coinbase_reward
    assert_equals(alice_new_balance.balance, expected_balance)

    bob_balance = alice_api.get_account_balance(pub_key=test_settings["spend_tx"]["bob_pubkey"])
    print("Coinbase reward is " + str(coinbase_reward))
    print("Alice's balance (with a coinbase reward) is now " + str(alice_new_balance.balance))
    print("Bob's balance is now " + str(bob_balance.balance))
    assert_equals(bob_balance.balance, spend_tx_amt)
    # stop node
    common.stop_node(alice_node)
    shutil.rmtree(root_dir)

def make_keys_config(root_dir, file_name, password, keys_dir):
    sys_config = os.path.join(root_dir, file_name)
    f = open(sys_config, "w")
    # if autostart is not true - there will be no miner
    conf ='[{aecore, [{keys_dir, "' + keys_dir + '/"},' +\
                    ' {password, <<"' + password + '">>},' + \
                    ' {autostart, true}]}].' 
    f.write(conf)
    f.close()
    return sys_config

def setup_node_with_tokens(test_settings):
    preset_keys_dir = test_settings["keys"]["dir"]
    keys_password = test_settings["keys"]["password"]
    chain_data_file = test_settings["chain_data"]

    # prepare a dir to hold the configs and the keys
    root_dir = tempfile.mkdtemp()

    # setup the dir with Alice's key pair; later on we will post blocks to the
    # chain and the blocks will be mined by Alice - that's how she will have
    # some tokens to spend
    keys_dir = os.path.join(root_dir, "keys")
    os.mkdir(keys_dir)
    shutil.copyfile(os.path.join(preset_keys_dir, "key"),\
                    os.path.join(keys_dir, "key"))
    shutil.copyfile(os.path.join(preset_keys_dir, "key.pub"),\
                    os.path.join(keys_dir, "key.pub"))
    time.sleep(1)


    # start the node
    alice_node = test_settings["nodes"]["alice"]
    sys_config = make_keys_config(root_dir, "sys.config",
                                keys_password, keys_dir)
    common.start_node(alice_node, sys_config)
    alice_api = common.external_api(alice_node)

    # populate the chain so Alice had mined some blocks and has tokens
    # to spend
    blocks_to_post = test_settings["blocks_to_post"]
    common.post_blocks(alice_api, blocks_to_post, chain_data_file)
    alice_top = alice_api.get_top()
    assert_equals(alice_top.height, blocks_to_post)
    # Now the node has blocks_to_post blocks mined by Alice and her keys

    return (root_dir, alice_node, alice_api, alice_top)


