# coding: utf-8

import tempfile
import os
import shutil
import time
from nose.tools import assert_equals, assert_not_equals, with_setup
import common
import json
from waiting import wait
from swagger_client.models.ping import Ping 
from swagger_client.models.tx import Tx
from swagger_client.models.spend_tx import SpendTx
from swagger_client.models.name_preclaim_tx import NamePreclaimTx
from swagger_client.models.name_claim_tx import NameClaimTx
from swagger_client.models.name_update_tx import NameUpdateTx
from swagger_client.rest import ApiException

import keys

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
    alice_internal_api = common.internal_api(alice_node)

    spend_tx_amt = test_settings["spend_tx"]["amount"]
    spend_tx_fee = test_settings["spend_tx"]["fee"]
    alice_balance = common.get_account_balance(alice_internal_api)
    alice_has_enough_tokens = alice_balance.balance >= spend_tx_amt + spend_tx_fee
    assert_equals(alice_has_enough_tokens, True)
    print("Alice initial balance is " + str(alice_balance.balance))

    bob_balance0 = common.get_account_balance(alice_internal_api, pub_key=test_settings["spend_tx"]["bob_pubkey"])
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
    alice_balance1 = common.get_account_balance(alice_internal_api)
    assert_equals(alice_balance.balance, alice_balance1.balance)

    # ensure Bob balance had not changed
    bob_balance1 = common.get_account_balance(alice_internal_api, pub_key=test_settings["spend_tx"]["bob_pubkey"])
    assert_equals(bob_balance1.balance, bob_balance0.balance)

    # wait for a block to be mined
    print("Waiting for a next block to be mined")
    common.wait_until_height(alice_api, alice_top.height + 1)

    alice_new_top = alice_api.get_top()
    alice_new_balance = common.get_account_balance(alice_internal_api)
    
    blocks_mined = alice_new_top.height - alice_top.height

    # Since Alice had mined the block she is receiving the fee and the
    # coinbase_reward
    # Alice should have
    # tokens = old_balance - spent_amt - spend_tx + spent_fee + coinbase_reward
    expected_balance = alice_balance.balance - spend_tx_amt  + coinbase_reward * blocks_mined
    assert_equals(alice_new_balance.balance, expected_balance)

    bob_balance = common.get_account_balance(alice_internal_api, pub_key=test_settings["spend_tx"]["bob_pubkey"])
    print("Coinbase reward is " + str(coinbase_reward) + ", had mined " +
            str(blocks_mined) + " blocks")
    print("Alice's balance (with a coinbase reward) is now " + str(alice_new_balance.balance))
    print("Bob's balance is now " + str(bob_balance.balance))
    assert_equals(bob_balance.balance, spend_tx_amt)
    # stop node
    common.stop_node(alice_node)
    shutil.rmtree(root_dir)

def test_not_enough_tokens():
    # Bob should not be able to send more tokens than he has
    #
    # Let's say Bob has 100 tokens. He should not be able to send more than
    # 100 tokens to Alice.
    #
    # If there's an incoming but unconfirmed deposit into Bob's account then Bob
    # should not be able to use the incoming tokens until the spend transaction
    # they are in is confirmed.
    test_settings = settings["test_not_enough_tokens"]
    coinbase_reward = common.coinbase_reward() 
    (root_dir, bob_node, bob_api, bob_top) = setup_node_with_tokens(test_settings, "bob") 
    bob_internal_api = common.internal_api(bob_node)

    spend_tx_amt = test_settings["spend_tx"]["amount"]
    spend_tx_fee = test_settings["spend_tx"]["fee"]
    bob_balance = common.get_account_balance(bob_internal_api)
    bob_has_not_enough_tokens = bob_balance.balance < spend_tx_amt + spend_tx_fee
    assert_equals(bob_has_not_enough_tokens, True)
    print("Bob initial balance is " + str(bob_balance.balance) +
            " and he will try to spend " + str(spend_tx_amt + spend_tx_fee))

    alice_balance0 = common.get_account_balance(bob_internal_api, pub_key=test_settings["spend_tx"]["alice_pubkey"])

    # Bob tries to send some tokens to Alice
    spend_tx_obj = SpendTx(
        recipient_pubkey=test_settings["spend_tx"]["alice_pubkey"],
        amount=spend_tx_amt,
        fee=spend_tx_fee)
    print("Bob's spend_tx is " + str(spend_tx_obj))
    bob_internal_api.post_spend_tx(spend_tx_obj)
    print("Transaction sent")

    # ensure Bob balance had not changed
    bob_balance1 = common.get_account_balance(bob_internal_api)
    assert_equals(bob_balance.balance, bob_balance1.balance)

    # ensure Alice balance had not changed
    alice_balance1 = common.get_account_balance(bob_internal_api, pub_key=test_settings["spend_tx"]["alice_pubkey"])
    # wait for a block to be mined
    print("Waiting for a next block to be mined")
    common.wait_until_height(bob_api, bob_top.height + 1)

    bob_new_top = bob_api.get_top()
    bob_new_balance = common.get_account_balance(bob_internal_api)
    
    blocks_mined = bob_new_top.height - bob_top.height

    # Since Bob had mined the block he is receiving the coinbase_reward
    expected_balance = bob_balance.balance + coinbase_reward * blocks_mined
    assert_equals(bob_new_balance.balance, expected_balance)

    # ensure Alice balance had not changed
    alice_balance = common.get_account_balance(bob_internal_api, pub_key=test_settings["spend_tx"]["alice_pubkey"])
    print("Coinbase reward is " + str(coinbase_reward) + ", had mined " +
            str(blocks_mined) + " blocks")
    print("Bob's balance (with a coinbase reward) is now " + str(bob_new_balance.balance))
    print("Alice's balance is now " + str(alice_balance.balance))
    assert_equals(alice_balance.balance, alice_balance0.balance)
    # stop node
    common.stop_node(bob_node)
    shutil.rmtree(root_dir)

def test_send_by_name():
    # Bob registers a name 'bob.aet'
    # Alice should be able to send tokens to Bob using that name
    test_settings = settings["test_send_by_name"]
    coinbase_reward = common.coinbase_reward() 
    (root_dir, node, ext_api, top) = setup_node_with_tokens(test_settings, "miner") 
    int_api = common.internal_api(node)

    alice_private_key = keys.new_private()
    alice_public_key = keys.public_key(alice_private_key)
    alice_address = keys.address(alice_public_key)

    bob_private_key = keys.new_private()
    bob_public_key = keys.public_key(bob_private_key)
    bob_address = keys.address(bob_public_key)

    # initial balances - amounts that the miner should send them
    alice_init_balance = test_settings["send_tokens"]["alice"]
    bob_init_balance = test_settings["send_tokens"]["bob"]

    # populate accounts with tokens
    miner_send_tokens(alice_address, alice_init_balance, int_api, ext_api)
    miner_send_tokens(bob_address, bob_init_balance, int_api, ext_api)

    # validate balances
    alice_balance0 = common.get_account_balance(int_api, pub_key=alice_address).balance
    bob_balance0 = common.get_account_balance(int_api, pub_key=bob_address).balance

    assert_equals(alice_balance0, alice_init_balance)
    assert_equals(bob_balance0, bob_init_balance)
    print("Alice balance is " + str(alice_balance0))
    print("Bob balance is " + str(bob_balance0))
    print("Bob address is " + bob_address)

    bob_name = test_settings["name_register"]["name"]
    register_name(bob_name, bob_address, ext_api, bob_private_key)

    print("Bob has registered " + bob_name)
    bob_balance1 = common.get_account_balance(int_api, pub_key=bob_address).balance
    print("Bob balance is " + str(bob_balance1))

    tokens_to_send = test_settings["spend_tx"]["amount"]
    print("Alice is about to send " + str(tokens_to_send) + " to " + bob_name)
    send_tokens_to_name(bob_name, tokens_to_send, alice_address, alice_private_key, ext_api)

    # validate balances
    alice_balance2 = common.get_account_balance(int_api, pub_key=alice_address).balance
    bob_balance2 = common.get_account_balance(int_api, pub_key=bob_address).balance

    print("Alice balance is " + str(alice_balance2))
    print("Bob balance is " + str(bob_balance2))

    # Alice's balance should be decresed by the amount being send and the fee (1)
    assert_equals(alice_balance2, alice_balance0 - tokens_to_send - 1)

    # Bob's balance should be incresed by the amount being send
    assert_equals(bob_balance2, bob_balance1 + tokens_to_send)

    # stop node
    common.stop_node(node)
    shutil.rmtree(root_dir)

def make_mining_config(root_dir, file_name):
    sys_config = os.path.join(root_dir, file_name)
    f = open(sys_config, "w")
    # if autostart is not true - there will be no miner
    conf ='[{aecore, [{autostart, true},' + \
                    ' {expected_mine_rate, 100},' + \
                    ' {aec_pow_cuckoo, {"mean16s-generic", "-t 5", 16}}]}].'
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

def miner_send_tokens(address, amount, internal_api, external_api): 
    spend_tx_obj = SpendTx(
        recipient_pubkey=address,
        amount=amount,
        fee=1)

    # populate account
    internal_api.post_spend_tx(spend_tx_obj)

    top = external_api.get_top()
    common.wait_until_height(external_api, top.height + 3)

def register_name(name, address, external_api, private_key):
    salt = 42
    commitment = external_api.get_commitment_hash(name, salt).commitment

    # preclaim
    unsigned_preclaim = common.base58_decode(\
        external_api.post_name_preclaim(\
            NamePreclaimTx(commitment=commitment, fee=1, account=address)).tx)
    signed_preclaim = keys.sign_encode_tx(unsigned_preclaim, private_key)

    external_api.post_tx(Tx(tx=signed_preclaim))
    top = external_api.get_top()
    common.wait_until_height(external_api, top.height + 3)

    # claim
    encoded_name = common.encode_name(name)
    unsigned_claim = common.base58_decode(\
        external_api.post_name_claim(\
            NameClaimTx(name=encoded_name, name_salt=salt, fee=1, account=address)).tx)
    signed_claim = keys.sign_encode_tx(unsigned_claim, private_key)

    external_api.post_tx(Tx(tx=signed_claim))
    top = external_api.get_top()
    common.wait_until_height(external_api, top.height + 3)
    name_entry0 = external_api.get_name(name)
    print("Name " + name_entry0.name + " has been claimed and has hash " + name_entry0.name_hash)

    # set pointers
    pointers_str = json.dumps({'account_pubkey': address})
    unsigned_update = common.base58_decode(\
        external_api.post_name_update(\
            NameUpdateTx(name_hash=name_entry0.name_hash, name_ttl=600000, ttl=50,\
                pointers=pointers_str, fee=1, account=address)).tx)
    signed_update = keys.sign_encode_tx(unsigned_update, private_key)

    external_api.post_tx(Tx(tx=signed_update))
    top = external_api.get_top()
    common.wait_until_height(external_api, top.height + 3)
    name_entry = external_api.get_name(name)
    received_pointers = json.loads(name_entry.pointers)
    assert_equals(address, received_pointers['account_pubkey'])

def send_tokens_to_name(name, tokens, sender_address, private_key, external_api):
    name_entry = external_api.get_name(name)
    resolved_address = json.loads(name_entry.pointers)['account_pubkey']
    print("Name " + name + " resolved to address " + resolved_address)

    unsigned_spend = common.base58_decode(\
        external_api.post_spend(\
            SpendTx(sender=sender_address, recipient_pubkey=resolved_address, amount=tokens, fee=1)).tx)
    signed_spend = keys.sign_encode_tx(unsigned_spend, private_key)

    external_api.post_tx(Tx(tx=signed_spend))
    top = external_api.get_top()
    common.wait_until_height(external_api, top.height + 3)
