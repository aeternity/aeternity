# coding: utf-8

import os
import shutil
from nose.tools import assert_equals, assert_not_equals, with_setup
import common
import json
from swagger_client.models.tx import Tx
from swagger_client.models.spend_tx import SpendTx
from swagger_client.models.name_preclaim_tx import NamePreclaimTx
from swagger_client.models.name_claim_tx import NameClaimTx
from swagger_client.models.name_update_tx import NameUpdateTx

import keys

settings = common.test_settings(__name__.split(".")[-1])

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
    (bob_node, (root_dir, bob_api, bob_top)) = setup_node_with_tokens(test_settings, "bob")
    bob_internal_api = common.internal_api(bob_node)

    def get_bob_balance(height):
        return common.get_account_balance_at_height(bob_api, bob_internal_api, height)
    def get_alice_balance(height):
        k = test_settings["spend_tx"]["alice_pubkey"]
        return common.get_account_balance_at_height(bob_api, bob_internal_api, height, pub_key=k)

    spend_tx_amt = test_settings["spend_tx"]["amount"]
    spend_tx_fee = test_settings["spend_tx"]["fee"]
    bob_balance = get_bob_balance(bob_top.height)
    bob_has_not_enough_tokens = bob_balance.balance < spend_tx_amt + spend_tx_fee
    assert_equals(bob_has_not_enough_tokens, True)
    print("Bob initial balance is " + str(bob_balance.balance) +
            " and he will try to spend " + str(spend_tx_amt + spend_tx_fee))

    alice_balance0 = get_alice_balance(bob_top.height)

    # Bob tries to send some tokens to Alice
    spend_tx_obj = SpendTx(
        recipient_pubkey=test_settings["spend_tx"]["alice_pubkey"],
        amount=spend_tx_amt,
        fee=spend_tx_fee,
        ttl=100,
        payload="foo")
    print("Bob's spend_tx is " + str(spend_tx_obj))
    bob_internal_api.post_spend_tx(spend_tx_obj)
    print("Transaction sent")
    bob_top_after_tx = bob_api.get_top_block()

    print("Waiting for a few blocks to be mined")
    checked_height = bob_top_after_tx.height + 3
    common.wait_until_height(bob_api, checked_height)

    # ensure Alice balance had not changed
    alice_balance1 = get_alice_balance(checked_height)
    print("Alice's balance is now " + str(alice_balance1.balance))
    assert_equals(alice_balance1.balance, alice_balance0.balance)

    # ensure Bob not debited
    # Since Bob had mined the block he is receiving the coinbase_reward
    blocks_mined = checked_height - bob_top.height
    print("Coinbase reward is " + str(coinbase_reward) + ", had mined " +
          str(blocks_mined) + " blocks")
    expected_balance = bob_balance.balance + coinbase_reward * blocks_mined
    bob_new_balance = get_bob_balance(checked_height)
    print("Bob's balance (with coinbase rewards) is now " + str(bob_new_balance.balance))
    assert_equals(bob_new_balance.balance, expected_balance)

    # stop node
    common.stop_node(bob_node)
    shutil.rmtree(root_dir)

def test_send_by_name():
    # Bob registers a name 'bob.aet'
    # Alice should be able to send tokens to Bob using that name
    test_settings = settings["test_send_by_name"]
    (node, (root_dir, ext_api, top)) = setup_node_with_tokens(test_settings, "miner")
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
    alice_balance0 = common.get_account_balance(ext_api, int_api, pub_key=alice_address).balance
    bob_balance0 = common.get_account_balance(ext_api, int_api, pub_key=bob_address).balance

    assert_equals(alice_balance0, alice_init_balance)
    assert_equals(bob_balance0, bob_init_balance)
    print("Alice balance is " + str(alice_balance0))
    print("Bob balance is " + str(bob_balance0))
    print("Bob address is " + bob_address)

    bob_name = test_settings["name_register"]["name"]
    register_name(bob_name, bob_address, ext_api, bob_private_key)

    print("Bob has registered " + bob_name)
    bob_balance1 = common.get_account_balance(ext_api, int_api, pub_key=bob_address).balance
    print("Bob balance is " + str(bob_balance1))

    tokens_to_send = test_settings["spend_tx"]["amount"]
    print("Alice is about to send " + str(tokens_to_send) + " to " + bob_name)
    send_tokens_to_name(bob_name, tokens_to_send, alice_address, alice_private_key, ext_api)

    # validate balances
    alice_balance2 = common.get_account_balance(ext_api, int_api, pub_key=alice_address).balance
    bob_balance2 = common.get_account_balance(ext_api, int_api, pub_key=bob_address).balance

    print("Alice balance is " + str(alice_balance2))
    print("Bob balance is " + str(bob_balance2))

    # Alice's balance should be decresed by the amount being send and the fee (1)
    assert_equals(alice_balance2, alice_balance0 - tokens_to_send - 1)

    # Bob's balance should be incresed by the amount being send
    assert_equals(bob_balance2, bob_balance1 + tokens_to_send)

    # stop node
    common.stop_node(node)
    shutil.rmtree(root_dir)

def setup_node_with_tokens(test_settings, node_name):
    node = test_settings["nodes"][node_name]
    return node, common.setup_node_with_tokens(node, test_settings["blocks_to_mine"])

def miner_send_tokens(address, amount, internal_api, external_api):
    spend_tx_obj = SpendTx(
        recipient_pubkey=address,
        amount=amount,
        fee=1,
        ttl=100,
        payload="sending tokens")

    # populate account
    internal_api.post_spend_tx(spend_tx_obj)

    top = external_api.get_top_block()
    common.wait_until_height(external_api, top.height + 3)

def register_name(name, address, external_api, private_key):
    salt = 42
    commitment = external_api.get_commitment_hash(name, salt).commitment

    # preclaim
    unsigned_preclaim = common.base58_decode(\
        external_api.post_name_preclaim(\
            NamePreclaimTx(commitment=commitment, fee=1, ttl=100, account=address)).tx)
    signed_preclaim = keys.sign_encode_tx(unsigned_preclaim, private_key)

    external_api.post_tx(Tx(tx=signed_preclaim))
    top = external_api.get_top_block()
    common.wait_until_height(external_api, top.height + 3)

    # claim
    encoded_name = common.encode_name(name)
    unsigned_claim = common.base58_decode(\
        external_api.post_name_claim(\
            NameClaimTx(name=encoded_name, name_salt=salt, fee=1, ttl=100, account=address)).tx)
    signed_claim = keys.sign_encode_tx(unsigned_claim, private_key)

    external_api.post_tx(Tx(tx=signed_claim))
    top = external_api.get_top_block()
    common.wait_until_height(external_api, top.height + 3)
    name_entry0 = external_api.get_name(name)
    print("Name " + name_entry0.name + " has been claimed and has hash " + name_entry0.name_hash)

    # set pointers
    pointers_str = json.dumps({'account_pubkey': address})
    unsigned_update = common.base58_decode(\
        external_api.post_name_update(\
            NameUpdateTx(name_hash=name_entry0.name_hash, name_ttl=6000, client_ttl=50,\
                pointers=pointers_str, fee=1, ttl=100, account=address)).tx)
    signed_update = keys.sign_encode_tx(unsigned_update, private_key)

    external_api.post_tx(Tx(tx=signed_update))
    top = external_api.get_top_block()
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
            SpendTx(sender=sender_address, recipient_pubkey=resolved_address, amount=tokens, fee=1,\
                    ttl=100, payload="foo")).tx)
    signed_spend = keys.sign_encode_tx(unsigned_spend, private_key)

    external_api.post_tx(Tx(tx=signed_spend))
    top = external_api.get_top_block()
    common.wait_until_height(external_api, top.height + 3)
