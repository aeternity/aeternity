# coding: utf-8

import shutil
from nose.tools import assert_equals

import common
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
    beneficiary = common.setup_beneficiary()
    (node, (root_dir, ext_api, int_api, top)) = setup_node_with_tokens(test_settings, beneficiary, "miner")

    alice_address = keys.address(keys.public_key(keys.new_private()))

    bob_private_key = keys.new_private()
    bob_public_key = keys.public_key(bob_private_key)
    bob_address = keys.address(bob_public_key)
    bob = {'privk': bob_private_key, 'enc_pubk': bob_address}

    # initial balances - amounts that the miner should send them
    alice_init_balance = test_settings["send_tokens"]["alice"]
    bob_init_balance = test_settings["send_tokens"]["bob"]

    # populate accounts with tokens, and validate balances
    spend_tx_fee = test_settings["spend_tx"]["fee"]
    common.ensure_send_tokens(beneficiary, alice_address, alice_init_balance, spend_tx_fee, ext_api, int_api, 1)
    common.ensure_send_tokens(beneficiary, bob_address, bob_init_balance, spend_tx_fee, ext_api, int_api, 1)
    alice_balance0 = common.get_account_balance(ext_api, alice_address)
    bob_balance0 = common.get_account_balance(ext_api, bob_address)
    print("Alice balance is " + str(alice_balance0))
    print("Bob balance is " + str(bob_balance0))
    assert_equals(alice_balance0, alice_init_balance)
    assert_equals(bob_balance0, bob_init_balance)

    # check that Bob is able to send less tokens than he has
    few_tokens_to_send = test_settings["spend_tx"]["small_amount"]
    print("Bob is about to send " + str(few_tokens_to_send) + " to Alice")
    common.ensure_send_tokens(bob, alice_address, few_tokens_to_send, spend_tx_fee, ext_api, int_api, 1)
    alice_balance1 = common.get_account_balance(ext_api, pub_key=alice_address)
    bob_balance1 = common.get_account_balance(ext_api, pub_key=bob_address)
    print("Alice balance is " + str(alice_balance1))
    print("Bob balance is " + str(bob_balance1))
    assert_equals(alice_balance1, alice_balance0 + few_tokens_to_send)
    assert_equals(bob_balance1, bob_balance0 - (few_tokens_to_send + spend_tx_fee))

    # check that Bob is unable to send less tokens than he has
    many_tokens_to_send = test_settings["spend_tx"]["large_amount"]
    print("Bob is about to send " + str(many_tokens_to_send) + " to Alice")
    common.send_tokens(bob, alice_address, many_tokens_to_send, spend_tx_fee, ext_api, int_api)
    current_key_block = ext_api.GetCurrentKeyBlock().response().result
    common.wait_until_height(ext_api, current_key_block.height + 3)
    alice_balance2 = common.get_account_balance(ext_api, pub_key=alice_address)
    bob_balance2 = common.get_account_balance(ext_api, pub_key=bob_address)
    print("Alice balance is " + str(alice_balance2))
    print("Bob balance is " + str(bob_balance2))
    assert_equals(alice_balance2, alice_balance1)
    assert_equals(bob_balance2, bob_balance1)

    # stop node
    common.stop_node(node)
    shutil.rmtree(root_dir)

def test_send_by_name():
    # Bob registers a name 'bob.test'
    # Alice should be able to send tokens to Bob using that name
    test_settings = settings["test_send_by_name"]
    beneficiary = common.setup_beneficiary()
    (node, (root_dir, ext_api, int_api, top)) = setup_node_with_tokens(test_settings, beneficiary, "miner")

    alice_private_key = keys.new_private()
    alice_public_key = keys.public_key(alice_private_key)
    alice_address = keys.address(alice_public_key)
    alice = {'privk': alice_private_key, 'enc_pubk': alice_address}

    bob_private_key = keys.new_private()
    bob_public_key = keys.public_key(bob_private_key)
    bob_address = keys.address(bob_public_key)

    # initial balances - amounts that the miner should send them
    alice_init_balance = test_settings["send_tokens"]["alice"]
    bob_init_balance = test_settings["send_tokens"]["bob"]
    spend_fee = test_settings["send_tokens"]["spend_fee"]

    # populate accounts with tokens
    common.ensure_send_tokens(beneficiary, alice_address, alice_init_balance, spend_fee, ext_api, int_api, 1)
    common.ensure_send_tokens(beneficiary, bob_address, bob_init_balance, spend_fee, ext_api, int_api, 1)

    # validate balances
    alice_balance0 = common.get_account_balance(ext_api, alice_address)
    bob_balance0 = common.get_account_balance(ext_api, bob_address)

    assert_equals(alice_balance0, alice_init_balance)
    assert_equals(bob_balance0, bob_init_balance)
    print("Alice balance is " + str(alice_balance0))
    print("Bob balance is " + str(bob_balance0))
    print("Bob address is " + bob_address)

    bob_name = test_settings["name_register"]["name"]
    fee = test_settings["name_register"]["fee"]
    register_name(bob_name, bob_address, ext_api, int_api, bob_private_key,
                  fee)

    print("Bob has registered " + bob_name)
    bob_balance1 = common.get_account_balance(ext_api, bob_address)
    print("Bob balance is " + str(bob_balance1))

    tokens_to_send = test_settings["spend_tx"]["amount"]
    print("Alice is about to send " + str(tokens_to_send) + " to " + bob_name)
    resolved_address = get_address_by_name(bob_name, ext_api)
    common.ensure_send_tokens(alice, resolved_address, tokens_to_send, spend_fee, ext_api, int_api, 1)

    # validate balances
    alice_balance2 = common.get_account_balance(ext_api, alice_address)
    bob_balance2 = common.get_account_balance(ext_api, bob_address)

    print("Alice balance is " + str(alice_balance2))
    print("Bob balance is " + str(bob_balance2))

    # Alice's balance should be decresed by the amount being send and the fee (1)
    assert_equals(alice_balance2, alice_balance0 - tokens_to_send - spend_fee)

    # Bob's balance should be incresed by the amount being send
    assert_equals(bob_balance2, bob_balance1 + tokens_to_send)

    # stop node
    common.stop_node(node)
    shutil.rmtree(root_dir)

def setup_node_with_tokens(test_settings, beneficiary, node_name):
    node = test_settings["nodes"][node_name]
    return node, common.setup_node_with_tokens(node, beneficiary, test_settings["blocks_to_mine"])

def register_name(name, address, external_api, internal_api, private_key, fee):
    salt = 42
    commitment_id = internal_api.GetCommitmentId(name=name, salt=salt).response().result.commitment_id

    # preclaim
    NamePreclaimTx = internal_api.get_model('NamePreclaimTx')
    preclaim_tx = NamePreclaimTx(commitment_id=commitment_id, fee=fee, ttl=100, account_id=address)
    encoded_preclaim = internal_api.PostNamePreclaim(body=preclaim_tx).response().result.tx
    unsigned_preclaim = common.api_decode(encoded_preclaim)
    signed_preclaim = keys.sign_encode_tx(unsigned_preclaim, private_key)
    common.ensure_transaction_posted(external_api, signed_preclaim)

    # claim
    encoded_name = common.encode_name(name)
    NameClaimTx = internal_api.get_model('NameClaimTx')
    claim_tx = NameClaimTx(name=encoded_name, name_salt=salt, fee=fee, ttl=100, account_id=address)
    encoded_claim = internal_api.PostNameClaim(body=claim_tx).response().result.tx
    unsigned_claim = common.api_decode(encoded_claim)
    signed_claim = keys.sign_encode_tx(unsigned_claim, private_key)
    common.ensure_transaction_posted(external_api, signed_claim)
    name_entry0 = external_api.GetNameEntryByName(name=name).response().result

    # set pointers
    NameUpdateTx = internal_api.get_model('NameUpdateTx')
    NamePointer = internal_api.get_model('NamePointer')
    pointers = [ NamePointer(key='account_pubkey', id=address) ]
    name_update_tx = NameUpdateTx(name_id=name_entry0.id, name_ttl=6000, client_ttl=50,\
                pointers=pointers, fee=fee, ttl=100, account_id=address)

    encoded_name_update = internal_api.PostNameUpdate(body=name_update_tx).response().result.tx
    unsigned_update = common.api_decode(encoded_name_update)
    signed_update = keys.sign_encode_tx(unsigned_update, private_key)
    common.ensure_transaction_posted(external_api, signed_update)
    name_entry = external_api.GetNameEntryByName(name=name).response().result
    received_pointers = name_entry.pointers[0]
    assert_equals('account_pubkey', received_pointers.key)
    assert_equals(address, received_pointers.id)

def get_address_by_name(name, ext_api):
    name_entry = ext_api.GetNameEntryByName(name=name).response().result
    resolved_address = name_entry.pointers[0].id
    print("Name " + name + " resolved to address " + resolved_address)
    return resolved_address
