# coding: utf-8

import tempfile
import os
import shutil
import time
from nose.tools import assert_equals, assert_not_equals,  assert_regexp_matches, with_setup
import common
from waiting import wait
from swagger_client.rest import ApiException

from swagger_client.models.tx import Tx
from swagger_client.models.spend_tx import SpendTx
from swagger_client.models.contract import Contract
from swagger_client.models.contract_create_tx import ContractCreateTx
from swagger_client.models.contract_call_tx import ContractCallTx
from swagger_client.models.contract_call_input import ContractCallInput

import keys

settings = common.test_settings(__name__.split(".")[-1])

def read_id_contract(api):
    # Read a contract
    currentFile = __file__
    dirPath = os.path.dirname(currentFile)
    contract_file = open(dirPath + "/identity.aes", "r")
    contract_string = contract_file.read()

    # Compile contract to bytecode
    contract = Contract( contract_string, "")
    compilation_result = api.compile_contract(contract)
    assert_regexp_matches(compilation_result.bytecode, 'cb_.*')

    return compilation_result.bytecode


def test_contract_create():
    test_settings = settings["test_contract_create"]
    beneficiary = common.setup_beneficiary()
    (node, (root_dir, external_api, internal_api, top)) = setup_node_with_tokens(test_settings, beneficiary, "node")

    private_key = keys.new_private()
    public_key = keys.public_key(private_key)

    alice_address = keys.address(public_key)

    test_settings["alice"]["pubkey"] = alice_address
    send_tokens_to_user(beneficiary, "alice", test_settings, external_api, internal_api)
    encoded_tx, contract_id = get_unsigned_contract_create(alice_address, test_settings["create_contract"], external_api, internal_api)

    print("Unsigned encoded transaction: " + encoded_tx)
    print("Contract id: " + contract_id)
    unsigned_tx = common.api_decode(encoded_tx)
    tx = common.decode_unsigned_tx(unsigned_tx)
    print("Unsigned decoded transaction: " + str(tx))

    # make sure same tx
    assert_equals(tx['type'], 'contract_create_tx')
    assert_equals(tx['owner_id'], common.api_decode(test_settings["alice"]["pubkey"]))
    assert_equals(tx['vm_version'], test_settings["create_contract"]["vm_version"])
    assert_equals(tx['deposit'], test_settings["create_contract"]["deposit"])
    assert_equals(tx['amount'], test_settings["create_contract"]["amount"])
    assert_equals(tx['gas'], test_settings["create_contract"]["gas"])
    assert_equals(tx['gas_price'], test_settings["create_contract"]["gas_price"])
    assert_equals(tx['fee'], test_settings["create_contract"]["fee"])

    signed = keys.sign_verify_encode_tx(unsigned_tx, private_key, public_key)
    print("Signed transaction " + signed)

    alice_balance0 = common.get_account_balance(external_api, alice_address)
    common.ensure_transaction_posted(external_api, signed)
    alice_balance = common.get_account_balance(external_api, alice_address)

    assert_equals(alice_balance0,
                  alice_balance
                  + test_settings["create_contract"]["fee"]
                  + test_settings["create_contract"]["gas_used"]
                  + test_settings["create_contract"]["deposit"]
                  + test_settings["create_contract"]["amount"])

    print("Fee was consumed, transaction is part of the chain")

    cleanup(node, root_dir)

def test_contract_call():
    test_settings = settings["test_contract_call"]
    create_settings = settings["test_contract_create"]
    beneficiary = common.setup_beneficiary()
    (node, (root_dir, external_api, internal_api, top)) = setup_node_with_tokens(test_settings, beneficiary, "node")


    private_key = keys.new_private()
    public_key = keys.public_key(private_key)

    alice_address = keys.address(public_key)

    test_settings["alice"]["pubkey"] = alice_address
    send_tokens_to_user(beneficiary, "alice", test_settings, external_api, internal_api)

    ## create contract
    encoded_tx, encoded_contract_id = get_unsigned_contract_create(alice_address, create_settings["create_contract"], external_api, internal_api)
    unsigned_tx = common.api_decode(encoded_tx)
    signed = keys.sign_verify_encode_tx(unsigned_tx, private_key, public_key)

    alice_balance0 = common.get_account_balance(external_api, alice_address)
    common.ensure_transaction_posted(external_api, signed)
    alice_balance = common.get_account_balance(external_api, pub_key=alice_address)

    # assert contract created:
    call_contract = test_settings["contract_call"]
    assert_equals(alice_balance0,
                  alice_balance
                  + create_settings["create_contract"]["fee"]
                  + create_settings["create_contract"]["gas_used"]
                  + create_settings["create_contract"]["deposit"]
                  + create_settings["create_contract"]["amount"])

    bytecode = read_id_contract(internal_api)
    call_input = ContractCallInput("sophia", bytecode,
                                             call_contract["data"]["function"],
                                             call_contract["data"]["argument"])
    result = internal_api.call_contract(call_input)
    contract_call_obj = ContractCallTx(
        caller_id=test_settings["alice"]["pubkey"],
        contract_id=encoded_contract_id,
        vm_version=call_contract["vm_version"],
        fee=call_contract["fee"],
        ttl=100,
        amount=call_contract["amount"],
        gas=call_contract["gas"],
        gas_price=call_contract["gas_price"],
        call_data=result.out)


    call_tx_obj = internal_api.post_contract_call(contract_call_obj)
    encoded_call_tx = call_tx_obj.tx

    print("Unsigned encoded transaction: " + encoded_call_tx)
    unsigned_call_tx = common.api_decode(encoded_call_tx)

    signed_call = keys.sign_verify_encode_tx(unsigned_call_tx, private_key, public_key)

    print("Signed transaction: " + signed_call)
    alice_balance0 = common.get_account_balance(external_api, alice_address)
    common.ensure_transaction_posted(external_api, signed_call)
    alice_balance = common.get_account_balance(external_api, alice_address)

    # The call runs out of gas and all gas is consumed
    # assert contract called:
    assert_equals(alice_balance0, alice_balance
                  + test_settings["contract_call"]["fee"]
                  + test_settings["contract_call"]["gas"]
                  + test_settings["contract_call"]["amount"])
    print("Fee and gas was consumed, transaction is part of the chain")

    cleanup(node, root_dir)


def test_contract_on_chain_call_off_chain():
    test_settings = settings["test_contract_call"]
    create_settings = settings["test_contract_create"]
    beneficiary = common.setup_beneficiary()
    (node, (root_dir, external_api, internal_api, top)) = setup_node_with_tokens(test_settings, beneficiary, "node")

    private_key = keys.new_private()
    public_key = keys.public_key(private_key)

    alice_address = keys.address(public_key)

    test_settings["alice"]["pubkey"] = alice_address
    send_tokens_to_user(beneficiary, "alice", test_settings, external_api, internal_api)

    ## create contract
    encoded_tx, encoded_contract_id = get_unsigned_contract_create(alice_address, create_settings["create_contract"], external_api, internal_api)
    unsigned_tx = common.api_decode(encoded_tx)

    signed = keys.sign_verify_encode_tx(unsigned_tx, private_key, public_key)
    common.ensure_transaction_posted(external_api, signed)

    call_contract = test_settings["contract_call"]
    call_input = ContractCallInput("sophia-address", encoded_contract_id,\
                                   call_contract["data"]["function"],\
                                   call_contract["data"]["argument"])
    result = internal_api.call_contract(call_input)

    assert_equals(common.hexstring_to_contract_bytearray('0x000000000000000000000000000000000000000000000000000000000000002a'),
                   result.out)

    cleanup(node, root_dir)


def test_spend():
    # Alice should be able to create a spend transaction to send tokens to
    # Bob. In a controlled environment, Alice should see her transaction
    # appear in the next block or two that are added to the blockchain.
    #
    # Once that happens, Alice should see her account debited and Bob's
    # account credited with the same number of tokens.
    #
    # The debit/credit should not happen until the transaction is confirmed,
    # e.g. included in at least one block in the chain.

    # Setup
    test_settings = settings["test_spend"]
    beneficiary = common.setup_beneficiary()
    (node, (root_dir, external_api, internal_api, top)) = setup_node_with_tokens(test_settings, beneficiary, "node")

    private_key = keys.new_private()
    public_key = keys.public_key(private_key)
    alice_address = keys.address(public_key)
    test_settings["alice"]["pubkey"] = alice_address
    bob_address = test_settings["spend_tx"]["recipient"]

    send_tokens_to_user(beneficiary, "alice", test_settings, external_api, internal_api)

    alice_balance0 = common.get_account_balance(external_api, alice_address)
    bob_balance0 = common.get_account_balance(external_api, bob_address)
    print("Alice balance at start is " + str(alice_balance0))
    print("Bob balance at start is " + str(bob_balance0))

    # Alice creates spend tx
    print("Tx amount " + str(test_settings["spend_tx"]["amount"]))
    print("Tx fee " + str(test_settings["spend_tx"]["fee"]))
    spend_data_obj = SpendTx(
            sender_id=alice_address,
            recipient_id=bob_address,
            amount=test_settings["spend_tx"]["amount"],
            fee=test_settings["spend_tx"]["fee"],
            ttl=100,
            payload="foo")
    unsigned_spend_obj = internal_api.post_spend(spend_data_obj)
    unsigned_spend_enc = unsigned_spend_obj.tx
    unsigned_tx = common.api_decode(unsigned_spend_enc)

    # Alice signs spend tx
    signed = keys.sign_verify_encode_tx(unsigned_tx, private_key, public_key)

    # Alice posts spend tx
    common.ensure_transaction_posted(external_api, signed)

    # Check that Alice was debited and Bob was credited
    alice_balance = common.get_account_balance(external_api, alice_address)
    bob_balance = common.get_account_balance(external_api, bob_address)
    print("Alice balance now is " + str(alice_balance))
    print("Bob balance now is " + str(bob_balance))
    assert_equals(alice_balance0, alice_balance + test_settings["spend_tx"]["fee"] + test_settings["spend_tx"]["amount"])
    assert_equals(bob_balance0, bob_balance - test_settings["spend_tx"]["amount"])

    # Cleanup
    cleanup(node, root_dir)

def cleanup(node, root_dir):
    common.stop_node(node)
    shutil.rmtree(root_dir)

def setup_node_with_tokens(test_settings, beneficiary, node_name):
    node = test_settings["nodes"][node_name]
    return node, common.setup_node_with_tokens(node, beneficiary, test_settings["blocks_to_mine"])

def send_tokens_to_user(beneficiary, user, test_settings, external_api, internal_api):
    return common.send_tokens_to_unchanging_user_and_wait_balance(beneficiary,
                                                                  test_settings[user]["pubkey"],
                                                                  test_settings[user]["amount"],
                                                                  test_settings[user]["fee"],
                                                                  external_api,
                                                                  internal_api)

def get_unsigned_contract_create(owner_id, contract, external_api, internal_api):
    bytecode = read_id_contract(internal_api)
    call_input = ContractCallInput("sophia",
                                   bytecode,
                                   contract["function"],
                                   contract["argument"])
    print("Call input:", call_input)
    result = internal_api.encode_calldata(call_input)
    call_data = result.calldata

    print("OWNERID", owner_id)
    contract_create_tx_obj = ContractCreateTx(
        owner_id=owner_id,
        code=bytecode,
        vm_version=contract["vm_version"],
        deposit=contract["deposit"],
        amount=contract["amount"],
        gas=contract["gas"],
        gas_price=contract["gas_price"],
        fee=contract["fee"],
        ttl=100,
        call_data=call_data)
    tx_obj = internal_api.post_contract_create(contract_create_tx_obj)
    return (tx_obj.tx, tx_obj.contract_id)
