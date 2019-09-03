# coding: utf-8

import os
import shutil
from nose.tools import assert_equals
from waiting import wait

import common
import keys

settings = common.test_settings(__name__.split(".")[-1])

def id_contract():
    currentFile = __file__
    dirPath = os.path.dirname(currentFile)
    return dirPath + "/identity.aes"

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
    assert_equals(tx['abi_version'], test_settings["create_contract"]["abi_version"])
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
                  + test_settings["create_contract"]["gas_used"] * test_settings["create_contract"]["gas_price"]
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
                  + create_settings["create_contract"]["gas_used"] * create_settings["create_contract"]["gas_price"]
                  + create_settings["create_contract"]["deposit"]
                  + create_settings["create_contract"]["amount"])

    bytecode = common.compile_contract(id_contract())
    calldata = common.encode_calldata(id_contract(), call_contract["data"]["function"], call_contract["data"]["argument"])
    ContractCallTx = internal_api.get_model('ContractCallTx')
    contract_call_obj = ContractCallTx(
        caller_id=test_settings["alice"]["pubkey"],
        contract_id=encoded_contract_id,
        abi_version=call_contract["abi_version"],
        fee=call_contract["fee"],
        ttl=100,
        amount=call_contract["amount"],
        gas=call_contract["gas"],
        gas_price=call_contract["gas_price"],
        call_data=calldata)


    call_tx_obj = internal_api.PostContractCall(body=contract_call_obj).response().result
    encoded_call_tx = call_tx_obj.tx

    print("Unsigned encoded transaction: " + encoded_call_tx)
    unsigned_call_tx = common.api_decode(encoded_call_tx)

    signed_call = keys.sign_verify_encode_tx(unsigned_call_tx, private_key, public_key)

    print("Signed transaction: " + signed_call)
    alice_balance0 = common.get_account_balance(external_api, alice_address)
    common.ensure_transaction_posted(external_api, signed_call)
    alice_balance = common.get_account_balance(external_api, alice_address)

    assert_equals(alice_balance0, alice_balance
                  + test_settings["contract_call"]["fee"]
                  + test_settings["contract_call"]["amount"]
                  + test_settings["contract_call"]["gas_used"] * test_settings["contract_call"]["gas_price"])
    print("Fee and gas was consumed, transaction is part of the chain")

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
    SpendTx = internal_api.get_model('SpendTx')
    spend_data_obj = SpendTx(
            sender_id=alice_address,
            recipient_id=bob_address,
            amount=test_settings["spend_tx"]["amount"],
            fee=test_settings["spend_tx"]["fee"],
            ttl=100,
            payload="foo")
    unsigned_spend_obj = internal_api.PostSpend(body=spend_data_obj).response().result
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
    return common.ensure_send_tokens(beneficiary,
                                     test_settings[user]["pubkey"],
                                     test_settings[user]["amount"],
                                     test_settings[user]["fee"],
                                     external_api,
                                     internal_api,
                                     1)

def get_unsigned_contract_create(owner_id, contract, external_api, internal_api):
    bytecode = common.compile_contract(id_contract())
    calldata = common.encode_calldata(id_contract(), contract["function"], contract["argument"])

    print("OWNERID", owner_id)
    ContractCreateTx = internal_api.get_model('ContractCreateTx')
    contract_create_tx_obj = ContractCreateTx(
        owner_id=owner_id,
        code=bytecode,
        vm_version=contract["vm_version"],
        abi_version=contract["abi_version"],
        deposit=contract["deposit"],
        amount=contract["amount"],
        gas=contract["gas"],
        gas_price=contract["gas_price"],
        fee=contract["fee"],
        ttl=100,
        call_data=calldata)
    tx_obj = internal_api.PostContractCreate(body=contract_create_tx_obj).response().result
    return (tx_obj.tx, tx_obj.contract_id)
