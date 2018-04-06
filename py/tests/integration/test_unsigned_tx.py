# coding: utf-8

import tempfile
import os
import shutil
import time
from nose.tools import assert_equals, assert_not_equals, with_setup
import common
from waiting import wait
from swagger_client.rest import ApiException

from swagger_client.models.tx import Tx
from swagger_client.models.spend_tx import SpendTx
from swagger_client.models.contract_create_data import ContractCreateData
from swagger_client.models.contract_call_data import ContractCallData
from swagger_client.models.contract_call_input import ContractCallInput

import keys

settings = common.test_settings(__name__.split(".")[-1])

def test_contract_create():
    test_settings = settings["test_contract_create"]
    (root_dir, node, external_api, top) = setup_node_with_tokens(test_settings, "node") 
    internal_api = common.internal_api(node)

    private_key = keys.new_private()
    public_key = keys.public_key(private_key)

    alice_address = keys.address(public_key)

    test_settings["alice"]["pubkey"] = alice_address
    send_tokens_to_user("alice", test_settings, external_api, internal_api)
    encoded_tx, contract_address = get_unsigned_contract_create(alice_address, test_settings["create_contract"], external_api)

    print("Unsigned encoded transaction: " + encoded_tx)
    print("Contract address: " + contract_address)
    unsigned_tx = common.base58_decode(encoded_tx)
    tx = common.decode_unsigned_tx(unsigned_tx)
    print("Unsigned decoded transaction: " + str(tx))

    # make sure same tx
    assert_equals(tx['type'], 'contract_create_tx')
    assert_equals(tx['owner'], common.base58_decode(test_settings["alice"]["pubkey"]))
    assert_equals(tx['vm_version'], test_settings["create_contract"]["vm_version"])
    assert_equals(tx['deposit'], test_settings["create_contract"]["deposit"])
    assert_equals(tx['amount'], test_settings["create_contract"]["amount"])
    assert_equals(tx['gas'], test_settings["create_contract"]["gas"])
    assert_equals(tx['gas_price'], test_settings["create_contract"]["gas_price"])
    assert_equals(tx['fee'], test_settings["create_contract"]["fee"])

    code = bytearray.fromhex(test_settings["create_contract"]["code"][2:]) # without 0x
    assert_equals(tx['code'], code)

    call_data = bytearray.fromhex(test_settings["create_contract"]["call_data"][2:]) # without 0x
    assert_equals(tx['call_data'], call_data)

    signed = keys.sign_verify_encode_tx(unsigned_tx, private_key, public_key)
    print("Signed transaction " + signed)

    alice_balance0 = common.get_account_balance(external_api, internal_api, pub_key=alice_address).balance
    tx_object = Tx(tx=signed)
    external_api.post_tx(tx_object)

    top = external_api.get_top()
    common.wait_until_height(external_api, top.height + 3)
    alice_balance = common.get_account_balance(external_api, internal_api, pub_key=alice_address).balance

    assert_equals(alice_balance0, alice_balance + test_settings["create_contract"]["fee"])
    print("Fee was consumed, transaction is part of the chain")

    cleanup(node, root_dir)

def test_contract_call():
    test_settings = settings["test_contract_call"]
    create_settings = settings["test_contract_create"]
    (root_dir, node, external_api, top) = setup_node_with_tokens(test_settings, "node") 
    internal_api = common.internal_api(node)

    private_key = keys.new_private()
    public_key = keys.public_key(private_key)

    alice_address = keys.address(public_key)

    test_settings["alice"]["pubkey"] = alice_address
    send_tokens_to_user("alice", test_settings, external_api, internal_api)

    ## create contract
    encoded_tx, contract_address = get_unsigned_contract_create(alice_address, create_settings["create_contract"], external_api)
    unsigned_tx = common.base58_decode(encoded_tx)

    signed = keys.sign_verify_encode_tx(unsigned_tx, private_key, public_key)

    alice_balance0 = common.get_account_balance(external_api, internal_api, pub_key=alice_address).balance
    tx_object = Tx(tx=signed)
    external_api.post_tx(tx_object)

    top = external_api.get_top()
    common.wait_until_height(external_api, top.height + 3)
    alice_balance = common.get_account_balance(external_api, internal_api, pub_key=alice_address).balance

    # assert contract created:
    call_contract = test_settings["contract_call"]
    assert_equals(alice_balance0, alice_balance + create_settings["create_contract"]["fee"])

    call_input = ContractCallInput("ring", create_settings["create_contract"]["code"],\
                                           call_contract["data"]["function"],\
                                           call_contract["data"]["argument"])
    result = external_api.call_contract(call_input)
    contract_call_obj = ContractCallData(
        caller=test_settings["alice"]["pubkey"],
        contract=contract_address,
        vm_version=call_contract["vm_version"],
        fee=call_contract["fee"],
        amount=call_contract["amount"],
        gas=call_contract["gas"],
        gas_price=call_contract["gas_price"],
        call_data=result.out)


    call_tx_obj = external_api.post_contract_call(contract_call_obj)
    encoded_call_tx = call_tx_obj.tx

    print("Unsigned encoded transaction: " + encoded_call_tx)
    unsigned_call_tx = common.base58_decode(encoded_call_tx)

    signed_call = keys.sign_verify_encode_tx(unsigned_call_tx, private_key, public_key)

    print("Signed transaction: " + signed_call)
    alice_balance0 = common.get_account_balance(external_api, internal_api, pub_key=alice_address).balance
    tx_object = Tx(tx=signed_call)
    external_api.post_tx(tx_object)

    top = external_api.get_top()
    common.wait_until_height(external_api, top.height + 3)
    alice_balance = common.get_account_balance(external_api, internal_api, pub_key=alice_address).balance

    # The call runs out of gas and all gas is consumed
    # assert contract called:
    assert_equals(alice_balance0, alice_balance + test_settings["contract_call"]["fee"]
                  + test_settings["contract_call"]["gas"])
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
    (root_dir, node, external_api, top) = setup_node_with_tokens(test_settings, "node") 
    internal_api = common.internal_api(node)

    private_key = keys.new_private()
    public_key = keys.public_key(private_key)

    alice_address = keys.address(public_key)
    bob_address = test_settings["spend_tx"]["recipient"]

    test_settings["alice"]["pubkey"] = alice_address
    send_tokens_to_user("alice", test_settings, external_api, internal_api)

    alice_balance0 = common.get_account_balance(external_api, internal_api, pub_key=alice_address).balance
    bob_balance0 = common.get_account_balance(external_api, internal_api, pub_key=bob_address).balance

    # Alice creates spend tx
    spend_data_obj = SpendTx(
            sender=alice_address,
            recipient_pubkey=bob_address,
            amount=test_settings["spend_tx"]["amount"],
            fee=test_settings["spend_tx"]["fee"])
    unsigned_spend_obj = external_api.post_spend(spend_data_obj)
    unsigned_spend_enc = unsigned_spend_obj.tx
    tx_hash = unsigned_spend_obj.tx_hash
    unsigned_tx = common.base58_decode(unsigned_spend_enc)
    print("Tx hash " + tx_hash)

    # Alice signs spend tx
    signed = keys.sign_verify_encode_tx(unsigned_tx, private_key, public_key)
    print("Signed transaction " + signed)

    # Alice posts spend tx
    tx_object = Tx(tx=signed)
    external_api.post_tx(tx_object)
    _ = external_api.get_tx(tx_hash=tx_hash) # it is there - either in mempool or in a block

    # Wait until spend tx is mined
    wait(lambda: common.get_account_balance(external_api, internal_api, pub_key=alice_address).balance < alice_balance0,
         timeout_seconds=120, sleep_seconds=0.25)
    _ = external_api.get_tx(tx_hash=tx_hash) # it is there - shall be in a block
    print("Tx in chain ")

    # Check that Alice was debited and Bob was credited
    alice_balance = common.get_account_balance(external_api, internal_api, pub_key=alice_address).balance
    bob_balance = common.get_account_balance(external_api, internal_api, pub_key=bob_address).balance
    print("Alice balance now is " + str(alice_balance))
    print("Bob balance now is " + str(bob_balance))
    assert_equals(alice_balance0, alice_balance + test_settings["spend_tx"]["fee"] + test_settings["spend_tx"]["amount"])
    assert_equals(bob_balance0, bob_balance - test_settings["spend_tx"]["amount"])

    # Cleanup
    cleanup(node, root_dir)

def cleanup(node, root_dir):
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

    # populate the chain so node had mined some blocks and has tokens
    # to spend
    blocks_to_mine = test_settings["blocks_to_mine"]
    common.wait_until_height(api, blocks_to_mine)
    top = api.get_top()
    assert_equals(top.height >= blocks_to_mine, True)
    # Now the node has at least blocks_to_mine blocks mined

    return (root_dir, node, api, top)


def send_tokens_to_user(user, test_settings, external_api, internal_api):
    return send_tokens_to_user_(test_settings[user]["pubkey"],
                                test_settings[user]["amount"],
                                test_settings[user]["fee"],
                                external_api,
                                internal_api)

def send_tokens_to_user_(address, tokens, fee, external_api, internal_api):
    def get_balance(k):
        return common.get_account_balance(external_api, internal_api, k).balance
    bal0 = get_balance(address)
    spend_tx_obj = SpendTx(
        recipient_pubkey=address,
        amount=tokens,
        fee=fee)
    internal_api.post_spend_tx(spend_tx_obj)
    wait(lambda: get_balance(address) == (bal0 + tokens),
         timeout_seconds=120, sleep_seconds=0.25)

def get_unsigned_contract_create(owner, contract, external_api):
    contract_create_data_obj = ContractCreateData(
        owner=owner,
        code=contract["code"],
        vm_version=contract["vm_version"],
        deposit=contract["deposit"],
        amount=contract["amount"],
        gas=contract["gas"],
        gas_price=contract["gas_price"],
        fee=contract["fee"],
        call_data=contract["call_data"])

    tx_obj = external_api.post_contract_create(contract_create_data_obj)
    return (tx_obj.tx, tx_obj.contract_address)
