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

settings = common.test_settings(__name__.split(".")[-1])

def test_contract_create():
    test_settings = settings["test_contract_create"]
    (root_dir, node, external_api, top) = setup_node_with_tokens(test_settings, "node") 
    internal_api = common.internal_api(node)

    send_tokens_to_user("alice", test_settings, internal_api, external_api)

    encoded_tx = get_unsigned_contract_create(test_settings["alice"]["pubkey"], test_settings["create_contract"], external_api)

    print("Unsigned encoded transaction: " + encoded_tx)
    unsigned_tx = common.base58_decode(encoded_tx)
    unpacked_tx = common.unpack_tx(unsigned_tx)
    tx = common.parse_tx(unpacked_tx)
    print("Unsigned decoded transaction: " + str(tx))

    # make sure same tx
    assert_equals(tx['type'], 'contract_create')
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

    signature = bytearray(list(map(int, test_settings["create_contract"]["signature"].split(","))))
    signed = common.encode_signed_tx(unpacked_tx, [signature]) 
    print("Signed transaction " + signed)

    alice_balance0 = common.get_account_balance(internal_api, pub_key=test_settings["alice"]["pubkey"]).balance
    tx_object = Tx(tx=signed)
    external_api.post_tx(tx_object)

    top = external_api.get_top()
    common.wait_until_height(external_api, top.height + 3)
    alice_balance = common.get_account_balance(internal_api, pub_key=test_settings["alice"]["pubkey"]).balance

    assert_equals(alice_balance0, alice_balance + test_settings["create_contract"]["fee"])

    cleanup(node, root_dir)

def test_contract_call():
    test_settings = settings["test_contract_call"]
    create_settings = settings["test_contract_create"]
    (root_dir, node, external_api, top) = setup_node_with_tokens(test_settings, "node") 
    internal_api = common.internal_api(node)

    send_tokens_to_user("alice", test_settings, internal_api, external_api)

    ## create contract
    encoded_tx = get_unsigned_contract_create(test_settings["alice"]["pubkey"], create_settings["create_contract"], external_api)
    unsigned_tx = common.base58_decode(encoded_tx)
    unpacked_tx = common.unpack_tx(unsigned_tx)
    signature = bytearray(list(map(int, create_settings["create_contract"]["signature"].split(","))))
    signed = common.encode_signed_tx(unpacked_tx,[signature]) 

    alice_balance0 = common.get_account_balance(internal_api, pub_key=test_settings["alice"]["pubkey"]).balance
    tx_object = Tx(tx=signed)
    external_api.post_tx(tx_object)

    top = external_api.get_top()
    common.wait_until_height(external_api, top.height + 3)
    alice_balance = common.get_account_balance(internal_api, pub_key=test_settings["alice"]["pubkey"]).balance

    # assert contract created:
    assert_equals(alice_balance0, alice_balance + create_settings["create_contract"]["fee"])

    call_contract = test_settings["contract_call"]
    contract_call_obj = ContractCallData(
        caller=test_settings["alice"]["pubkey"],
        contract=call_contract["contract"],
        vm_version=call_contract["vm_version"],
        fee=call_contract["fee"],
        amount=call_contract["amount"],
        gas=call_contract["gas"],
        gas_price=call_contract["gas_price"],
        call_data=call_contract["call_data"])


    call_tx_obj = external_api.post_contract_call(contract_call_obj)
    encoded_call_tx = call_tx_obj.tx

    print("Unsigned encoded transaction: " + encoded_call_tx)
    unsigned_call_tx = common.base58_decode(encoded_call_tx)
    unpacked_call_tx = common.unpack_tx(unsigned_call_tx)
    tx = common.parse_tx(unpacked_call_tx)
    print("Unsigned decoded transaction: " + str(tx))

    signature = bytearray(list(map(int, test_settings["contract_call"]["signature"].split(","))))

    signed = common.encode_signed_tx(unpacked_call_tx,[signature]) 

    print("Signed transaction: " + signed)
    alice_balance0 = common.get_account_balance(internal_api, pub_key=test_settings["alice"]["pubkey"]).balance
    tx_object = Tx(tx=signed)
    external_api.post_tx(tx_object)

    top = external_api.get_top()
    common.wait_until_height(external_api, top.height + 3)
    alice_balance = common.get_account_balance(internal_api, pub_key=test_settings["alice"]["pubkey"]).balance

    print("BALANCE0 " + str(alice_balance0))
    print("BALANCE " + str(alice_balance))
    # assert contract created:
    assert_equals(alice_balance0, alice_balance + test_settings["contract_call"]["fee"])



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

    # populate the chain so Alice had mined some blocks and has tokens
    # to spend
    blocks_to_mine = test_settings["blocks_to_mine"]
    common.wait_until_height(api, blocks_to_mine)
    top = api.get_top()
    assert_equals(top.height >= blocks_to_mine, True)
    # Now the node has at least blocks_to_mine blocks mined by Alice 

    return (root_dir, node, api, top)


def send_tokens_to_user(user, test_settings, internal_api, external_api):
    spend_tx_obj = SpendTx(
        recipient_pubkey=test_settings[user]["pubkey"],
        amount=test_settings[user]["amount"],
        fee=test_settings[user]["amount"])

    # populate Alice's account
    internal_api.post_spend_tx(spend_tx_obj)

    top = external_api.get_top()
    common.wait_until_height(external_api, top.height + 3)

    balance_obj = common.get_account_balance(internal_api, pub_key=test_settings[user]["pubkey"])
    print(user.capitalize() + "'s balance is now " + str(balance_obj.balance))

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
    return tx_obj.tx
