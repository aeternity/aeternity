# coding: utf-8

import tempfile
import os
import shutil
import time
from nose.tools import assert_equals, assert_not_equals, with_setup
import common
from waiting import wait
from swagger_client.models.ping import Ping 
from swagger_client.models.contract import Contract
from swagger_client.models.contract_call_input import ContractCallInput
from swagger_client.rest import ApiException

settings = common.test_settings(__name__.split(".")[-1])

def test_compile_id():
    # Alice should be able to compile a ring contract with an identity
    # function into hex string encoded bytecode.
    test_settings = settings["test_compile_id"]
    (root_dir, node, api) = setup_node(test_settings, "alice")

    # Read a contract
    currentFile = __file__
    dirPath = os.path.dirname(currentFile)
    contract_file = open(dirPath + "/identity.aer", "r")
    contract_string = contract_file.read()

    # Call the compile function
    contract = Contract( contract_string, "")
    result = api.compile_contract(contract)

    bytecode = '0x36600080376200002160008080805180516004146200003057505b5060011951005b60005260206000f35b80905090565b602001517f6d61696e000000000000000000000000000000000000000000000000000000001462000061576200001a565b602001519050809150506200002a56'
    assert_equals(result['bytecode'], bytecode)

    # stop node
    common.stop_node(node)
    shutil.rmtree(root_dir)

def test_encode_id_call():
    # Alice should be able to encode a call to a function in
    # a ring contract.
    
    test_settings = settings["test_encode_id_call"]
    (root_dir, node, api) = setup_node(test_settings, "alice")

    bytecode = '0x36600080376200002160008080805180516004146200003057505b5060011951005b60005260206000f35b80905090565b602001517f6d61696e000000000000000000000000000000000000000000000000000000001462000061576200001a565b602001519050809150506200002a56'

    call_input = ContractCallInput(bytecode, "main", "42")
    result = api.encode_calldata(call_input)

    calldata = '0x0000000000000000000000000000000000000000000000000000000000000040000000000000000000000000000000000000000000000000000000000000002a00000000000000000000000000000000000000000000000000000000000000046d61696e00000000000000000000000000000000000000000000000000000000'
    assert_equals(result.calldata, calldata)
    
    # stop node
    common.stop_node(node)
    shutil.rmtree(root_dir)

def test_id_call():
    # Alice should be able to call the id function in
    # an Id contract.
    
    test_settings = settings["test_id_call"]
    (root_dir, node, api) = setup_node(test_settings, "alice")

    bytecode = '0x36600080376200002160008080805180516004146200003057505b5060011951005b60005260206000f35b80905090565b602001517f6d61696e000000000000000000000000000000000000000000000000000000001462000061576200001a565b602001519050809150506200002a56'

    call_input = ContractCallInput(bytecode, "main", "42")
    result = api.call_contract(call_input)
    print(result)

    retval = '0x000000000000000000000000000000000000000000000000000000000000002a'
    assert_equals(result.out, retval)
    # stop node
    common.stop_node(node)
    shutil.rmtree(root_dir)
    

    
def make_mining_config(root_dir, file_name):
    sys_config = os.path.join(root_dir, file_name)
    f = open(sys_config, "w")
    # if autostart is not true - there will be no miner
    # We dont need a miner for running off-chain contracts.
    conf ='[{aecore, [{autostart, false},' + \
                    ' {expected_mine_rate, 100},' + \
                    ' {aec_pow_cuckoo, {"mean16s-generic", "-t 5", 16}}]}].'
    f.write(conf)
    f.close()
    return sys_config
    
def setup_node(test_settings, node_name):
    # prepare a dir to hold the configs and the keys
    root_dir = tempfile.mkdtemp()

    # setup the dir with Alice's node mining
    node = test_settings["nodes"][node_name]
    sys_config = make_mining_config(root_dir, "sys.config")
    common.start_node(node, sys_config)
    api = common.external_api(node)

    return (root_dir, node, api)
