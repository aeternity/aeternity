# coding: utf-8

import os
from nose.tools import assert_regexp_matches

import common

settings = common.test_settings(__name__.split(".")[-1])

def id_contract():
    currentFile = __file__
    dirPath = os.path.dirname(currentFile)
    return dirPath + "/identity.aes"

def test_encode_calldata():
    bytecode = common.compile_contract(id_contract())
    calldata = common.encode_calldata(id_contract(), "main", "42")

    assert_regexp_matches(bytecode, 'cb_.*')
    assert_regexp_matches(calldata, 'cb_.*')

