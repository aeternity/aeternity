# coding: utf-8

import tempfile
import os
import shutil
import time
from nose.tools import assert_equals, assert_not_equals, assert_regexp_matches, with_setup
import common
from waiting import wait
from swagger_client.rest import ApiException

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

