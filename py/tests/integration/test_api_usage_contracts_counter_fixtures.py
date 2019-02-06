# nose doctest fixtures
import os

def globs(globs):
    def read_file(name):
        with open(name, 'rt') as f:
            return f.read()
    currentFile = __file__
    dirPath = os.path.dirname(currentFile)
    globs['counter_contract'] = read_file(dirPath + "/counter.aes")
    return globs

def setup_test(test):
    import common
    import keys
    test_settings = _test_settings_(test)
    node = test_settings['miner_node']
    beneficiary = common.setup_beneficiary()

    node_root_dir = _setup_node_(node, beneficiary, test_settings['blocks_to_mine'])
    ext_api = common.external_api(node)
    int_api = common.internal_api(node)

    users = {k: {'encoded_pub_key': v['key_pair']['enc_pubk'],
                 'pub_key': v['key_pair']['pubk'],
                 'priv_key': v['key_pair']['privk']
                 } for (k, v) in _setup_users_(beneficiary,
                                               test_settings['users'],
                                               test_settings['min_spend_tx_fee'],
                                               ext_api, int_api).items()}
    test.globs['ae_node'] = {'_root_dir_': node_root_dir,
                             'external_api': ext_api,
                             'internal_api': int_api}
    test.globs['users'] = users
setup_test.__test__ = False

def teardown_test(test):
    test_settings = _test_settings_(test)
    node = test_settings['miner_node']
    node_glob = test.globs['ae_node']
    _cleanup_node_(node, node_glob['_root_dir_'])
teardown_test.__test__ = False

# Module private functions

def _test_settings_(test):
    _assert_doctest_(test)  # Hardcoded expectation on test framework.
    import common
    return common.test_settings(_test_name_from_test_path_(test.filename))

def _assert_doctest_(x):
    from nose.tools import assert_equal
    import doctest
    assert_equal(doctest.DocTest, x.__class__)

def _test_name_from_test_path_(test_path):
    import os
    (test_name, _) = os.path.splitext(os.path.basename(test_path))
    return test_name

def _setup_node_(node_makefile_id, beneficiary, blocks_to_mine):
    import common
    (node_root_dir,_,  _, _) = common.setup_node_with_tokens(node_makefile_id, beneficiary, blocks_to_mine)
    return node_root_dir

def _cleanup_node_(node_makefile_id, node_root_dir):
    import common
    import shutil
    common.stop_node(node_makefile_id)
    shutil.rmtree(node_root_dir)

def _setup_users_(beneficiary, user_settings, spend_tx_fee, ext_api, int_api):
    def key_pair():
        import keys
        priv = keys.new_private()
        pub = keys.public_key(priv)
        return {'privk': priv,
                'pubk': pub,
                'enc_pubk': keys.address(pub)}
    import common
    users = {k: {'bal': v['balance'],
                 'key_pair': key_pair()
                 } for (k, v) in user_settings.items()}
    for (_, u) in users.items():
        common.ensure_send_tokens(beneficiary,
                                  u['key_pair']['enc_pubk'],
                                  u['bal'],
                                  spend_tx_fee,
                                  ext_api, int_api,
                                  1) ## Confirmations.
    return users
