Overview
========

The following shows the intended usage of the Aeternity node API for Sophia contracts.

It uses a counter as sample contract.

>>> print(counter_contract)
<BLANKLINE>
contract Counter =
<BLANKLINE>
  record state = { value : int }
<BLANKLINE>
  entrypoint init(val) = { value = val }
  entrypoint get()     = state.value
  stateful function tick()    = put(state{ value = state.value + 1 })
<BLANKLINE>
<BLANKLINE>

It uses get function of the sample contract.

>>> counter_contract_function = "get"

Step 1: Bob creates the counter contract on the chain
=====================================================

Bob has the public-private key pair of his account.

>>> print(users['b']['encoded_pub_key']) # doctest: +ELLIPSIS
ak_...

Assumption: Bob has enough tokens.

Bob computes - off-chain, using the Aeternity node API - the bytecode of the contract.

>>> import common
>>> contract_bytecode = common.compile_contract(counter_contract_file)
>>> print(contract_bytecode) # doctest: +ELLIPSIS
cb_...

Bob computes - off-chain, using the Aeternity node API - the initialization call data.

>>> counter_init_value = 21
>>> encoded_init_call_data = common.encode_calldata(counter_contract_file, "init", str(counter_init_value))

Bob computes - off-chain, using the Aeternity node API - the unsigned contract create transaction.

>>> ContractCreateTx = ae_node['internal_api'].get_model('ContractCreateTx')
>>> contract_create_tx = ae_node['internal_api'].PostContractCreate(body=ContractCreateTx(
...   owner_id=users['b']['encoded_pub_key'],
...   nonce=1,
...   code=contract_bytecode,
...   vm_version=5,
...   abi_version=3,
...   deposit=0,
...   amount=0,
...   gas=20000,
...   gas_price=1000000000,
...   fee=200000000000000,
...   call_data=encoded_init_call_data))
>>> raw_unsigned_contract_create_tx = contract_create_tx.response().result
>>> contract_id = raw_unsigned_contract_create_tx.contract_id
>>> print(contract_id) # doctest: +ELLIPSIS
ct_...
>>> print(raw_unsigned_contract_create_tx.tx) # doctest: +ELLIPSIS
tx_...
>>> import common
>>> unsigned_contract_create_tx = common.api_decode(raw_unsigned_contract_create_tx.tx)

Bob signs - locally - the contract create transaction.

>>> import keys
>>> encoded_signed_contract_create_tx = keys.sign_verify_encode_tx(unsigned_contract_create_tx, users['b']['priv_key'], users['b']['pub_key'])
>>> print(encoded_signed_contract_create_tx) # doctest: +ELLIPSIS
tx_...

Bob publishes the signed contract create transaction to an Aeternity node for inclusion in the chain.

>>> Tx = ae_node['external_api'].get_model('Tx')
>>> tx_object = Tx(tx=encoded_signed_contract_create_tx)
>>> contract_create_tx_hash = ae_node['external_api'].PostTransaction(body=tx_object).response().result.tx_hash
>>> print(contract_create_tx_hash) # doctest: +ELLIPSIS
th_...

Bob ensures that the published contract create transaction is included in the chain.

>>> def is_tx_confirmed(ext_api, tx_hash, min_confirmations):
...  top_key_height = ext_api.GetCurrentKeyBlockHeight().response().result['height']
...  tx = ext_api.GetTransactionByHash(hash=tx_hash).response().result
...  if "none" == tx.block_hash:
...    return False
...  return (top_key_height - tx.block_height) >= min_confirmations
>>> from waiting import wait
>>> wait(lambda: is_tx_confirmed(ae_node['external_api'], contract_create_tx_hash, 2),
...      timeout_seconds=30)
True
>>> contract_init_call_object = ae_node['external_api'].GetTransactionInfoByHash(hash=contract_create_tx_hash).response().result
>>> print(contract_init_call_object.call_info.return_type)
ok

Step 2: Alice retrieves the value of the counter on the chain
=============================================================

Alice has the public-private key pair of her account.

>>> print(users['a']['encoded_pub_key']) # doctest: +ELLIPSIS
ak_...

Assumption: Alice has enough tokens.

Alice computes - off-chain, using the Aeternity node API - the unsigned contract call transaction.

>>> contract_call_data = common.encode_calldata(counter_contract_file, "get", "")
>>> ContractCallTx = ae_node['internal_api'].get_model('ContractCallTx')
>>> contract_call_tx = ae_node['internal_api'].PostContractCall(body=ContractCallTx(
...   caller_id=users['a']['encoded_pub_key'],
...   nonce=1,
...   contract_id=contract_id,
...   abi_version=3,
...   fee=500000000000000,
...   amount=0,
...   gas=20000,
...   gas_price=1000000000,
...   call_data=contract_call_data))

Alice signs - locally - the contract call transaction.

>>> unsigned_contract_call_tx = common.api_decode(contract_call_tx.response().result.tx)
>>> encoded_signed_contract_call_tx = keys.sign_verify_encode_tx(unsigned_contract_call_tx, users['a']['priv_key'], users['a']['pub_key'])
>>> print(encoded_signed_contract_call_tx) # doctest: +ELLIPSIS
tx_...

Alice publishes the signed contract call transaction to an Aeternity node for inclusion in the chain.

>>> contract_call_tx_hash = ae_node['external_api'].PostTransaction(body=Tx(tx=encoded_signed_contract_call_tx)).response().result.tx_hash
>>> print(contract_call_tx_hash) # doctest: +ELLIPSIS
th_...

Alice ensures that the published contract call transaction is included in the chain.

>>> def call_status(sync_call_api_with_http_info_fun):
...   from requests.exceptions import ConnectionError
...   try:
...     (_, status, _) = sync_call_api_with_http_info_fun()
...     return status
...   except ConnectionError as e:
...     return False
>>> wait(lambda: is_tx_confirmed(ae_node['external_api'], contract_call_tx_hash, 2),
...      timeout_seconds=30)
True
>>> contract_call_object = ae_node['external_api'].GetTransactionInfoByHash(hash=contract_call_tx_hash).response().result
>>> print(contract_call_object.call_info.return_type)
ok

Alice decodes the return value - off-chain, using the Aeternity node API.

>>> print(contract_call_object.call_info.return_value) # doctest: +ELLIPSIS
cb_...
>>> common.call_result(counter_contract_file, counter_contract_function, contract_call_object.call_info.return_value)
'21'
