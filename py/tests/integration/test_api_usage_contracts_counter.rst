Overview
========

The following shows the intended usage of the epoch API for Sophia contracts.

It uses a counter as sample contract.

>>> print(counter_contract)
<BLANKLINE>
contract Counter =
<BLANKLINE>
  record state = { value : int }
<BLANKLINE>
  function init(val) = { value = val }
  function get()     = state.value
  function tick()    = put(state{ value = state.value + 1 })
<BLANKLINE>
<BLANKLINE>

Please note that the return value of the "get" function is (inferred as) "int".

>>> counter_contract_get_function_return_value_type = "int"

Step 1: Bob creates the counter contract on the chain
=====================================================

Bob has the public-private key pair of his account.

>>> print(users['b']['encoded_pub_key']) # doctest: +ELLIPSIS
ak$...

Assumption: Bob has enough tokens.

Bob computes - off-chain, using the epoch API - the bytecode of the contract.

>>> from swagger_client.models.contract import Contract
>>> contract_bytecode = epoch_node['external_api'].compile_contract(Contract(counter_contract, options="")).bytecode
>>> print(contract_bytecode) # doctest: +ELLIPSIS
0x...

Bob computes - off-chain, using the epoch API - the initialization call data.

>>> counter_init_value = 21
>>> contract_init_call_data = {'f': "init", 'arg': "({})".format(counter_init_value)}
>>> from swagger_client.models.contract_call_input import ContractCallInput
>>> encoded_init_call_data = epoch_node['external_api'].encode_calldata(ContractCallInput("sophia", contract_bytecode, contract_init_call_data['f'], contract_init_call_data['arg'])).calldata

Bob computes - off-chain, using the epoch API - the unsigned contract create transaction.

>>> from swagger_client.models.contract_create_data import ContractCreateData
>>> raw_unsigned_contract_create_tx = epoch_node['external_api'].post_contract_create(ContractCreateData(
...   owner=users['b']['encoded_pub_key'],
...   nonce=1,
...   code=contract_bytecode,
...   vm_version=1,
...   deposit=0,
...   amount=0,
...   gas=20000,
...   gas_price=1,
...   fee=1,
...   call_data=encoded_init_call_data))
>>> contract_address = raw_unsigned_contract_create_tx.contract_address
>>> print(contract_address) # doctest: +ELLIPSIS
ct$...
>>> print(raw_unsigned_contract_create_tx.tx) # doctest: +ELLIPSIS
tx$...
>>> import base58
>>> unsigned_contract_create_tx = base58.b58decode_check(raw_unsigned_contract_create_tx.tx[3:])

Bob signs - locally - the contract create tx.

>>> import keys
>>> encoded_signed_contract_create_tx = keys.sign_verify_encode_tx(unsigned_contract_create_tx, users['b']['priv_key'], users['b']['pub_key'])
>>> print(encoded_signed_contract_create_tx) # doctest: +ELLIPSIS
tx$...

Bob publishes the signed contract create transaction to an epoch node for inclusion in the chain.

>>> from swagger_client.models.tx import Tx
>>> contract_create_tx_hash = epoch_node['external_api'].post_tx(Tx(encoded_signed_contract_create_tx)).tx_hash
>>> print(contract_create_tx_hash) # doctest: +ELLIPSIS
th$...

Bob ensures that the published contract create transaction is included in the chain.

>>> from waiting import wait
>>> wait(lambda: 200 == epoch_node['external_api'].get_contract_call_from_tx_with_http_info(contract_create_tx_hash,
...                                                                                         _return_http_data_only=False)[1],
...      timeout_seconds=30)
True
>>> contract_init_call_object = epoch_node['external_api'].get_contract_call_from_tx(contract_create_tx_hash)
>>> print(contract_init_call_object.return_type)
ok

Step 2: Alice retrieves the value of the counter on the chain
=============================================================

Alice has the public-private key pair of her account.

>>> print(users['a']['encoded_pub_key']) # doctest: +ELLIPSIS
ak$...

Assumption: Alice has enough tokens.

Alice computes - off-chain, using the epoch API - the unsigned contract call transaction.

>>> contract_call_data = {'f': "get", 'arg': "()"}
>>> from swagger_client.models.contract_call_compute import ContractCallCompute
>>> unsigned_contract_call_tx = base58.b58decode_check(epoch_node['external_api'].post_contract_call_compute(ContractCallCompute(
...   caller=users['a']['encoded_pub_key'],
...   nonce=1,
...   contract=contract_address,
...   vm_version=1,
...   fee=1,
...   amount=0,
...   gas=20000,
...   gas_price=1,
...   function=contract_call_data['f'],
...   arguments=contract_call_data['arg'])).tx[3:])

Alice signs - locally - the contract call tx.

>>> encoded_signed_contract_call_tx = keys.sign_verify_encode_tx(unsigned_contract_call_tx, users['a']['priv_key'], users['a']['pub_key'])
>>> print(encoded_signed_contract_call_tx) # doctest: +ELLIPSIS
tx$...

Alice publishes the signed contract create transaction to an epoch node for inclusion in the chain.

>>> contract_call_tx_hash = epoch_node['external_api'].post_tx(Tx(encoded_signed_contract_call_tx)).tx_hash
>>> print(contract_call_tx_hash) # doctest: +ELLIPSIS
th$...

Alice ensures that the published contract create transaction is included in the chain.

>>> def call_status(sync_call_api_with_http_info_fun):
...   from swagger_client.rest import ApiException
...   try:
...     (_, status, _) = sync_call_api_with_http_info_fun()
...     return status
...   except ApiException as e:
...     return e.status
>>> wait(lambda: 200 == call_status(lambda: epoch_node['external_api'].get_contract_call_from_tx_with_http_info(contract_call_tx_hash,
...                                                                                                             _return_http_data_only=False)),
...      timeout_seconds=30)
True
>>> contract_call_object = epoch_node['external_api'].get_contract_call_from_tx(contract_call_tx_hash)
>>> print(contract_call_object.return_type)
ok

Alice decodes the return value - off-chain, using the epoch API.

>>> print(contract_call_object.return_value) # doctest: +ELLIPSIS
0x...
>>> from swagger_client.models.sophia_binary_data import SophiaBinaryData
>>> epoch_node['external_api'].decode_data(SophiaBinaryData(sophia_type=counter_contract_get_function_return_value_type,
...                                                         data=contract_call_object.return_value)).data
{u'type': u'word', u'value': 21}
