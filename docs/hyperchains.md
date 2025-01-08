# Introduction

This document describes the basics of he Aeternity Hyperchain, with links to more detailed documentation. It also covers a reference to the Hyperchain related Aeternity node configuration parameters and a quick-start guide on how to configure your Hyperchain node.

# chain\.consensus: object

The consensus algorithms used for validating blocks. Ignored if 'fork_management > network_id' has value 'ae_mainnet' or 'ae_uat'.


**Properties (Pattern)**

|Name|Type|Description|Required|
|----|----|-----------|--------|
|[\<height\>](#chainconsensus1-90-9)|`object`|Property name indicates minimum height at which the given consensus algorithm gets activated. By default from genesis Cuckoo cycle based BitcoinNG is used.<br/>|yes|

<a name="chainconsensus1-90-9"></a>
## chain\.consensus\.\<height\>: object

Configuration parameters for the selected consensus algorithm (Hyperchain).


**Properties**

|Name|Type|Description|Required|
|----|----|-----------|--------|
|**type**|`string`|The type of the consensus algorithm used at the given height (ex. pow_cuckoo, smart_contract or hyperchain)<br/>Default: `"hyperchain"`<br/>|yes|
|[**config**](#chainconsensus1-90-9config)|`object`|Configuration for the given consensus algorithm<br/>|no|

**Example**

```yaml
type: hyperchain
config:

```

<a name="chainconsensus1-90-9config"></a>
### chain\.consensus\.\<height\>.config: object

Configuration for the given consensus algorithm


**Properties**

|Name|Type|Description|Required|
|----|----|-----------|--------|
|**contract\_owner**|`string`|Owner of the smart contracts that controls the consensus.<br/>||
|**election\_contract**|`string`|The address of the smart contract that will be used for leader elections. For a new chain this contract should be loaded at genesis<br/>||
|**rewards\_contract**|`string`|The address of the smart contract that will be used for reward distributions. For a new chain this contract should be loaded at genesis<br/>||
|**genesis\_start\_time**|`integer`|Timestamp for genesis<br/>Default: `0`<br/>||
|**child\_block\_time**|`integer`|The average time in milliseconds between two key blocks on the child chain<br/>Default: `3000`<br/>||
|**child\_block\_production\_time**|`integer`|The time in milliseconds to produce a child block<br/>||
|**child\_epoch\_length**|`integer`|The number of blocks in an epoch on the child chain<br/>||
|**pinning\_reward\_value**|`integer`|The initial value of the Pinning reward. It can later be changed through consensus<br/>Default: `0`<br/>||
|**default\_pinning\_behavior**|`boolean`|Use the default pinning behavior, where in each epoch, if the last leader has defined pinning/parent chain credentials, they will pin<br/>Default: `false`<br/>||
|**fixed\_coinbase**|`integer`|The coinbase reward specifies the fixed amount of newly minted tokens allocated to block producers as an incentive for validating and adding blocks to the chain.<br/>Default: `0`<br/>||
|[**parent\_chain**](#chainconsensus1-90-9configparent_chain)|`object`|Details of how this node will connect to a parent chain if this is a hyperchain.<br/>||
|[**stakers**](#chainconsensus1-90-9configstakers)|`object[]`|List of hyperchain accounts<br/>||
|[**pinners**](#chainconsensus1-90-9configpinners)|`object[]`|List of parent chain accounts<br/>||

**Example**

```yaml
config:
  genesis_start_time: 0
  child_block_time: 3000
  pinning_reward_value: 0
  default_pinning_behavior: false
  fixed_coinbase: 0
  parent_chain:
    start_height: 0
    acceptable_sync_offset: 60000
    consensus:
      network_id: ae_mainnet
      type: AE2AE
    polling:
      fetch_interval: 500
      retry_interval: 1000
      cache_size: 200
      nodes: []
  stakers:
    - []
  pinners:
    - []
```

<a name="chainconsensus1-90-9configparent_chain"></a>
### chain\.consensus\.\<height\>\.config\.parent\_chain: object

Details of how this node will connect to a parent chain if this is a hyperchain.


**Properties**

|Name|Type|Description|Required|
|----|----|-----------|--------|
|**start\_height**|`integer`|Height on the parent chain that this hyperchain will start posting commitments and start creating blocks<br/>Default: `0`<br/>||
|**finality**|`integer`|The number of blocks on the parent chain to reach finality.<br/>||
|**parent\_epoch\_length**|`integer`|The number of blocks in an epoch on the parent chain.<br/>||
|**acceptable\_sync\_offset**|`integer`|The maximum amount of time the parent chain block generation can be off the expected time.<br/>Default: `60000`<br/>||
|[**consensus**](#chainconsensus1-90-9configparent_chainconsensus)|`object`|Details of the parent chain.<br/>||
|[**polling**](#chainconsensus1-90-9configparent_chainpolling)|`object`|Parent chain connection configuration<br/>||

**Additional Properties:** not allowed
**Example**

```yaml
{
    "start_height": 0,
    "acceptable_sync_offset": 60000,
    "consensus": {
        "network_id": "ae_mainnet",
        "type": "AE2AE"
    },
    "polling": {
        "fetch_interval": 500,
        "retry_interval": 1000,
        "cache_size": 200,
        "nodes": []
    }
}
```

<a name="chainconsensus1-90-9configparent_chainconsensus"></a>
### chain\.consensus\.\<height\>\.config\.parent\_chain\.consensus: object

Details of the parent chain.


**Properties**

|Name|Type|Description|Required|
|----|----|-----------|--------|
|**network\_id**|`string`|The network Id of the parent chain if it has one<br/>Default: `"ae_mainnet"`<br/>||
|**type**|`string`|The type of parent network connection. Currently only AE, Bitcoin and Dogecoin are implemented<br/>Default: `"AE2AE"`<br/>Enum: `"AE2AE"`, `"AE2BTC"`, `"AE2DOGE"`<br/>||

**Additional Properties:** not allowed
**Example**

```yaml
network_id: ae_mainnet
type: AE2AE
```

<a name="chainconsensus1-90-9configparent_chainpolling"></a>
### chain\.consensus\.\<height\>\.config\.parent\_chain\.polling: object

Parent chain connection configuration


**Properties**

|Name|Type|Description|Required|
|----|----|-----------|--------|
|**fetch\_interval**|`integer`|The interval between polls of the parent chain looking for a new block (milliseconds)<br/>Default: `500`<br/>||
|**retry\_interval**|`integer`|The amount of time in milliseconds to wait before retrying a check for a block on the parent chain.<br/>Default: `1000`<br/>||
|**cache\_size**|`integer`|Size of local cache for parent chain<br/>Default: `200`<br/>||
|[**nodes**](#chainconsensus1-90-9configparent_chainpollingnodes)|`string[]`|List of parent chain nodes to poll for new blocks<br/>Default: <br/>||

**Additional Properties:** not allowed
**Example**

```yaml
fetch_interval: 500
retry_interval: 1000
cache_size: 200
nodes: []
```

<a name="chainconsensus1-90-9configparent_chainpollingnodes"></a>
### chain\.consensus\.\<height\>\.config\.parent\_chain\.polling\.nodes\[\]: array

List of parent chain nodes to poll for new blocks


**Items**


URL address of the API - <protocol>://<user>:<password>@<hostname>:<port>

**Item Type:** `string`
<a name="chainconsensus1-90-9configstakers"></a>
### chain\.consensus\.\<height\>\.config\.stakers\[\]: array

List of hyperchain accounts


**Items**


Hyperchain account pair

**Item Properties**

|Name|Type|Description|Required|
|----|----|-----------|--------|
|[**hyper\_chain\_account**](#chainconsensus1-90-9configstakershyper_chain_account)|`object`|Child chain staking account<br/>||

**Item Additional Properties:** not allowed
**Example**

```yaml
- []
```

<a name="chainconsensus1-90-9configstakershyper_chain_account"></a>
### chain\.consensus\.\<height\>\.config\.stakers\[\]\.hyper\_chain\_account: object

Hyper chain validator/staking account(s)

**Properties**

|Name|Type|Description|Required|
|----|----|-----------|--------|
|**pub**|`string`|Public key<br/>Default: `""`<br/>||
|**priv**|`string`|Private key<br/>Default: `""`<br/>||

**Additional Properties:** not allowed
**Example**

```yaml
pub: ""
priv: ""
```

<a name="chainconsensus1-90-9configpinners"></a>
### chain\.consensus\.\<height\>\.config\.pinners\[\]: array

List of parent chain accounts. Relevant only to Aeternity parent chains. For BTC and Doge, each node relies on a
local wallet setup with accounts and funds.


**Items**


Hyperchain account pair

**Item Properties**

|Name|Type|Description|Required|
|----|----|-----------|--------|
|[**parent\_chain\_account**](#chainconsensus1-90-9configpinnersparent_chain_account)|`object`|Parent chain pinning account<br/>||

**Item Additional Properties:** not allowed
**Example**

```yaml
- []
```

<a name="chainconsensus1-90-9configpinnersparent_chain_account"></a>
### chain\.consensus\.\<height\>\.config\.pinners\[\]\.parent\_chain\_account: object

Parent chain pinning account


**Properties**

|Name|Type|Description|Required|
|----|----|-----------|--------|
|**pub**|`string`|Public key<br/>Default: `""`<br/>||
|**priv**|`string`|Private key<br/>Default: `""`<br/>||
|**owner**|`string`|Public key of Hyperchain account owner<br/>Default: `""`<br/>||

**Additional Properties:** not allowed
**Example**

```yaml
pub: ""
priv: ""
owner: ""
```


## Complete `aeternity.yaml` example

```yaml
chain:
  consensus:
    '0':
      config:
        child_block_production_time: 50
        child_block_time: 200
        child_epoch_length: 10
        contract_owner: ak_11111111111111111111111111111115rHyByZ
        default_pinning_behavior: false
        election_contract: ct_LRbi65kmLtE7YMkG6mvG5TxAXTsPJDZjAtsPuaXtRyPA7gnfJ
        expected_key_block_rate: 2000
        fixed_coinbase: 100000000000000000000
        genesis_start_time: 1734687700341
        parent_chain:
          consensus:
            amount: 9700
            fee: 100000
            network_id: testnet
            spend_address: ak_2CPHnpGxYw3T7XdUybxKDFGwtFQY7E5o3wJzbexkzSQ2BQ7caJ
            type: AE2DOGE
          finality: 2
          parent_epoch_length: 3
          polling:
            cache_size: 10
            fetch_interval: 100
            nodes:
              - http://test:Pass@127.0.0.1:44555
          start_height: 133
        pinners:
          - parent_chain_account:
              owner: ak_2MGLPW2CHTDXJhqFJezqSwYSNwbZokSKkG7wSbGtVmeyjGfHtm
              priv: 6BFBBDB05CDD042E38E78975B5087C0ED496A7355F5E325690E65DDE3D74556008899F638BAF1B3A4D0BBF34C6C7073285C3B8DB947C04052CF7395FBCAD5F23
              pub: ak_4m5iGyT3AiahzGKCE2fCHVsQYU7FBMDiaMJ1YPxradKsyfCc9
          - parent_chain_account:
              owner: ak_2XNq9oKtThxKLNFGWTaxmLBZPgP7ECEGxL3zK7dTSFh6RyRvaG
              priv: 51B10F6C10B780E50472A6E32F7D911544C4B9732AC6A8CCDCCEC83A0C203862D4D4A94E95948ADD9C50049C098B9072F37A1467A82B2AF45D7626624722C75E
              pub: ak_2cjUYDhaKaiyGvuswL6K96ooKZKtFZZEopgxc3hwR2Yqb8SWxd
        pinning_reward_value: 4711
        rewards_contract: ct_KJgjAXMtRF68AbT5A2aC9fTk8PA4WFv26cFSY27fXs6FtYQHK
        stakers:
          - hyper_chain_account:
              priv: 91450EFE0516C24476390086426008147CFDEECFE6935FADA1C056C3A5BA73FBB1B577BCD327CB39E55E6C026BD6A74A1B35DE6C0650C4AE51EFAB759E415B66
              pub: ak_2MGLPW2CHTDXJhqFJezqSwYSNwbZokSKkG7wSbGtVmeyjGfHtm
          - hyper_chain_account:
              priv: ED0C148073A6206ADC8E6F618D68C9823864408E8BA357A6B95E049FD9F3A0A9C8AB5D0B035DB141C51B7B7FB1A5BED314704F6C554E58B51ACFBFD328E18A9A
              pub: ak_2XNq9oKtThxKLNFGWTaxmLBZPgP7ECEGxL3zK7dTSFh6RyRvaG
        staking_contract: ct_KJgjAXMtRF68AbT5A2aC9fTk8PA4WFv26cFSY27fXs6FtYQHK
      type: hyperchain
````
<!--
  db_backend: mnesia
  hard_forks:
    '7':
      accounts_file: /Users/aeternity/ae/_build/test/logs/latest.hyperchains/dev1/data/aecore/.7/hc_accounts.json
      contracts_file: /Users/aeternity/ae/_build/test/logs/latest.hyperchains/dev1/data/aecore/.7/hc_contracts.json
      height: 0
  persist: false
fork_management:
  network_id: this_will_be_overwritten_runtime
http:
  endpoints:
    hyperchain: true
  external:
    port: 3013
  internal:
    port: 3113
  rosetta:
    port: 3213
  rosetta_offline:
    port: 3413
include_default_peers: false
keys:
  dir: /Users/aeternity/ae/_build/test/logs/latest.hyperchains/dev1/data/keys
  peer_password: '1734.687700.415689'
logging:
  hwm: 5000
  level: debug
mining:
  autostart: false
  beneficiary: ak_2evAxTKozswMyw9kXkvjJt3MbomCR1nLrf91BduXKdJLrvaaZt
  beneficiary_reward_delay: 2
  micro_block_cycle: 1
peers:
  - aenode://pp_23YdvfRPQ1b1AMWmkKZUGk2cQLqygQp55FzDWZSEUicPjhxtp5@localhost:3025
  - aenode://pp_2M9oPohzsWgJrBBCFeYi3PVT4YF7F2botBtq6J1EGcVkiutx3R@localhost:3035
sync:
  ping_interval: 5000
  port: 3015
websocket:
  channel:
    port: 3014
-->