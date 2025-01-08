# Introduction

This document describes the basics of he Aeternity Hyperchain, with links to more detailed documentation. It also covers a reference to the Hyperchain related Aeternity node configuration parameters and a quick-start guide on how to configure your Hyperchain node.

Hyperchains represents a groundbreaking evolution in blockchain architecture, combining the security benefits of established Proof-of-Work (PoW) chains with the efficiency and scalability of Proof-of-Stake (PoS) systems. By implementing a child chain that periodically synchronizes with a parent PoW chain, Hyperchains enables organizations to build robust, scalable blockchain networks while leveraging the established security of existing chains. This hybrid approach significantly reduces energy consumption compared to traditional PoW systems while maintaining strong security guarantees through its innovative parent-child chain relationship.

The 0.9 beta release introduces the core framework for Hyperchain deployment, providing developers with the foundation to test and validate the Hyperchains architecture. This initial release focuses on establishing and maintaining the fundamental relationship between an Aeternity parent chain and its child chain, implementing the basic mechanisms for chain synchronization and validator operations. This documentation serves as a technical guide for configuring and deploying a Hyperchain, aimed at developers who are already running Aeternity nodes and want to experiment with Hyperchain functionality. It covers the essential configuration parameters and setup requirements needed to participate in this beta testing phase.

## 1. Prerequisites Checklist for Running a Hyperchain
### Required Infrastructure
- [ ] Running Aeternity node (v6.7.0 or later)
- [ ] Minimum 4GB RAM dedicated to Hyperchain operations
- [ ] 100GB available storage space
- [ ] Stable internet connection with minimum 10Mbps upload/download
### Required Technical Knowledge
- [ ] Experience running and maintaining Aeternity nodes
- [ ] Basic understanding of blockchain concepts
- [ ] Familiarity with command-line operations
- [ ] Understanding of YAML configuration files
### Required Accounts and Keys
- [ ] Access to parent chain (Aeternity) wallet with sufficient funds for pinning
- [ ] Private key for hyperchain staking operations
- [ ] Private key for parent chain pinning operations
## 2. What to Expect When Setting Up a Hyperchain
### Time Investment
- Initial setup: 2-3 hours
- Configuration testing: 1-2 hours
- Initial sync time: 2-4 hours (depends on network conditions)
### Resource Requirements
- Parent chain wallet must maintain minimum balance of X AE for pinning operations
- Continuous monitoring during first 24 hours of operation
- Regular maintenance windows for updates and optimizations
### Technical Process Overview
1. Configuration of parent chain connection
2. Hyperchain node setup and configuration
3. Validation period (24-48 hours)
4. Network participation initiation (edited)


## Complete Hyperchain `aeternity.yaml` configuration example

An Aeternity Hyperchain node is a standard Aeternity release configured to run a Hyperchain-configured consensus
algorithm. Please follow the instructions elswhere in the relase to install and do basic configuration. Then adopt and add the additional configuration below to your `aternity.yaml` or (`.json` if that is your fancy).

The recommended setup is to run your parent node locally, in the example below we have a `mainnet` node running on port `6013` (as defined in `parent_chain.polling.nodes`). Again, refer to the core documentation above how to install and configure a regular Aeternity node.

**What are reasonable values here????** The initial "speed" of the hyperchain is defined by the epoch and block production parameters. These will be different for each hyperchain, depending on the desired transactional characteristics of the chain. The values below are useful for testing purposes, and when running on Aeternity `mainnet`. Please see the main Hyperchain documentation for more information in how to optimize block and epoxh parameters per application and parent chain
characteristics.

**Contract section goes here. How are the default contracts started?**

We define two `stakers`, that need to have accounts and funds on the Hyperchain. Both these stakers also `mainnet` accounts used for pinning. Their credentials for `mainnet` is defined and mapped to their Hyperchain accounts in `pinners`. When `default_pinning_behavior` parameter is set to `true` each of theses validators will automatically
pin the Hyperc hain state to the parent chain when they are leaders of the last block of the epoch using their mapped parent account credentials. The `pinning_reward_value` (in whatever the native currency is for the Hyperchain) is the reward that will be handed out for doing a verifiable pinning for an epoch. This should be adjusted to reflect the economics of the Hyperchain.

```yaml
chain:
  consensus:
    '0':
      config:
        child_block_production_time: 250
        child_block_time: 5000
        child_epoch_length: 50
        parent_epoch_length: 3
        contract_owner: ak_11111111111111111111111111111115rHyByZ
        election_contract: ct_LRbi65kmLtE7YMkG6mvG5TxAXTsPJDZjAtsPuaXtRyPA7gnfJ
        default_pinning_behavior: true
        expected_key_block_rate: 2000 #??
        fixed_coinbase: 100000000000000000000 #??
        genesis_start_time: 1734687700341 #??
        stakers:
          - hyper_chain_account:
              priv: 91450EFE0516C24476390086426008147CFDEECFE6935FADA1C056C3A5BA73FBB1B577BCD327CB39E55E6C026BD6A74A1B35DE6C0650C4AE51EFAB759E415B66
              pub: ak_2MGLPW2CHTDXJhqFJezqSwYSNwbZokSKkG7wSbGtVmeyjGfHtm
          - hyper_chain_account:
              priv: ED0C148073A6206ADC8E6F618D68C9823864408E8BA357A6B95E049FD9F3A0A9C8AB5D0B035DB141C51B7B7FB1A5BED314704F6C554E58B51ACFBFD328E18A9A
              pub: ak_2XNq9oKtThxKLNFGWTaxmLBZPgP7ECEGxL3zK7dTSFh6RyRvaG
        parent_chain:
          consensus:
            network_id: mainnet
            type: AE2AE
          finality: 2
          polling:
            cache_size: 10
            fetch_interval: 100
            nodes:
              - http://127.0.0.1:6013
          start_height: 1060000
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
        staking_contract: ct_KJgjAXMtRF68AbT5A2aC9fTk8PA4WFv26cFSY27fXs6FtYQHK
      type: hyperchain
  http:
  endpoints:
    hyperchain: true
  # Do we need these below or just use default values?
  external:
    port: 3013
  internal:
    port: 3113
  rosetta:
    port: 3213
  rosetta_offline:
    port: 3413
```

# Addendum 1: Hyperchain-specific confguration reference
## chain\.consensus: object

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
chain:
  consensus:
    '0':
      type: hyperchain
      config: ...
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
|**child\_block\_time**|`integer`|The average time in milliseconds between two key blocks on the hyperchain<br/>Default: `3000`<br/>||
|**child\_block\_production\_time**|`integer`|The time in milliseconds to produce a hyperchain block<br/>||
|**child\_epoch\_length**|`integer`|The number of blocks in an epoch on the hyperchain<br/>||
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
|[**hyper\_chain\_account**](#chainconsensus1-90-9configstakershyper_chain_account)|`object`|Hyperchain staking account<br/>||

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

Hyperchain parent accounts.

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

Parent chain account for executing pinning (spend) transactions


**Properties**

|Name|Type|Description|Required|
|----|----|-----------|--------|
|**pub**|`string`|Public key<br/>Default: `""`<br/>||
|**priv**|`string`|Private key<br/>Default: `""`<br/>||
|**owner**|`string`|Public key of Hyperchain account owner. Needs to correspond to an account defined in [**stakers**](#chainconsensus1-90-9configstakers)<br/>Default: `""`<br/>||

**Additional Properties:** not allowed
**Example**

```yaml
pub: ""
priv: ""
owner: ""
```


