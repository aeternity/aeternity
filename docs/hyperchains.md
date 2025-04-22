# Introduction

This document describes the Aeternity Hyperchain architecture and provides configuration guidance for deploying a Hyperchain node. It includes configuration parameters, setup requirements, and step-by-step deployment instructions.

Hyperchains represent an evolution in blockchain architecture that combines Proof-of-Work (PoW) security with Proof-of-Stake (PoS) efficiency. Through a child chain that periodically synchronizes with a parent PoW chain, Hyperchains enable scalable blockchain networks while maintaining strong security guarantees. This hybrid approach significantly reduces energy consumption compared to traditional PoW systems.

The 0.9 beta release provides developers who are already running Aeternity nodes with the foundation to test and validate Hyperchain functionality. This release focuses on establishing the parent-child chain relationship, implementing chain synchronization, and managing validator operations. This documentation will guide you through the essential configuration parameters and setup requirements needed to participate in the beta testing phase.

# Before You Begin

This guide assumes you have:
- A running Aeternity node (v6.7.0 or later)
- Experience managing blockchain nodes
- Access to sufficient funds for pinning operations
- Understanding of node validator operations

Important:
- Back up your existing node configuration before making any changes
- Store all private keys securely
- Verify all prerequisites before proceeding

## Prerequisites Checklist

### Required Infrastructure
- Running Aeternity node (v6.7.0 or later)
- Minimum 4GB RAM dedicated to Hyperchain operations
- 100GB available storage space
- Stable internet connection with minimum 10Mbps upload/download

### Required Technical Knowledge
- Experience running and maintaining Aeternity nodes
- Basic understanding of blockchain concepts
- Familiarity with command-line operations
- Understanding of YAML configuration files

### Required Accounts and Keys
- Access to parent chain (Aeternity) wallet with sufficient funds for pinning
- Private key for hyperchain staking operations
- Private key for parent chain pinning operations

## Setup Process Overview

### Time Investment
- Initial setup: 2-3 hours
- Configuration testing: 1-2 hours
- Initial sync time: 2-4 hours (depends on network conditions)

### Resource Requirements
- Parent chain wallet must maintain minimum balance of X AE for pinning operations
- Continuous monitoring during first 24 hours of operation
- Regular maintenance windows for updates and optimizations

### Implementation Steps
1. Configure parent chain connection
2. Set up and configure Hyperchain node
3. Complete validation period (24-48 hours)
4. Begin network participation

<!--
The quickest way to explore Hyperchains is through our preconfigured Localnet environment. Localnet provides a complete Docker compose configuration that includes a pre-configured Aeternity parent chain, Hyperchain node, and all essential development tools for both networks. This environment serves both as a testing playground and a development platform, and is maintained with the latest tools and node versions. You can find information on how to run the Localnet here:
 [Localnet](https://github.com/aeternity/localnet/blob/master/README.md#hyperchains-configuration). -->

## Configuring Your Own Hyperchain Node

Setting up a Hyperchain requires careful configuration and several specific steps. To simplify this process, Aeternity provides the hyperchain-starter-kit tool, which automates many of the complex configuration tasks. See [`hyperchain-starter-kit`](https://github.com/aeternity/hyperchain-starter-kit) (from Aeternity
GitHub org)

**Please note that this tool is currently in development. While functional, you may encounter some warnings or minor issues as we continue to improve its stability and user experience**

### Requirements:

- A running Aeternity node installation (following the installation instructions)
- [Git](https://git-scm.com/downloads) version control system installed to download the starter kit code
- [Node.js](https://nodejs.org/en/download) installed to run the `hyperchain-starter-kit`
- [Jq](https://jqlang.github.io/jq/) installed for JSON formatting

Note: These instructions use Unix/Linux/MacOS shell commands. Windows users will need to adjust paths and commands accordingly.

### 1. Install the Hyperchains Starter Kit

```shell
git clone https://github.com/aeternity/hyperchain-starter-kit
cd hyperchain-starter-kit
npm install
npm run dev
```

### 2. Tool configuration

Our Hyperchain (configuration) will be named `hc_test` for this example.
A directory with the same name will be created in the root directory of the tool where generated files will end up.

```shell
npm run dev init hc_test
```

This command creates an `init.yaml` file in the root of your `hc_test` directory. It contains parameters
and settings for your hyperchain. It looks like:
```yaml
childBlockTime: 3000
childEpochLength: 600
contractSourcesPrefix: 'https://raw.githubusercontent.com/aeternity/aeternity/refs/tags/v7.3.0-rc3/'
enablePinning: true
faucetInitBalance: 1000000000000000000000000000
fixedCoinbase: 100000000000000000000
networkId: 'hc_test'
parentChain:
  epochLength: 10
  networkId: 'ae_uat'
  nodeURL: 'https://testnet.aeternity.io'
  type: 'AE2AE'
pinningReward: 1000000000000000000000
treasuryInitBalance: 1000000000000000000000000000000000000000000000000
validators:
  balance: 3100000000000000000000000000
  count: 3
  validatorMinStake: 1000000000000000000000000
```
This is the default setup that uses Aeternity `testnet` as parent chain and appropriate block time and epoch length for it.
The configuration can be changed as needs following the rules in the [Configuration Details](#configuration-explained).


### 3. Set Up Contracts

To prepare the Hyperchain contracts run the following:
```shell
npm run dev retrieve-contracts hc_test
```

This will download the standard contracts from the latest Aeternity release and compile them in `contracts` sub-directory.

### 4. Generate the Economy

To generate your Hyperchain "economy" according to `init.yaml` run the following:
```shell
npm run dev gen-economy hc_test
```

This command will generate random keypairs for all accounts that are needed:

- Faucet: can be used to fund other accounts in an automated way
- Treasury: the same purpose as Faucet but manual funding
- Validators: pre-funded (Stakers) accounts used to initialized the Hyperchain
- Pinners: a list of accounts used for pinning

**Please note that you have to somehow fund all pinners accounts on the parent chain prior starting your node/validator.**

You can inspect the outcome of this in the `hc_test/economy-unencrypted.yaml` file:

```bash
cat hc_test/economy-unencrypted.yaml
```

Example output:
```yml
faucet:
  account:
    addr: 'ak_WNkJkmaEoyScVRaeZeDvvmCqLn1HkfNnQDy1oSJp6Jm5YPpAq'
    index: 0
    mnemonic: 'alpha camp aunt tattoo gravity beach report run cube rude morning ahead elephant super team vague nut property surge erode total rhythm output perfect'
    privKey: 'd747348b11eff32d764a31e2a528fa5624fa6f18a4a6f99070d55ea77905ed2342b30be3a20307cb499230fa857611e5101b0682e57d23d21ca1f360705404d7'
  initialBalance: 1000000000000000000000000000
pinners:
  - account:
      addr: 'ak_2cYQac22msS2BGxR44BiD3CSLDvG9Xuamn61y2gdb8qJXwd7or'
      index: 0
      mnemonic: 'grant comic beauty impose dance rate crash scorpion cream domain level have vessel tortoise fringe north profit loop enemy traffic outer loud version return'
      privKey: '9e2c28f2037e6d352a88137572255ed8af2e86dfb2c2949c4d805ff08bff2784d465bc1253fd1594ecc68e128bb2e241fb2fc0bd48a72e7d57af6dadad67b5dc'
    owner: 'ak_o9UXHESE3mDd9qeDKnxkRrfUS3eY17xkhshZX3VFqAbkVMUDV'
  - account:
      addr: 'ak_kU8HHb7raYHf5USrEv1zQ1WfFU8Fccrzp2nPM7gtoAvi3cFZh'
      index: 0
      mnemonic: 'mesh cousin december tank rival please march museum dragon peanut border tourist execute cash slush also two other casual vague curve verify trophy ceiling'
      privKey: 'c447e8e3159a57ebffb19f3bd7c63afd739e5c592d9bd20620dff61c58d842b062b2dd406ded9a12fb6e455ce2fe3eb1af96182f5a59697e04478e372c58c927'
    owner: 'ak_2uc64DEjX1XCe2dYtTaDBfGXqPDiv1Xcu29T4P2x5NzXnRerAA'
  - account:
      addr: 'ak_2LS9FZLu7v5mVHW8sHa8DxvVWzfKf39SHrHNmmvQseTWCbff5J'
      index: 0
      mnemonic: 'sweet green lizard auto science wreck destroy indicate roast garment cloth album fringe valley remain bike black purse antique annual until umbrella wealth gadget'
      privKey: 'a19771d213eadede7c3332244ddeaa5e2caaa8be4aae79eee8d425e47a237dffafd27c0d8b8485c6dab5ec083448f81e68de9202258ee9f5ec313874bb0e2537'
    owner: 'ak_BXprz9o24a8RYbvBRDsUCBdGpz2uULBnDw6BiYpG957BiEvVn'
treasury:
  account:
    addr: 'ak_CLTKb9tdvXGwpgUBW8GytW7kXv9r1eJyY4YtgKKWtKcY3poQf'
    index: 0
    mnemonic: 'horror hunt card coconut wait snack clerk prepare process oval radar praise candy sting fury target fan nothing option pattern garden off when deer'
    privKey: '9ab3bb473a0ec30d4f75f877ee9de0145fbd883e07edab2e4555e156a729be5619bd0a236a9ebd4a47e5fc3961b59ea215515109c007e2ce844b1f543d4e9598'
  initialBalance: 1000000000000000000000000000000000000000000000000
validators:
  - account:
      addr: 'ak_o9UXHESE3mDd9qeDKnxkRrfUS3eY17xkhshZX3VFqAbkVMUDV'
      index: 0
      mnemonic: 'seminar else board area mercy dune bottom wide move emotion fold goat recipe horror liquid spoon visa click hen benefit link lawn castle hospital'
      privKey: 'fca30982735665d39d52146c30837cc33cf1c11015accb5ce520065223872e3968c7c7dc1372514ada09eac4a433ee479bf980c6d9d8bbdc88284cb19e8b4cb6'
    initialBalance: 3100000000000000000000000000
  - account:
      addr: 'ak_2uc64DEjX1XCe2dYtTaDBfGXqPDiv1Xcu29T4P2x5NzXnRerAA'
      index: 0
      mnemonic: 'arrest friend excuse expect learn material differ fat fiscal subway ghost quick science balcony thing receive wage hold visa boy still close pull mutual'
      privKey: '43a889d992a923e195cecf0f4346e4f480cec0ef2f561e383cf6b367e4da8b3dfb245ed4f23bcc25205d1cba30c85ed3a9a3a10858e12a6944d81944d3277850'
    initialBalance: 3100000000000000000000000000
  - account:
      addr: 'ak_BXprz9o24a8RYbvBRDsUCBdGpz2uULBnDw6BiYpG957BiEvVn'
      index: 0
      mnemonic: 'orchard issue drip level knife mechanic dirt keep gas potato close path recall rice strong seminar tattoo alien wealth note soft cash include business'
      privKey: '30267f551a56954f654edaa3cb21a58a21898af0519503d3737f8cd5c8b913eb17e9b9a1f3f725b0de559f30276ff065a84cca01b5720df302e2044032acd6ba'
    initialBalance: 3100000000000000000000000000
```

### 5. Generate Configuration Files

To generate all the configuration files needed to run a Hyperchain, run:
```shell
npm run dev gen-node-conf hc_test
```

This will create 3 files in `nodeConfig` directory:

- `aeternity.yaml`
- `hc_test_accounts.json`
- `hc_test_contracts.json`

**Don't forget to fund all pinners accounts on the parent chain prior starting your node/validator.**

**IMPORTANT:**
If you used a known public chain (testnet or mainnet) as parent chain, the tool will set the `start_height` as current block + 10, that is 30 minutes in future.
Keep that in mind when verifying your chain, either decrease the number or wait until that block is produced on the parent chain before you start transacting on the Hyperchain.

### 6. Run a Node

#### Docker

To allow inter-container connections between nodes, i.e. adding more nodes to the setup later, a Docker network should be created first:

```shell
docker network create hyperchain
```

A container name `initiator` is also used to allow docker network access to the container later.

Use [Docker volumes](https://docs.docker.com/engine/storage/volumes/) to install the configuration files and run the node at once:

```shell
docker run --rm -it -p 3013:3013 \
    --network=hyperchain --name initiator \
    -v ${PWD}/hc_test/nodeConfig/aeternity.yaml:/home/aeternity/.aeternity/aeternity/aeternity.yaml \
    -v ${PWD}/hc_test/nodeConfig/hc_test_accounts.json:/home/aeternity/node/data/aecore/hc_test_accounts.json \
    -v ${PWD}/hc_test/nodeConfig/hc_test_contracts.json:/home/aeternity/node/data/aecore/hc_test_contracts.json \
    aeternity/aeternity:v7.3.0-rc3
```

Verify your node is running with:
```shell
curl -s localhost:3013/v3/status | jq
```

Expected output:
```shell
{
  "difficulty": 0,
  "genesis_key_block_hash": "kh_7dm2zSo6NsnEDMYBYdXA9QvkJvvk7TenT68HxW5BSrRSz3WV6",
  "hashrate": 0,
  "listening": true,
  "network_id": "hc_test",
  "node_revision": "57bc00b760dbb3ccd10be51f447e33cb3a2f56e3",
  "node_version": "7.3.0-rc3",
  "peer_connections": {
    "inbound": 0,
    "outbound": 0
  },
  "peer_count": 0,
  "peer_pubkey": "pp_2ZX5Pae6a9L5UFm8VcCNsB39pn3EK7ZQZpp3dfF1WDNFXZ9p3b",
  "pending_transactions_count": 0,
  "protocols": [
    {
      "effective_at_height": 0,
      "version": 6
    }
  ],
  "solutions": 0,
  "sync_progress": 100,
  "syncing": false,
  "top_block_height": 2,
  "top_key_block_hash": "kh_2QCveiMjWXbuDzXkJmpposgmTrdSmPJnWBFpq33Xc6bZZnBXP4",
  "uptime": "6s.18"
}
```

For more details refer to dedicated [Docker section](docker.md).

#### Tarball Installation

Copy all of the above files to their node corresponding directory, i.e. assuming it's in `~/aeternity/node`:

```shell
cp ./hc_test/nodeConfig/aeternity.yaml ~/aeternity/node/
cp ./hc_test/nodeConfig/hc_test_*.json ~/aeternity/node/data/aecore/
```

then run your node:
```shell
~/aeternity/node/bin/aeternity start
```

Verify your node is running with:
```shell
~/aeternity/node/bin/aeternity status
```

Expected output:
```shell
Genesis block Hash          kh_7dm2zSo6NsnEDMYBYdXA9QvkJvvk7TenT68HxW5BSrRSz3WV6
Difficulty                  0
Syncing                     false
Sync progress               100.0
Node version                7.3.0-rc2
Node revision               336f76030d86f3accce7e8611f36f7e7641e2695
Peer count                  0
Peer connections (inbound)  0
Peer connections (outbound) 0
Pending transactions count  0
Network id                  hc_test
Peer pubkey                 pp_rx9MgTnZdB5Vhmpe9iCsms7bESn2ACfJtNoVXGUWKdPhQXqco
Top key block hash          kh_2DbFXJ5Uub5ogEXv7wVEhoiMMe5LDQTbYoU4x5D1jQp1XuzgE2
Top block height            404
Top block protocol          6 (ceres)
```

If the output is `Node is not running!` check node logs for errors to debug it further.

### 7. Begin Operations

After your Hyperchain node is running and producing blocks, you can begin interacting with the chain. The recommended next steps are:

1. Connect supporting tools to your chain:
   - Block explorer (e.g., Aescan)
   - Wallet interface (e.g., Superhero Wallet or Base app)

2. Import your test accounts:
   - Access the account mnemonics from `hc_test/economy-unencrypted.yaml`
   - Import these into your preferred wallet (e.g., Superhero Wallet)
   - Configure your wallet's network settings:
     - Node address (e.g., localhost)
     - HTTP API port (default: 3013)
     - Network ID (`hc_test` in this example)

You can now begin testing transactions and interactions with your Hyperchain deployment.

Happy Hyperchaining :)

## Configuring New Validator

> **NOTICE**: This documentation covers beta functionality intended for testing purposes only.

After establishing a Hyperchain, you can add additional validators or join an existing Hyperchain as a validator. This guide demonstrates how to configure a new validator on a Hyperchain using the Aeternity blockchain as the parent chain.

Important Notes:

- The process requires approximately 2 hours before the new validator begins producing blocks
- A minimum stake of 1000000AE is required
- Both a staker account (for the child chain) and a pinner account (for the parent chain) are needed

### Install Aeternity CLI

The Aeternity Command Line Interface (CLI) is required to manage wallets and interact with smart contracts. Install it using npm:

```shell
npm install --global @aeternity/aepp-cli@7
aecli --version
```

### Account Setup

Two accounts are required for validator operation:
1. Staker Account: Holds funds for staking in the Hyperchain consensus (child chain)
2. Pinner Account: Executes pinning transactions on the parent chain

#### Create a Staker Account

First we need to create a staker account, that's the account that will hold the funds used to stake in the Hyperchain PoS consensus.

```shell
aecli account create hc_test/wallets/staker.json
```

Expected output:
```shell
✔ Enter your password …
Address  ak_2hT1UTevtPkEpocjEcbbBi14gS1Ba1unHQBrtXupHnKHxk26kU
Path     /path/to/hyperchain-starter-kit/configs/hc_test/wallets/staker.json
```

Once the account is created, the private key should be extracted as it's needed in the node configuration afterwards:

```shell
aecli account address --secretKey hc_test/wallets/staker.json
```

Expected output:

```shell
✔ Are you sure you want print your secret key? … yes
✔ Enter your password …
Address            ak_2hT1UTevtPkEpocjEcbbBi14gS1Ba1unHQBrtXupHnKHxk26kU
Secret Key         sk_2KaWrN7mJ5yfj5yikQwepSaPXksgjuYda7QoXtjfVhRfFVDJvs
Secret Key in hex  ade11d353b3b02f9602aa7073683a1c2b2a5d95c73b99d8fc40eafa82b02e957df8a07444f1195795a2f92915e655696a3a6a330015c58673ae9f73a1abff374
```

Note the public and private keys of the account, it will be used below in CLI commands and node configuration.

#### Create a Pinner Account

> **Important**: The pinner account must be funded on the parent chain before starting your validator node. For testnet deployment, use the [testnet faucet](https://faucet.aepps.com).

If the validator pinning is enabled, a parent chain account will be needed as well to execute it:

```shell
aecli account create hc_test/wallets/pinner.json
```

Expected output:
```shell
✔ Enter your password …
Address  ak_2CQQctWm267tfHMUcZf7YX5uVHE6T9t3bg4UNpWnpn8qog6MZ
Path     /path/to/hyperchain-starter-kit/configs/hc_test/wallets/pinner.json
```

Once the account is created, the private key should be extracted as it's needed in the node configuration afterwards:

```shell
aecli account address --secretKey hc_test/wallets/pinner.json
```

Expected output:

```shell
✔ Are you sure you want print your secret key? … yes
✔ Enter your password …
Address            ak_2CQQctWm267tfHMUcZf7YX5uVHE6T9t3bg4UNpWnpn8qog6MZ
Secret Key         sk_X8YAj8XRzTEsAdQJZDt6n3baCcCs2mHRwiT4rtBYEE8baBLfX
Secret Key in hex  4469eb651fa54025ccecf2bfe462a2051e690f7b2bb23ea0f49e6f77a7b1763c02b7910b1a543221b15f14b196ed1ba3212031d817a9c09ac200387ab5631a25
```

Note the public and private keys of the account, it will be used below in CLI commands and node configuration.

### Configure the Validator Node

The new validator node needs at least one peer to connect to an existing Hyperchain network.
As this example assuming an already running local node, the peer key can be obtained from its status API endpoint:

```shell
curl -s localhost:3013/v3/status | jq -r '.peer_pubkey'
```

Expected output:
```shell
pp_2ZX5Pae6a9L5UFm8VcCNsB39pn3EK7ZQZpp3dfF1WDNFXZ9p3b
```

This example assumes your initiator runs in a docker container from the example above and its address would be `initiator` (the container name). If the initiator node runs outside docker container, the address will be `localhost`.

So, the peer URL is: `aenode://pp_2ZX5Pae6a9L5UFm8VcCNsB39pn3EK7ZQZpp3dfF1WDNFXZ9p3b@initiator:3015`
That will be configured under the `peers` configuration key below.

The `accounts.json` and `contracts.json` will be reused from the previous example, but one can duplicate them as well in new directory as needed.

Copy the node configuration from the previous example:
```shell
cp hc_test/nodeConfig/aeternity.yaml hc_test/nodeConfig/validator2.yaml
```

The `validator2.yaml` configuration must be changed to remove the initial stakers and pinners and replace it with the new accounts created above and add `peers`, **everything else should stay intact**:

```yaml
peers:
  # Connect to existing Hyperchain network
  - aenode://pp_2ZX5Pae6a9L5UFm8VcCNsB39pn3EK7ZQZpp3dfF1WDNFXZ9p3b@initiator:3015
chain:
  consensus:
    '0':
      config:
        child_block_time: 3000
        child_epoch_length: 600
        contract_owner: 'ak_11111111111111111111111111111115rHyByZ'
        default_pinning_behavior: true
        election_contract: 'ct_LRbi65kmLtE7YMkG6mvG5TxAXTsPJDZjAtsPuaXtRyPA7gnfJ'
        fixed_coinbase: 100000000000000000000
        parent_chain:
          consensus:
            network_id: 'ae_uat'
            type: 'AE2AE'
          parent_epoch_length: 10
          polling:
            fetch_interval: 500
            nodes:
              - 'https://testnet.aeternity.io'
          start_height: 1064939
        pinners:
          - parent_chain_account:
              owner: 'ak_2hT1UTevtPkEpocjEcbbBi14gS1Ba1unHQBrtXupHnKHxk26kU'
              priv: '4469eb651fa54025ccecf2bfe462a2051e690f7b2bb23ea0f49e6f77a7b1763c02b7910b1a543221b15f14b196ed1ba3212031d817a9c09ac200387ab5631a25'
              pub: 'ak_2CQQctWm267tfHMUcZf7YX5uVHE6T9t3bg4UNpWnpn8qog6MZ'
        pinning_reward_value: 1000000000000000000000
        rewards_contract: 'ct_KJgjAXMtRF68AbT5A2aC9fTk8PA4WFv26cFSY27fXs6FtYQHK'
        stakers:
          - hyper_chain_account:
              priv: 'ade11d353b3b02f9602aa7073683a1c2b2a5d95c73b99d8fc40eafa82b02e957df8a07444f1195795a2f92915e655696a3a6a330015c58673ae9f73a1abff374'
              pub: 'ak_2hT1UTevtPkEpocjEcbbBi14gS1Ba1unHQBrtXupHnKHxk26kU'
        staking_contract: 'ct_KJgjAXMtRF68AbT5A2aC9fTk8PA4WFv26cFSY27fXs6FtYQHK'
      type: 'hyperchain'
  hard_forks:
    '6':
      accounts_file: 'hc_test_accounts.json'
      contracts_file: 'hc_test_contracts.json'
      height: 0
fork_management:
  network_id: 'hc_test'
mining:
  autostart: true
http:
  endpoints:
    dry-run: true
    hyperchain: true
```

**Note that account addresses, account keys and peer keys will be different on different runs, make sure to copy the correct ones!**

### Start the Node

This example assumes you have a Hyperchain validator node (initiator) already running on localhost in a Docker container within a network named hyperchain. Since the node's API ports are exposed to the host, we need to use alternative ports to prevent conflicts. In this example, we will remap port 3013 to 33013 while keeping the default port in the node configuration.

```shell
docker run --rm -it -p 33013:3013 \
    --network hyperchain \
    -v ${PWD}/hc_test/nodeConfig/validator2.yaml:/home/aeternity/.aeternity/aeternity/aeternity.yaml \
    -v ${PWD}/hc_test/nodeConfig/hc_test_accounts.json:/home/aeternity/node/data/aecore/hc_test_accounts.json \
    -v ${PWD}/hc_test/nodeConfig/hc_test_contracts.json:/home/aeternity/node/data/aecore/hc_test_contracts.json \
    aeternity/aeternity:v7.3.0-rc3
```

Verify the new validator node is running with:
```shell
curl -s localhost:33013/v3/status | jq
```

Expected output:
```yaml
{
  "difficulty": 0,
  "genesis_key_block_hash": "kh_7dm2zSo6NsnEDMYBYdXA9QvkJvvk7TenT68HxW5BSrRSz3WV6",
  "hashrate": 0,
  "listening": true,
  "network_id": "hc_test",
  "node_revision": "57bc00b760dbb3ccd10be51f447e33cb3a2f56e3",
  "node_version": "7.3.0-rc3",
  "peer_connections": {
    "inbound": 0,
    "outbound": 1
  },
  "peer_count": 1,
  "peer_pubkey": "pp_ENJfMAPXWbZruYgexnGCNEwCBwdLcEiR8F541CuzCkgtFQ5jt",
  "pending_transactions_count": 0,
  "protocols": [
    {
      "effective_at_height": 0,
      "version": 6
    }
  ],
  "solutions": 0,
  "sync_progress": 100,
  "syncing": false,
  "top_block_height": 35,
  "top_key_block_hash": "kh_FK7EvnCYsq9DdbZLtBpFPdD5UdTNYGWnWfQShNgVkbY6iyP3h",
  "uptime": "59s.448"
}
```

Make sure that:

- `genesis_key_block_hash` is the same as the status output of the initiator node
- `network_id` is the same as the status output of the initiator node
- `peer_count` is more than 0
- `top_block_height` is more than 0
- `sync_progress` is 100

The above should confirm the new validator is up, running and connected to the initiator Hyperchain network!

### Register New Validator

Once the new node is running it's time to register it as validator with the Hyperchain consensus.

#### CLI Node Config
First the CLI have to be configured to work with the locally running Hyperchain network created in this example:
```shell
aecli select-node http://localhost:3013/
```

Verify the configuration by running:
```shell
aecli config
```

Expected output:
```shell
Node http://localhost:3013/ network id hc_test, version 7.3.0-rc3, protocol 6 (Ceres)
Compiler https://v8.compiler.aepps.com/ version 8.0.0
```

#### Funding

**The staker account must be funded with at least the minimum staking amount (1000000AE in this example) and some extra for transacting. In this example the treasury account can be used by loading it in a wallet**

With the help of the CLI the treasury secret key is imported then used to send tokens to the new staker account. The treasury private key can be found in `hc_test/economy-unencrypted.yaml`.

```shell
aecli account create hc_test/wallets/treasury.js 9ab3bb473a0ec30d4f75f877ee9de0145fbd883e07edab2e4555e156a729be5619bd0a236a9ebd4a47e5fc3961b59ea215515109c007e2ce844b1f543d4e9598
```

Output:
```shell
✔ Enter your password …
Address  ak_CLTKb9tdvXGwpgUBW8GytW7kXv9r1eJyY4YtgKKWtKcY3poQf
Path     /path/to/hyperchain-starter-kit/configs/hc_test/wallets/treasury.js
```

Verify the treasury balance:
```shell
aecli inspect ak_CLTKb9tdvXGwpgUBW8GytW7kXv9r1eJyY4YtgKKWtKcY3poQf
```

Output:
```shell
Account ID       ak_CLTKb9tdvXGwpgUBW8GytW7kXv9r1eJyY4YtgKKWtKcY3poQf
Account balance  1000000000000000000000000000000ae
Account nonce    0
No pending transactions
```

Fund the staker account from the treasury wallet:
```shell
aecli spend hc_test/wallets/treasury.js ak_2hT1UTevtPkEpocjEcbbBi14gS1Ba1unHQBrtXupHnKHxk26kU 1000100ae
```

Output:
```shell
Cost of SpendTx execution ≈ 1000100.0000169ae
✔ Enter your password …
Transaction mined
Transaction hash   th_2JLcgQkpD4Rz42L3rmKsLoZ2fomGuoVNpoJjnAMxTcbmoznjbb
Block hash         mh_2uWKDfDj5evXncvJd6V7c76pQdhYuKWDUjxP2jqr5cj7BW9b99
Block height       2052 (about now)
Signatures         ["sg_BYKqfbZej3CPUHz9sJfHRM5o8YLevTjhoU3JAHivU8d8wv9L8TuA2kx74vm5xTHSmFQxo2d4uDBJKWuz9K5QmTDCUNDLc"]
Transaction type   SpendTx (ver. 1)
Sender address     ak_CLTKb9tdvXGwpgUBW8GytW7kXv9r1eJyY4YtgKKWtKcY3poQf
Recipient address  ak_2hT1UTevtPkEpocjEcbbBi14gS1Ba1unHQBrtXupHnKHxk26kU
Amount             1000100ae
Payload            ba_Xfbg4g==
Fee                0.0000169ae
Nonce              1
TTL                2053 (about now)
```

#### Validator Registration
The registration happens with a contract call to the `staking_contract` address, in this example: `ct_KJgjAXMtRF68AbT5A2aC9fTk8PA4WFv26cFSY27fXs6FtYQHK`.

For a reference *(not shell command)*, the **Sophia** function signature that needs to be called is:
```solidity
payable stateful entrypoint new_validator(owner : address, sign_key : address, restake : bool) : StakingValidator
```

The same address (`ak_2hT1UTevtPkEpocjEcbbBi14gS1Ba1unHQBrtXupHnKHxk26kU`) will be used for owner and `sign_key`, `restake` will be set to `false`.

```bash
aecli contract call new_validator '["ak_2hT1UTevtPkEpocjEcbbBi14gS1Ba1unHQBrtXupHnKHxk26kU", "ak_2hT1UTevtPkEpocjEcbbBi14gS1Ba1unHQBrtXupHnKHxk26kU", false]' hc_test/wallets/staker.json --contractAddress ct_KJgjAXMtRF68AbT5A2aC9fTk8PA4WFv26cFSY27fXs6FtYQHK --contractAci hc_test/contracts/MainStaking.aci.json --amount 1000000ae
```

Output:
```shell
Cost of ContractCallTx execution ≈ 1000000.000236691ae
✔ Enter your password …
Transaction hash  th_4VXdLJjpAXAnGMe7RRfwKtPguXGWJaLVLoKR7EgcVe1w2MGtk
Block hash        mh_2XhasD3FXo4RSyKdrkUMw8NQdDx2bkXKhZJfV1idwXjUeFHPEk
Block height      2512 (about now)
Signatures        ["sg_R5STDGXkZyCejyiyahzJJLGN8GThrZsCH3hY6e7GkLb8eebz8sHbj8ECe21tscXw2Eh2zDvaryyuy9fnCvNX8wewciXER"]
Transaction type  ContractCallTx (ver. 1)
Caller address    ak_2hT1UTevtPkEpocjEcbbBi14gS1Ba1unHQBrtXupHnKHxk26kU
Contract address  ct_KJgjAXMtRF68AbT5A2aC9fTk8PA4WFv26cFSY27fXs6FtYQHK
Gas               53031 (0.000053031ae)
Gas price         0.000000001ae
Call data         cb_KxFuGm1JO58AoN+KB0RPEZV5Wi+SkV5lVpajpqMwAVxYZzrp9zoav/N0nwCg34oHRE8RlXlaL5KRXmVWlqOmozABXFhnOun3Ohq/83R/GCn/XQ==
ABI version       3 (Fate)
Amount            1000000ae
Fee               0.00018366ae
Nonce             1
TTL               2514 (in 6 minutes)
----------------------Call info-----------------------
Gas used                42425 (0.000042425ae)
Return value (encoded)  cb_nwKgZYU1vdfnMJUPDrJIhcZ5xLhgivcS2Y7lZhl77npIApLx8dm1
Return value (decoded)  ct_miCfZgeT2fEE9E7XpFikAUReP62QJtZizypJGvw38SYuXJNHN
```

Now the new validator is registered as part of the consensus! The return value `ct_miCfZgeT2fEE9E7XpFikAUReP62QJtZizypJGvw38SYuXJNHN` is the validator contact address.

**Please note that the new validator will start producing blocks after 4 epochs (2400 blocks), that's after 2 hours in this example!**

## Configuration Explained

An Aeternity Hyperchain node is built on a standard Aeternity release with specific consensus configuration. After completing the installation and basic configuration, you'll need to add the following Hyperchain-specific configurations to your aeternity.yaml or .json file.
Please follow the [installation instructions](installation.md) and do [basic configuration](configuration.md). Then adopt and add the additional configuration below to your `aternity.yaml` or (`.json`).

### Parent chain
<!--
Running your parent node locally is recommended for optimal control and performance. In this example configuration, we use a testnet node running on port 6013. For detailed instructions on setting up and configuring a regular Aeternity node on testnet, refer to the core configuration documentation. -->

To configure your parent chain connection, you'll need to specify several key parameters. Start by setting the chain connector to AE2AE and network ID to ae_uat. Next, configure your parent chain node address using the parent node's HTTP API port, not the external port. When setting the fetch interval, ensure it's frequent enough to capture all state changes on the parent chain.
Finally, specify a future block height on the parent chain as your starting point. This height determines when your child chain will begin its operations, including block production and pinning. This configuration creates a deliberate delay, allowing for proper initialization and synchronization:

```yaml
parent_chain:
  consensus:
    network_id: 'ae_uat'
    type: 'AE2AE'
  polling:
    fetch_interval: 500
    nodes:
      - 'http://localhost:6013'
  start_height: 1064531
```

### Block times and epochs

The initial "speed" of the Hyperchain is defined by the epoch and block production parameters. These will be different for each Hyperchain, depending on the desired transactional characteristics of the chain. The values below are useful for testing purposes, and when running on Aeternity `testnet`. Please see the [main Hyperchain documentation](https://github.com/aeternity/hyperchains-whitepaper/blob/master/Periodically-Syncing-HyperChains.md) for more information in how to optimize block and epoch parameters per application and parent chain characteristics.

First a parent chain epoch length should be picked. That should be long enough to statistically easy any block time fluctuations.
In this example with Aeternity `testnet` as parent chain, 10 blocks should be enough.
```yaml
parent_chain:
  parent_epoch_length: 10
```

This version of Hyperchains supports block times as low as 3 seconds for various reasons (i.e. network latency, disk latency, etc.) That can be set as (in milliseconds):
```yaml
child_block_time: 3000
```

And lastly, but very important is the child epoch length. It is *mandatory* to have equal parent and child epoch length in wall clock times:
```
parent_block_time * parent_epoch_length = child_block_time * child_epoch_length
(parent_block_time * parent_epoch_length)/child_block_time = child_epoch_length
```

In this example the Aeternity `testnet` is having 180s block times, so:
```
(180 * 10)/3 = 600
```
```yaml
child_epoch_length: 3000
```

### Stakers

We define three `stakers`, that need to have accounts and funds to stake on the Hyperchain. Once a staker becomes leader, their private key is used to sign produced blocks by the node.

```yaml
stakers:
  - hyper_chain_account:
      priv: 'fca30982735665d39d52146c30837cc33cf1c11015accb5ce520065223872e3968c7c7dc1372514ada09eac4a433ee479bf980c6d9d8bbdc88284cb19e8b4cb6'
      pub: 'ak_o9UXHESE3mDd9qeDKnxkRrfUS3eY17xkhshZX3VFqAbkVMUDV'
  - hyper_chain_account:
      priv: '43a889d992a923e195cecf0f4346e4f480cec0ef2f561e383cf6b367e4da8b3dfb245ed4f23bcc25205d1cba30c85ed3a9a3a10858e12a6944d81944d3277850'
      pub: 'ak_2uc64DEjX1XCe2dYtTaDBfGXqPDiv1Xcu29T4P2x5NzXnRerAA'
  - hyper_chain_account:
      priv: '30267f551a56954f654edaa3cb21a58a21898af0519503d3737f8cd5c8b913eb17e9b9a1f3f725b0de559f30276ff065a84cca01b5720df302e2044032acd6ba'
      pub: 'ak_BXprz9o24a8RYbvBRDsUCBdGpz2uULBnDw6BiYpG957BiEvVn'
```

### Accounts

When starting a Hyperchain it needs some pre-funded accounts as most other chains and types. At least the staker's accounts must be funded so that they can be registered with some initial stake to boot the consensus. In this example there are 3 stakers pre-funded with `3100000000000000000000000000` each so they can be registered at genesis time.
Also some additional accounts like Faucet (`ak_WNkJkmaEoyScVRaeZeDvvmCqLn1HkfNnQDy1oSJp6Jm5YPpAq`) and Treasury (`ak_CLTKb9tdvXGwpgUBW8GytW7kXv9r1eJyY4YtgKKWtKcY3poQf`) are also funded for easing the chain usage.

```json
  {
    "ak_CLTKb9tdvXGwpgUBW8GytW7kXv9r1eJyY4YtgKKWtKcY3poQf": 1000000000000000000000000000000000000000000000000,
    "ak_WNkJkmaEoyScVRaeZeDvvmCqLn1HkfNnQDy1oSJp6Jm5YPpAq": 1000000000000000000000000000,
    "ak_o9UXHESE3mDd9qeDKnxkRrfUS3eY17xkhshZX3VFqAbkVMUDV": 3100000000000000000000000000,
    "ak_2uc64DEjX1XCe2dYtTaDBfGXqPDiv1Xcu29T4P2x5NzXnRerAA": 3100000000000000000000000000,
    "ak_BXprz9o24a8RYbvBRDsUCBdGpz2uULBnDw6BiYpG957BiEvVn": 3100000000000000000000000000
  }
```

### Contracts

Hyperchains operate through FATE contracts which must be compiled and provided to the node. These contracts require initialization during the first (genesis) block of the Hyperchain, which involves specific contract calls. While this configuration can be manually set in contracts.json, the process is technically complex. We recommend using our purpose-built CLI tools to handle contract deployment and initialization.

In this example two contracts are deployed: MainStaking.aes and HCElection.aes then there are three contract calls to register the stakers/validators, 3 in this example.

Please refer to the [`hyperchain-starter-kit` documentation](#generate-configuration) above how to generate the files.

```json
{
  "contracts": [
    {
      "abi_version": 3,
      "vm_version": 8,
      "amount": 0,
      "nonce": 1,
      "call_data": "cb_KxFE1kQfG2+K08IbzsztoP//wBN1Y+0=",
      "code": "cb_+RooRgOgOUIJ6aG22mq1hxGo9uIfqnsWK5hy9d6LWKhQXT42NwfAuRn6uRM9....SNIP",
      "owner_pubkey": "ak_11111111111111111111111111111115rHyByZ",
      "pubkey": "ct_KJgjAXMtRF68AbT5A2aC9fTk8PA4WFv26cFSY27fXs6FtYQHK"
    },
    {
      "abi_version": 3,
      "vm_version": 8,
      "amount": 0,
      "nonce": 2,
      "call_data": "cb_KxFE1kQfG58CoCmQRGuzLDSzySG/5UcFOdoQ1iYy6a6fhJXVahjufrESSdmdMQ==",
      "code": "cb_+Q2cRgOgT4Um34MEjJkEOajx7RyKC+E7kWgbLSK4Hgf8VYE1Q4vAuQ1uuQoT/...SNIP",
      "owner_pubkey": "ak_11111111111111111111111111111115rHyByZ",
      "pubkey": "ct_LRbi65kmLtE7YMkG6mvG5TxAXTsPJDZjAtsPuaXtRyPA7gnfJ"
    }
  ],
  "calls": [
    {
      "abi_version": 3,
      "amount": 1000000000000000000000000,
      "call_data": "cb_KxFuGm1JO58AoGjHx9wTclFK2gnqxKQz7keb+YDG2di73IgoTLGei0y2nwCgaMfH3BNyUUraCerEpDPuR5v5gMbZ2LvciChMsZ6LTLZ/AEXInw==",
      "caller": "ak_o9UXHESE3mDd9qeDKnxkRrfUS3eY17xkhshZX3VFqAbkVMUDV",
      "contract_pubkey": "ct_KJgjAXMtRF68AbT5A2aC9fTk8PA4WFv26cFSY27fXs6FtYQHK",
      "nonce": 1,
      "fee": 1000000000000000,
      "gas": 1000000,
      "gas_price": 1000000000
    },
    {
      "abi_version": 3,
      "amount": 1000000000000000000000000,
      "call_data": "cb_KxFuGm1JO58AoPskXtTyO8wlIF0cujDIXtOpo6EIWOEqaUTYGUTTJ3hQnwCg+yRe1PI7zCUgXRy6MMhe06mjoQhY4SppRNgZRNMneFB/pplUGQ==",
      "caller": "ak_2uc64DEjX1XCe2dYtTaDBfGXqPDiv1Xcu29T4P2x5NzXnRerAA",
      "contract_pubkey": "ct_KJgjAXMtRF68AbT5A2aC9fTk8PA4WFv26cFSY27fXs6FtYQHK",
      "nonce": 1,
      "fee": 1000000000000000,
      "gas": 1000000,
      "gas_price": 1000000000
    },
    {
      "abi_version": 3,
      "amount": 1000000000000000000000000,
      "call_data": "cb_KxFuGm1JO58AoBfpuaHz9yWw3lWfMCdv8GWoTMoBtXIN8wLiBEAyrNa6nwCgF+m5ofP3JbDeVZ8wJ2/wZahMygG1cg3zAuIEQDKs1rp/TbwekQ==",
      "caller": "ak_BXprz9o24a8RYbvBRDsUCBdGpz2uULBnDw6BiYpG957BiEvVn",
      "contract_pubkey": "ct_KJgjAXMtRF68AbT5A2aC9fTk8PA4WFv26cFSY27fXs6FtYQHK",
      "nonce": 1,
      "fee": 1000000000000000,
      "gas": 1000000,
      "gas_price": 1000000000
    }
  ]
}
```

Once the contracts are generated they have to be also added in the node consensus configuration. Current configuration expect 3 separate contracts:

```yaml
staking_contract: 'ct_KJgjAXMtRF68AbT5A2aC9fTk8PA4WFv26cFSY27fXs6FtYQHK'
election_contract: 'ct_LRbi65kmLtE7YMkG6mvG5TxAXTsPJDZjAtsPuaXtRyPA7gnfJ'
rewards_contract: 'ct_KJgjAXMtRF68AbT5A2aC9fTk8PA4WFv26cFSY27fXs6FtYQHK'
```

However, the current implementation is using the staking contract instance for rewards distribution, thus the addresses are the same.

Finally the contract owner should be set the same as the owner of the contract deployment in the `contracts.json`:

```yaml
contract_owner: 'ak_11111111111111111111111111111115rHyByZ'
```

This ensures that the protocol methods can be called only by the protocol and not other accounts. In this example the so called "zero" address is used for that.

### Pinning

Currently only a single pinning behavior is supported. Validators will automatically pin the Hyperchain state to the parent chain when they are leaders of the last block of the epoch using their mapped parent account credentials. It can be enabled by:

```yaml
default_pinning_behavior: true
```

That feature also needs pinning accounts configuration, it's map of staker to pinning account, for example:

```yaml
pinners:
  - parent_chain_account:
      owner: 'ak_o9UXHESE3mDd9qeDKnxkRrfUS3eY17xkhshZX3VFqAbkVMUDV'
      priv: '9e2c28f2037e6d352a88137572255ed8af2e86dfb2c2949c4d805ff08bff2784d465bc1253fd1594ecc68e128bb2e241fb2fc0bd48a72e7d57af6dadad67b5dc'
      pub: 'ak_2cYQac22msS2BGxR44BiD3CSLDvG9Xuamn61y2gdb8qJXwd7or'
  - parent_chain_account:
      owner: 'ak_2uc64DEjX1XCe2dYtTaDBfGXqPDiv1Xcu29T4P2x5NzXnRerAA'
      priv: 'c447e8e3159a57ebffb19f3bd7c63afd739e5c592d9bd20620dff61c58d842b062b2dd406ded9a12fb6e455ce2fe3eb1af96182f5a59697e04478e372c58c927'
      pub: 'ak_kU8HHb7raYHf5USrEv1zQ1WfFU8Fccrzp2nPM7gtoAvi3cFZh'
  - parent_chain_account:
      owner: 'ak_BXprz9o24a8RYbvBRDsUCBdGpz2uULBnDw6BiYpG957BiEvVn'
      priv: 'a19771d213eadede7c3332244ddeaa5e2caaa8be4aae79eee8d425e47a237dffafd27c0d8b8485c6dab5ec083448f81e68de9202258ee9f5ec313874bb0e2537'
      pub: 'ak_2LS9FZLu7v5mVHW8sHa8DxvVWzfKf39SHrHNmmvQseTWCbff5J'
```

To encourage validators to pin epoch states to the parent chain, the system provides rewards in the Hyperchain's native currency. The reward amount for each successful pinning operation can be configured to align with your Hyperchain's economic model. This value represents the compensation validators receive for performing verifiable pinning operations for each epoch.

```yaml
pinning_reward_value: 1000000000000000000000
```

### Complete configuration example

```yaml
chain:
  consensus:
    '0':
      config:
        child_block_time: 3000
        child_epoch_length: 600
        contract_owner: 'ak_11111111111111111111111111111115rHyByZ'
        default_pinning_behavior: true
        election_contract: 'ct_LRbi65kmLtE7YMkG6mvG5TxAXTsPJDZjAtsPuaXtRyPA7gnfJ'
        fixed_coinbase: 100000000000000000000
        fee_distribution: [30, 50, 20]
        parent_chain:
          consensus:
            network_id: 'ae_uat'
            type: 'AE2AE'
          parent_epoch_length: 10
          polling:
            fetch_interval: 500
            nodes:
              - 'https://testnet.aeternity.io'
          start_height: 1064939
        pinners:
          - parent_chain_account:
              owner: 'ak_o9UXHESE3mDd9qeDKnxkRrfUS3eY17xkhshZX3VFqAbkVMUDV'
              priv: '9e2c28f2037e6d352a88137572255ed8af2e86dfb2c2949c4d805ff08bff2784d465bc1253fd1594ecc68e128bb2e241fb2fc0bd48a72e7d57af6dadad67b5dc'
              pub: 'ak_2cYQac22msS2BGxR44BiD3CSLDvG9Xuamn61y2gdb8qJXwd7or'
          - parent_chain_account:
              owner: 'ak_2uc64DEjX1XCe2dYtTaDBfGXqPDiv1Xcu29T4P2x5NzXnRerAA'
              priv: 'c447e8e3159a57ebffb19f3bd7c63afd739e5c592d9bd20620dff61c58d842b062b2dd406ded9a12fb6e455ce2fe3eb1af96182f5a59697e04478e372c58c927'
              pub: 'ak_kU8HHb7raYHf5USrEv1zQ1WfFU8Fccrzp2nPM7gtoAvi3cFZh'
          - parent_chain_account:
              owner: 'ak_BXprz9o24a8RYbvBRDsUCBdGpz2uULBnDw6BiYpG957BiEvVn'
              priv: 'a19771d213eadede7c3332244ddeaa5e2caaa8be4aae79eee8d425e47a237dffafd27c0d8b8485c6dab5ec083448f81e68de9202258ee9f5ec313874bb0e2537'
              pub: 'ak_2LS9FZLu7v5mVHW8sHa8DxvVWzfKf39SHrHNmmvQseTWCbff5J'
        pinning_reward_value: 1000000000000000000000
        rewards_contract: 'ct_KJgjAXMtRF68AbT5A2aC9fTk8PA4WFv26cFSY27fXs6FtYQHK'
        stakers:
          - hyper_chain_account:
              priv: 'fca30982735665d39d52146c30837cc33cf1c11015accb5ce520065223872e3968c7c7dc1372514ada09eac4a433ee479bf980c6d9d8bbdc88284cb19e8b4cb6'
              pub: 'ak_o9UXHESE3mDd9qeDKnxkRrfUS3eY17xkhshZX3VFqAbkVMUDV'
          - hyper_chain_account:
              priv: '43a889d992a923e195cecf0f4346e4f480cec0ef2f561e383cf6b367e4da8b3dfb245ed4f23bcc25205d1cba30c85ed3a9a3a10858e12a6944d81944d3277850'
              pub: 'ak_2uc64DEjX1XCe2dYtTaDBfGXqPDiv1Xcu29T4P2x5NzXnRerAA'
          - hyper_chain_account:
              priv: '30267f551a56954f654edaa3cb21a58a21898af0519503d3737f8cd5c8b913eb17e9b9a1f3f725b0de559f30276ff065a84cca01b5720df302e2044032acd6ba'
              pub: 'ak_BXprz9o24a8RYbvBRDsUCBdGpz2uULBnDw6BiYpG957BiEvVn'
        staking_contract: 'ct_KJgjAXMtRF68AbT5A2aC9fTk8PA4WFv26cFSY27fXs6FtYQHK'
      type: 'hyperchain'
  hard_forks:
    '6':
      accounts_file: 'hc_test_accounts.json'
      contracts_file: 'hc_test_contracts.json'
      height: 0
fork_management:
  network_id: 'hc_test'
mining:
  autostart: true
```

----
----
# Addendum 1: Hyperchain-specific confguration reference
## chain\.consensus: object

The consensus algorithms used for validating blocks. Ignored if 'fork_management > network_id' has value
'ae_mainnet' or 'ae_uat'.


**Properties (Pattern)**

|Name|Type|Description|Required|
|----|----|-----------|--------|
|[\<height\>](#chainconsensus1-90-9)|`object`|Property name indicates minimum height at which the given
consensus algorithm gets activated. By default from genesis Cuckoo cycle based BitcoinNG is used.<br/>|yes|

<a name="chainconsensus1-90-9"></a>
## chain\.consensus\.\<height\>: object

Configuration parameters for the selected consensus algorithm (Hyperchain).


**Properties**

|Name|Type|Description|Required|
|----|----|-----------|--------|
|**type**|`string`|The type of the consensus algorithm used at the given height (ex. pow_cuckoo, smart_contract
or hyperchain)<br/>Default: `"hyperchain"`<br/>|yes|
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
  fee_distribution: [30, 50, 20]
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


----
<a name="hc_specific_fate_behavior"></a>
# Addendum 2: Hyperchain-specific FATE/Contract behavior

A HyperChain is a Proof-of-stake chain, and it is organized differently from
Aeternity Mainnet. See the
[whitepaper](https://github.com/aeternity/hyperchains-whitepaper/blob/master/Periodically-Syncing-HyperChains.md)
for details. This means that some of the chain-inspection operations
(`Chain.block\_hash`, `Chain.coinbase`, and `Chain.difficulty`) can't work
exactly the same. Here we summarize the differences and current state.

### Chain.block\_hash

Change: `Chain.block\_hash(current_height)` returns `None`

The block hash (at height) in Sophia/FATE always refer to the hash of the
key-block. (Most of the time in Bitcoin-NG there will be many micro-blocks at
the same height, so it is not unique.) And, in a HyperChain the micro-block
(where the contract calls happen) comes _before_ the key-block, and thus there
simply isn't a block hash to return.

### Chain.coinbase

Calls to `Chain.coinbase` will fail and abort the execution.

The same reason as for block hash, the key-block isn't yet produced so there is
no (simple) place to find the information. It is always possible to call the
consensus contract and check who is the producer of the current
block/generation.

### Chain.difficulty

Calls to `Chain.difficulty` will return `0`.

There is no mining, so no difficulty to keep track of. Internally, the
`difficulty` field is used for other purposes, but that number makes little
sense in Sophia/FATE so no point returning it.
