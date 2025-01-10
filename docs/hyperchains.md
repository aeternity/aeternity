# Introduction

This document describes the basics of he Aeternity Hyperchain, with links to more detailed documentation.
It also covers a reference to the Hyperchain related Aeternity node configuration parameters and a
quick-start guide on how to configure your Hyperchain node.

Hyperchains represents a groundbreaking evolution in blockchain architecture, combining the security benefits
of established Proof-of-Work (PoW) chains with the efficiency and scalability of Proof-of-Stake (PoS) systems.
By implementing a child chain that periodically synchronizes with a parent PoW chain, Hyperchains enables
organizations to build robust, scalable blockchain networks while leveraging the established security of
existing chains. This hybrid approach significantly reduces energy consumption compared to traditional PoW
systems while maintaining strong security guarantees through its innovative parent-child chain relationship.

The 0.9 beta release introduces the core framework for Hyperchain deployment, providing developers with the
foundation to test and validate the Hyperchains architecture. This initial release focuses on establishing
and maintaining the fundamental relationship between an Aeternity parent chain and its child chain,
implementing the basic mechanisms for chain synchronization and validator operations. This documentation
serves as a technical guide for configuring and deploying a Hyperchain, aimed at developers who are already
running Aeternity nodes and want to experiment with Hyperchain functionality. It covers the essential
configuration parameters and setup requirements needed to participate in this beta testing phase.

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

## Getting Started

The easiest way to start playing with Hyperchains is the so called [Localnet](https://github.com/aeternity/localnet/blob/master/README.md#hyperchains-configuration).
It's a Docker compose configuration that is pre-configured with Aeternity parent chain, Hyperchain and all the tools for both networks for playing around.
This is setup is also supposed to be used for development purposes as we aim to keep it up-to-date with all the tools and node versions.

## Configuring Your Own Hyperchain Node

To setup a Hyperchain can be a fairly complex process. Aeternity provides some tooling to help out. We
will use the [`hyperchain-starter-kit`](https://github.com/aeternity/hyperchain-starter-kit) (from Aeternity
GitHub org) to do a lot of the heavy lifting.

**Please note that this tool is still in development and might have some warnings and bugs, we'd also work to improve it's user friendliness and stability.**

### Requirements:

- a runnable Node installation (see below)
- [Node.js](https://nodejs.org/en/download) installed to run the `hyperchain-starter-kit`
- [Git](https://git-scm.com/downloads) installed, to download the code for the starter kit

Note: the following instructions are based on Unix/linux/MacOS shell commands.
Using Windows can look slightly different w/r paths etc.

### Install the tool

```shell
git clone https://github.com/aeternity/hyperchain-starter-kit
cd hyperchain-starter-kit
npm install
npm run dev
```

### Tool configuration

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
contractSourcesPrefix: 'https://raw.githubusercontent.com/aeternity/aeternity/refs/tags/v7.3.0-rc2/'
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

### Contracts

To prepare the Hyperchain contracts run the following:
```shell
npm run dev retrieve-contracts hc_test
```

This will download the standard contracts from the latest Aeternity release and compile them in `contracts` sub-directory.

### Economy

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

### Generate Configuration

To generate all the configuration files needed to run a Hyperchain, run:
```shell
npm run dev gen-node-conf hc_test
```

This will create 3 files in `nodeConfig` directory:

- `aeternity.yaml`
- `hc_test_accounts.json`
- `hc_test_contracts.json`

Copy all of the above files to their node corresponding directory, i.e. assuming it's in `~/aeternity/node`:

```shell
cp ./hc_test/nodeConfig/aeternity.yaml ~/aeternity/node/
cp ./hc_test/nodeConfig/hc_test_*.json ~/aeternity/node/data/aecore/
```

**Don't forget to fund all pinners accounts on the parent chain prior starting your node/validator.**

then run your node:
```shell
~/aeternity/node/bin/aeternity start
```

**IMPORTANT:**
If you used a known public chain (testnet or mainnet) as parent chain, the tool will set the `start_height` as current block + 10, that is 30 minutes in future.
Keep that in mind when verifying your chain, either decrease the number or wait until that block is produced on the parent chain before you start transacting on the Hyperchain.

### Status

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

### Usage

Once your Hyperchain node is running, and there is some block production, one can start transacting on the Hyperchain.
Connecting other tools to the chain should be proper next step, like explorer (Aescan), a favorite wallet like Superhero or Base app.

For example import some of the economy accounts to an wallet i.e. the treasury by using it's mnemonic in the `hc_test/economy-unencrypted.yaml` to an wallet i.e. Superhero Wallet.
Then add a custom network with your node address (i.e. localhost) and it's HTTP API port (3013 by default) with the proper network ID witch is `hc_test` in this example.

Happy Hyperchaining :)

## Configuration Explained

An Aeternity Hyperchain node is a standard Aeternity release configured to run a Hyperchain-configured consensus
algorithm. Please follow the [installation instructions](installation.md) and do [basic configuration](configuration.md). Then adopt and add the additional configuration below to your `aternity.yaml` or (`.json` if that is your fancy).

### Parent chain

The recommended setup is to run your parent node locally, in the example below we have a `testnet` node running on port `6013` (as defined in `parent_chain.polling.nodes`). Again, refer to the [core configuration documentation](configuration.md) above how to install and configure a regular Aeternity node on `testnet`.

First parent chain connector: (`AE2AE`) and network ID (`ae_uat`) must be set.
Then a parent chain node address is configured. The port used should be the parent not external HTTP API port (see the above). A fetch interval should be picked short enough to cover all state changes of the parent.
In addition a block height of the parent chain should be picked sometime in the future. That's the parent height at witch the child chain will actually start (i.e. producing blocks, pinning, etc.):

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

Hyperchains are run by FATE contracts, they need to be compiled and made available to the node.
Depending on the actual contracts used by the Hyperchain, they might also need to be initialized on first (genesis) Hyperchain block, that involves contract calls. That's done in the `contracts.json` configuration, but it's highly technical so the recommended way is to use a CLI tools especially developed for that purpose.

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
      "code": "cb_+RooRgOgOUIJ6aG22mq1hxGo9uIfqnsWK5hy9d6LWKhQXT42NwfAuRn6uRM9/gGcurQCNwNHADcGRwBHAAcHZwcHFwc3ACgcBgIiIIgHDAQBAz8oHg4IAigcBgItGhAOBCmcCAIQLRqCggABAz/+AjAQIgI3AUcANwAvGIIABwwE+wNpTm90IGEgcmVnaXN0ZXJlZCB2YWxpZGF0b3IBAz/+AxmYDwI3Avf39ygeAgICKC4GAgIoLgoGAiIoCogHDAQBAwMMAwMMAgYMAgonDAQ0AAD+E3fYBgI3AifnACfnACfnADMEAAcMBDUGAAA2BQAANBkCAAIGAwABAQL+GjHkbAI3AkcABzcADAECKxiCAAIDEersXDUtGoKCAAEDP/4eha1SAjcC9/f3DAMDDAEAKBwCAigcAAICAxEBnLq0NAAA/ia0rRACNwEn5wAn5wAMAwMMAQAEAxETd9gG/ioU54YCNwJHAAcHGgoAgisYAAAoDAgs0AIAAP4tEMm9BDcANwALAB8wAAcMBPsDeURlcG9zaXQgbmVlZHMgYSBwb3NpdGl2ZSB2YWx1ZVUAAgMRAjAQIg8Cb4ImzwsAVQAEAxEaMeRs/i9lBWQANwFHAAcaCgCCKxoCAAAMAgICAxGfFwXlKCwEAhUAAP44jgRvADcAJzcCRwAHGgoAgjIIAAwDKxEDGZgPPwIDEWh21t8MAysRlb6HmT8EAxH8CA0d/kEPzJ0ANwEHNwBVAAIDEQIwECIPAm+CJs9VAAIDES9lBWQPAgIiGAIABwwI+wNRVG9vIGxhcmdlIHdpdGhkcmF3YWwMAQBVAAIDEUneiKkPAm+CJs9VAGUEAAEDP/5DDIOYAjcANwB9AFQAIAAHDAT7A3lNdXN0IGJlIGNhbGxlZCBieSB0aGUgcHJvdG9jb2wBAz/+RNZEHwA3AQc3ABoOgi8AGg6ELwAaDoYvABoGiAAaDooCAQM//kbkss0CNwQ3Anf35wAn5wAn5wAnJ+cAMwQGBwwONQYABjYGAgYMAQIMAgAoHAIAKBwAAAIABwwINBUEAgQaCQIAGgkGAgYDADQoAAIMAQACAxGJ3Aw/NBQCBAIDESa0rRA0AAAMAwM0FAIEAgMRJrStEDQAAP5H86/lAjcCRwAHNwAMAQIrGIIAAgMRXjE5BS0agoIAAQM//kneiKkCNwJHAAc3AAwBAisYggACAxGd3U6yLRqCggABAz/+S60x7QI3Avf39wwBAgQDEaCfHoD+TuH48gI3AScHBwwBAAwDAAwDKxG0djwJPwQDEV534SX+VRDeKgA3ABcaCgCCVQArCAAoDAoA/lWfW28ANwAHVQAEAxGU2Q4m/lX4pRACNwI3Anf3JyfnACcn5wAzBAIHDAw1BgACNgYCAjMIAgcMDAYDBjYIAgwBAAIDEVX4pRA1CAIMAgAMAQACAxHb+BxsNAAAAQEC/lrbON4ENwA3AAsAHzAABwwE+wNxU3Rha2UgbmVlZHMgYSBwb3NpdGl2ZSB2YWx1ZVUAAgMRAjAQIg8Cb4ImzwsAVQAEAxFH86/l/luybDYANwEHBwwBAFUABAMRKhTnhv5eMTkFAjcCNwZHAEcABwdnBwcXBzcGRwBHAAcHZwcHFygeAAYAFBoCAAIpngQGAAIoHgYEABQaCAYCKawEBAgA/l534SUCNwM3Anf35wAn5wHnADMEBAcMBjUEBAwBAigcAgAoHAAAAgAPAQI2BQQEBgMAAQEC/mLgpzACNwL39/cMAQIMAQAEAxHpijNd/mh21t8CNwI3Anf3J+cAJ+cBMwQCBwwINgQCDAEAAgMRaHbW3zUEAigcAgAoHAAAAgA5AAABAwP+bhptSQQ3A0cARwAXRwILACIgiAcMBPsDtUEgbmV3IHZhbGlkYXRvciBtdXN0IHN0YWtlIHRoZSBtaW5pbXVtIGFtb3VudBoKBoQvGAYAJgAHDAj7A1FPd25lciBtdXN0IGJlIHVuaXF1ZRoKDIYvGAwCJgAHDAz7A11TaWduIGtleSBtdXN0IGJlIHVuaXF1ZQwBAgwBAF4AgAAMAwAMAzcDRwJHAEcADAOPb4IE7fkFKkYDoASVuQfQCjs+7/1O/Qk/yfJ+FSzeo4VWh2A2swoYWQOpwLkE/LkDQ/4EU+bbAjcANwAaCgCCdggAVQAgAAcMBPsDiU9ubHkgbWFpbiBzdGFraW5nIGNvbnRyYWN0IGFsbG93ZWQBAz/+Bh2ngwI3AYcCNwA3AecAFwg9AAIEAQN/AQP//g5WEFQANwAXDAKIBAMRBh2ng/4tEMm9BDcANwALAB8wAAcMBPsDYURlcG9zaXQgbXVzdCBiZSBwb3NpdGl2ZQIDEVfkwn0PAm+CJs8LAAwCggMA/BEtEMm9NwA3AAD+OVNRJQA3AUcCNwACAxFX5MJ9DwJvgibPDAEARP6IIwACAgIBAz/+QQ/MnQA3AQc3AAIDEVfkwn0PAm+CJs8MAQAMAwAMAoIDAPwRQQ/MnTcBBzcADwJvgibPVQBlBAABAz/+RNZEHwA3A0cCRwBHADcAGg6Ir4IAAQA/GgaCABoGhAIaBoYEAQM//lUQ3ioANwAXDAMADAKCAwD8EVUQ3io3ABcA/lWfW28ANwAHDAMADAKCAwD8EVWfW283AAcA/lfkwn0CNwA3AFUAICCEBwwE+wNtT25seSBjb250cmFjdCBvd25lciBhbGxvd2VkAQM//lrbON4ENwA3AAsAHzAABwwE+wNZU3Rha2UgbXVzdCBiZSBwb3NpdGl2ZQIDEVfkwn0PAm+CJs8LAAwCggMA/BFa2zjeNwA3AAD+W7JsNgA3AQcHDAEADAMADAKCAwD8EVuybDY3AQcHAP5wTHmOADcABwwDAAwCggMA/BFwTHmONwAHAP6bORmmADcDBwcXNwACAxEEU+bbDwJvgibPGgoCiAg+iAYIDwJvgibPAQM/DAOvggABAD8GAwQMAQQMAQIMAQAMA2+CTeAMAwBGOAIAojD8EVpWi8Q3AwcHFzcA/wYDBP6b//MGADcBFzcAAgMRV+TCfQ8Cb4ImzwwBAAwDAAwCggMA/BGb//MGNwEXNwAA/p/uQCMANwEHNwACAxFX5MJ9DwJvgibPDAEADAMADAKCAwD8EZ/uQCM3AQc3AAD+wcUWrgA3AAcMAwAMAoIDAPwRwcUWrjcABwD+1ElUMgA3AAcMAwAMAoIDAPwR1ElUMjcABwD+6cifUAA3AAcMAwAMAoIDAPwR6cifUDcABwC5AbAvExEEU+bbsS5TdGFraW5nVmFsaWRhdG9yLmFzc2VydF9tYWluX3N0YWtpbmdfY2FsbGVyEQYdp4M9Lk9wdGlvbi5pc19zb21lEQ5WEFRNaGFzX3Jld2FyZF9jYWxsYmFjaxEtEMm9HWRlcG9zaXQROVNRJWFyZWdpc3Rlcl9yZXdhcmRfY2FsbGJhY2sRQQ/MnSF3aXRoZHJhdxFE1kQfEWluaXQRVRDeKi1nZXRfcmVzdGFrZRFVn1tvRWdldF9jdXJyZW50X3N0YWtlEVfkwn2VLlN0YWtpbmdWYWxpZGF0b3IuYXNzZXJ0X293bmVyX2NhbGxlchFa2zjeFXN0YWtlEVuybDZFZ2V0X3N0YWtlZF9hbW91bnQRcEx5jkVnZXRfY3VycmVudF9lcG9jaBGbORmmHXJld2FyZHMRm//zBi1zZXRfcmVzdGFrZRGf7kAjMWFkanVzdF9zdGFrZRHBxRauXWdldF92YWxpZGF0b3JfbWluX3N0YWtlEdRJVDJVZ2V0X2F2YWlsYWJsZV9iYWxhbmNlEenIn1BFZ2V0X3RvdGFsX2JhbGFuY2WCLwCFOC4wLjABowAPAhJ2ChQSDAEADAECDAMADAMADAMvAAwBBCcMDC0qgoIULZqEhAAULZqGhgIUCwAMAhQCAxFH86/lDwJvgibPAQIS/nBMeY4ANwAHAQKK/ni0ufYCNwJHAAc3AAwBAisYggACAxHPrcYGLRqCggABAz/+e7zT6QI3AjcCRwAHNwJHAAcXKB4AAAAoHgICACgeBAACKB4GAgIgKAIGBwwEHygCBgAeKAAEAP59g3MJAjcBRwA3BkcARwAHB2cHBxcMAQACAxG4MakrDwJvgibPGgoEhCsYBAArCIIA/oA6GiACNwP39/f3FBQCBAD+iWZ0hQA3AUcABwwBAAIDEX2DcwkPAgAoLAYAAP6J3Aw/AjcCNwJ39yfnACcn5wAzBAIHDA41BgACNgYCAjMIAgcMDgYDBjUKBAI2CgYCDAIADAIEKBwCACgcAAACAAcMDAwCBjQ4AAMMAgQMAQAEAxFG5LLNDAIGNDgAAwwCBAwBAAQDEe7gCYg0NAIDAP6N7MvfAjcCNwJ39ycn5wAn5wAzBAIHDAY1BgACNgYCAjMIAgcMCgYDBgwBAgwBAAIDEVX4pRAPAQIGAwABAgD+kEO0nQI3A0cANwZHAEcABwdnBwcXBzcAKB4KCAIuGgwKBCmcCAIMLRqCggABAz/+kS+4YAA3AUcARwIMAQACAxG4MakrDwJvgibPGgoChCsYAgCAAAD+lNkOJgA3AUcABxoKAIIrGgIAACgsBgIA/pW+h5kCNwP39/f3DAEEDAECBAMRe7zT6f6b//MGADcBFzcAVQACAxECMBAiDwJvgibPVQIMKyoOggwpbAoOAC0qgoIMAQM//p3dTrICNwI3BkcARwAHB2cHBxcHNwZHAEcABwdnBwcXKBwEABUSAAIoHAYADAIAAgMR1E28KA8CBCmeBgYABCmsBAYAAP6fFwXlAjcBNwZHAEcABwdnBwcXBygcCAAyAAwDKxFLrTHtPwIDEaACi3gEAxFO4fjy/p/uQCMANwEHNwBVAAIDEQIwECIPAm+CJs8MAQBVAAQDEXi0ufb+oAKLeAI3AjcCd/cn5wAn5wEzBAIHDAg2BAIMAQACAxGgAot4NQQCKBwCACgcAAACADQAAAEDA/6gnx6AAjcBNwLnAOcB5wEoHAIAAP6hPE2IAjcC9/f3DAECBAMRoJ8egP6uaF5cAjcC9/f3DAMDDAEAKBwCAigcAAICAxGQQ7SdNAAA/rRZv30ANwEHJzcCRwAHAgMRQwyDmA8Cb4ImzxoKAoIyCAIMAxEeha1SDAEAJwwEAgMRaHbW3w8Cb4ImzwQDETiOBG/+tHY8CQI3A/f39/cfFAIEBwwEAQEEAQEC/rgxqSsCNwFHADcALxiEAAcMBPsDgU5vdCBhIHJlZ2lzdGVyZWQgdmFsaWRhdG9yIG93bmVyAQM//sHFFq4ANwAHAQKI/sJSficCNwFHADcALxiGAAcMBPsDZU5vdCBhIHJlZ2lzdGVyZWQgc2lnbiBrZXkBAz/+w/IvPAI3AifnADcCd/c3ADMEAAcMBjUEACgcAgIoHAACAgAPAm+CJs82BQAABgMAAQM//s+txgYCNwI3BkcARwAHB2cHBxcHNwZHAEcABwdnBwcXKBwGABQQAigcBAAiAAcMBPsDPVRvbyBsYXJnZSBzdGFrZSgcBgAUEAIhDAAHDAj7Az1Ub28gc21hbGwgc3Rha2UoHggGABQaCggCKZwGAAoA/tRJVDIANwAHVQAEAxEvZQVk/tRNvCgCNwIHBwceFAACBwwEAQECAQEA/tmmqd4ENwIHJzcCRwAHNwACAxFDDIOYDwJvgibPDAECDAMrEaE8TYg/AgMRoAKLeAwDAAwDKxGAOhogPwIDEV534SUPAgILACAIAgcMCvsDcUluY29ycmVjdCB0b3RhbCByZXdhcmQgZ2l2ZW4MAxFi4KcwDAEAJwwEDAECAgMRw/IvPA8Cb4ImzxoKCoIyCAoMAxGuaF5cDAEAJwwEAgMRaHbW3w8Cb4ImzxQ2igAEAQM//tnn19UANwFHADcGRwBHAAcHZwcHFwwBAAQDEX2Dcwn+2/gcbAI3AzcCd/cn5wAn5wAn5wAzBAIHDBg1BgACNgYCAjMEBAcMEgYDBjUGBAQ2BgYEDAIEDAIAKBwCACgcAAACAAcMDgwCBjQoAAIMAQACAxHb+BxsNAgEADQoBAYMAgIMAQACAxHb+BxsNAgAADMEBAcMFvsDTUluY29tcGxldGUgcGF0dGVybnMBAQIBAQT+3EVBIQA3AUcABxoKAIIrGAAAKAwEAP7pijNdAjcCBzcCRwAHNwAoHgAAAigeAgICDAIAAgMRwlJ+Jw8Cb4ImzxoKBoYrKggGABoKCoIrKAoIKA4MCgcODAwMAgIMAggCAxEaMeRsBgMIDwJvgibPDAIMDAICDAEADAMAgAgIAwD8EZs5GaY3AwcHFzcAAAwCAgwCCAIDEUfzr+UGAwj+6cifUAA3AAdVAAQDEdxFQSH+6uxcNQI3AjcGRwBHAAcHZwcHFwc3BkcARwAHB2cHBxcoHgAEABQaAgACKZwEAAIA/u7gCYgCNwQ3Anf35wAn5wAn5wAnJ+cAMwQGBwwMNQYABjYGAgYMAgAMAQIoHAIAKBwAAAIABwwINBUEAgQaCQIAGgkGAgYDADQoAAIMAQACAxGJ3Aw/NBQCBDQAADQUAgQ0MAMA/vwIDR0CNwI3Anf3J+cAJ+cAMwQCBwwIBgMEDAECDAEAAgMRidwMPwwBAAQDEY3sy98BAwO5BrQvQxEBnLq0YS5NYWluU3Rha2luZy5sb2NrX3N0YWtlXxECMBAidS5NYWluU3Rha2luZy5hc3NlcnRfdmFsaWRhdG9yEQMZmA+NLnNvcnRlZF92YWxpZGF0b3JzLiVsYW1iZGEuMTI1LjE0LjARE3fYBjkuTGlzdC5yZXZlcnNlXxEaMeRsVS5NYWluU3Rha2luZy5kZXBvc2l0XxEeha1SbS5sb2NrX3N0YWtlLiVsYW1iZGEuMTIxLjUuMBEmtK0QNS5MaXN0LnJldmVyc2URKhTnhn0uTWFpblN0YWtpbmcuZ2V0X3N0YWtlZF9hbW91bnRfES0Qyb0dZGVwb3NpdBEvZQVkWWdldF9hdmFpbGFibGVfYmFsYW5jZV8ROI4Eb0Vzb3J0ZWRfdmFsaWRhdG9ycxFBD8ydIXdpdGhkcmF3EUMMg5iFLk1haW5TdGFraW5nLmFzc2VydF9wcm90b2NvbF9jYWxsEUTWRB8RaW5pdBFG5LLNJS5MaXN0LmFzYxFH86/lTS5NYWluU3Rha2luZy5zdGFrZV8RSd6IqVkuTWFpblN0YWtpbmcud2l0aGRyYXdfEUutMe2pLk1haW5TdGFraW5nLmxvY2tlZF9zdGFrZS4lbGFtYmRhLjIwOC4yNy4wEU7h+PJBLk1haW5TdGFraW5nLm1heBFVEN4qLWdldF9yZXN0YWtlEVWfW29FZ2V0X2N1cnJlbnRfc3Rha2URVfilEEUuTGlzdC5tZXJnZV9wYWlycxFa2zjeFXN0YWtlEVuybDZFZ2V0X3N0YWtlZF9hbW91bnQRXjE5BVEuTWFpblN0YWtpbmcuc3Rha2VfdhFed+ElLS5MaXN0LmZvbGRsEWLgpzB1LmFkZF9yZXdhcmRzLiVsYW1iZGEuMTEzLjMxLjIRaHbW31kuTGlzdEludGVybmFsLmZsYXRfbWFwEW4abUk1bmV3X3ZhbGlkYXRvchFwTHmORWdldF9jdXJyZW50X2Vwb2NoEXi0ufZpLk1haW5TdGFraW5nLmFkanVzdF9zdGFrZV8Re7zT6WkuTWFpblN0YWtpbmcuY21wX3ZhbGlkYXRvchF9g3MJdS5NYWluU3Rha2luZy5sb29rdXBfdmFsaWRhdG9yEYA6GiB1LmFkZF9yZXdhcmRzLiVsYW1iZGEuMTExLjM3LjARiWZ0hTVzdGFraW5nX3Bvd2VyEYncDD9RLkxpc3QubW9ub3RvbmljX3N1YnMRjezL3z0uTGlzdC5tZXJnZV9hbGwRkEO0nWkuTWFpblN0YWtpbmcudW5sb2NrX3N0YWtlXxGRL7hgWWdldF92YWxpZGF0b3JfY29udHJhY3QRlNkOJklnZXRfY3VycmVudF9zdGFrZV8Rlb6HmY0uc29ydGVkX3ZhbGlkYXRvcnMuJWxhbWJkYS4xMjguMTUuMRGb//MGLXNldF9yZXN0YWtlEZ3dTrJdLk1haW5TdGFraW5nLndpdGhkcmF3X3YRnxcF5WUuTWFpblN0YWtpbmcubG9ja2VkX3N0YWtlEZ/uQCMxYWRqdXN0X3N0YWtlEaACi3glLkxpc3QubWFwEaCfHoAlLlBhaXIuc25kEaE8TYh1LmFkZF9yZXdhcmRzLiVsYW1iZGEuMTExLjUzLjERrmheXHEuYWRkX3Jld2FyZHMuJWxhbWJkYS4xMTQuNS4zEbRZv30pbG9ja19zdGFrZRG0djwJhS5NYWluU3Rha2luZy5tYXguJWxhbWJkYS4yMTIuMjMuMBG4MakrZS5NYWluU3Rha2luZy5hc3NlcnRfb3duZXIRwcUWrl1nZXRfdmFsaWRhdG9yX21pbl9zdGFrZRHCUn4naS5NYWluU3Rha2luZy5hc3NlcnRfc2lnbmVyEcPyLzw1Lkxpc3QuZm9yZWFjaBHPrcYGbS5NYWluU3Rha2luZy5hZGp1c3Rfc3Rha2VfdhHUSVQyVWdldF9hdmFpbGFibGVfYmFsYW5jZRHUTbwoQS5NYWluU3Rha2luZy5taW4R2aap3i1hZGRfcmV3YXJkcxHZ59fVTWdldF92YWxpZGF0b3Jfc3RhdGUR2/gcbC0uTGlzdC5tZXJnZRHcRUEhSWdldF90b3RhbF9iYWxhbmNlXxHpijNdXS5NYWluU3Rha2luZy5hZGRfcmV3YXJkEenIn1BFZ2V0X3RvdGFsX2JhbGFuY2UR6uxcNVkuTWFpblN0YWtpbmcuZGVwb3NpdF92Ee7gCYgpLkxpc3QuZGVzYxH8CA0dKS5MaXN0LnNvcnSCLwCFOC4wLjAAKhAPDw==",
      "owner_pubkey": "ak_11111111111111111111111111111115rHyByZ",
      "pubkey": "ct_KJgjAXMtRF68AbT5A2aC9fTk8PA4WFv26cFSY27fXs6FtYQHK"
    },
    {
      "abi_version": 3,
      "vm_version": 8,
      "amount": 0,
      "nonce": 2,
      "call_data": "cb_KxFE1kQfG58CoCmQRGuzLDSzySG/5UcFOdoQ1iYy6a6fhJXVahjufrESSdmdMQ==",
      "code": "cb_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cnNlXxEUyJdCIXN0ZXBfZW9lERsUbq0pYWRkX3Jld2FyZBEbzmkrpS5IQ0VsZWN0aW9uLnBheV9yZXdhcmRzXy4lbGFtYmRhLjIwOS40MS4wESEKpcSRLnZhbGlkYXRvcl9zY2hlZHVsZS4lbGFtYmRhLjE2MC41MS4yESa0rRA1Lkxpc3QucmV2ZXJzZREnB+ufEXN0ZXARKSVv45EudmFsaWRhdG9yX3NjaGVkdWxlLiVsYW1iZGEuMTU4LjM1LjARLg3Q6SFwaW5faW5mbxE1Lvi0XS5IQ0VsZWN0aW9uLnBheV9yZXdhcmRfET+XLpplLkhDRWxlY3Rpb24ubWtfZXBvY2hfaW5mbxFE1kQfEWluaXQRR+DZiy1pbml0X2Vwb2NocxFYCet8YS5IQ0VsZWN0aW9uLnBheV9yZXdhcmRzXxFed+ElLS5MaXN0LmZvbGRsEWPUmhR9LkhDRWxlY3Rpb24udmFsaWRhdG9yX3NjaGVkdWxlXxF89ZduMWVwb2NoX2xlbmd0aBGB0IEkNWZpbmFsaXplX2luZm8RhW49SUFlcG9jaF9pbmZvX2Vwb2NoEY/NcLRdLkhDRWxlY3Rpb24ucGF5X3Jld2FyZHMRoAKLeCUuTGlzdC5tYXARoJ8egCUuUGFpci5zbmQRpDzMVBlsZWFkZXIRs1rVEhVlcG9jaBG5f0d3aS5IQ0VsZWN0aW9uLnBpY2tfdmFsaWRhdG9yEboRANE5ZmluYWxpemVfZXBvY2gRx+q8gg1waW4R1Kf3tG0uSENFbGVjdGlvbi5jYWxjX2NhcnJ5X292ZXIR2Nxw9z1waW5fcmV3YXJkX2luZm8R59HQwIEuSENFbGVjdGlvbi5hc3NlcnRfcHJvdG9jb2xfY2FsbBHt2SFlQXN0YWtpbmdfY29udHJhY3QR7e5395EudmFsaWRhdG9yX3NjaGVkdWxlLiVsYW1iZGEuMTU4LjUxLjER8Y74a0l2YWxpZGF0b3Jfc2NoZWR1bGUR/YOpRyllcG9jaF9pbmZvgi8AhTguMC4wALiPi/0=",
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

To have an initiative to pin an epoch state to parent chain, pinners should be rewarded in whatever the native currency is for the Hyperchain.
The reward that will be handed out for doing a verifiable pinning for an epoch. This should be adjusted to reflect the economics of the Hyperchain.

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


