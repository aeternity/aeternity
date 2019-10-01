# Token migration contract

In the Lima hard-fork the remaining (frozen) Aeternity tokens from the Ethereum
ERC20 contract are put into a contract and can be retreived by the entity
holding the correct Ethereum private key. This contract is added to chain as
part of the hard-fork, and the contract is also funded as part of the
hard-fork. Here we describe how we prepare the data (the `contracts.json` file)
that is used during the hard-fork. The `.json`-file has the following format
and constraints (`apps/aecore/src/aec_fork_block_settings.erl`):

```
%%% { <API encoded pubkey for contract> : { "amount" : <integer>,
%%%                                         "vm_version" : <integer>,
%%%                                         "abi_version" : <integer>,
%%%                                         "nonce" : <integer>
%%%                                         "code" : <API encoded contract byte array>
%%%                                         "call_data" : <API encoded contract byte array>
%%%                                       }
%%% ...
%%% }
%%%
%%% The locked token account will be the owner of the contracts.
%%% The nonces must correspond to the nonces of the owner account.
%%% The nonces must be consecutive (but not necessarity ordered)
%%% The pubkey of the contract must correspond to the computed pubkey
%%%    (based on owner account and nonce). This is mostly a fail safe to ensure
%%%    that the contract pubkey is visible in the file.
```

Since this is the first contract to be added by this mechanism, the nonce to be
used is `1` and we can compute the contract pubkey using Erlang:

```
25> Locked = aec_governance:locked_coins_holder_account(), Nonce = 1.
1
26> CtPK = aect_contracts:compute_contract_pubkey(Locked, Nonce).
<<84,180,196,235,185,254,235,68,37,168,101,128,127,111,97,
  136,141,11,134,251,228,200,73,71,175,98,22,115,172,...>>
27> aeser_api_encoder:encode(contract_pubkey, CtPK).
<<"ct_eJhrbPPS4V97VLKEVbSCJFpdA4uyXiZujQyLqMFoYV88TzDe6">>
```

Then we need to compute the total sum of tokens. The final list of accounts
that has not migrated their tokens is in `test/json/contracts_accounts.json`.
The sum of the tokens is:

```
79> mtree:total_sum_from_json("../test/json/contracts_accounts.json").
29622067581238053773524138
```

We also need the root-hash of the Merkle tree (the tree contains all the
Ethereum accounts and their respective balances).

```
80> BigT = mtree:tree_from_json("../test/json/contracts_accounts.json"), ok.
ok
81> mtree:root_hash(BigT).
"E4DBC69BF2783B81B0423DA3F5B684C1D37CCFAE798474525C4001DB42C67669"
```

Finally we need to compile the contract [available
HERE](test/contracts/token_migration.aes), and the contract init call data:

```
aeternity : ls -l test/contracts/token_migration.aes
-rw-r--r--  1 hans  staff  3966 Sep 27 10:39 test/contracts/token_migration.aes
aeternity : md5 test/contracts/token_migration.aes
MD5 (test/contracts/token_migration.aes) = 56ead7528cb20700e148566e5683b5c0
aeternity : md5 ../aesophia_cli/priv/bin/v4.0.0-rc5/aesophia_cli
MD5 (../aesophia_cli/priv/bin/v4.0.0-rc5/aesophia_cli) = 39ae10aa26838dc99f5caae5f43d98ff
aeternity : ../aesophia_cli/priv/bin/v4.0.0-rc5/aesophia_cli test/contracts/token_migration.aes
Bytecode:
cb_+QP9RgOguo6qpyu/hfk5d+qq/tgbo35YmOc3e32WGzPH9WKYCUnAuQPLuQL0/gYNP1AANwUHRwAHJ3eXbwEHfAQCBwwgDAOBUGFzc2VkIEFFIGFkZHJlc3MgaXMgbm90IHBheWFibGX7ABoCb4ImzzwGAgI+CAI7ADogAjoOBGkZRXRoZXJldW0gU2lnbmVkIE1lc3NhZ2U6CgwBCBsIBAIDEeg39H8aAggIPggcHhoCCgwCCgIDEcnWapQmAAcMGgwDvVRoaXMgYWNjb3VudCBoYXMgYWxyZWFkeSB0cmFuc2ZlcnJlZCBpdHMgdG9rZW5z+wAaAm+CJs8MAQYMAQQMAQAMAgoCAxFdcNL1BwwYDAPFRnJvbSBwcm92aWRlZCBkYXRhLCBjYW5ub3QgYmUgZ2VuZXJhdGVkIHNhbWUgcm9vdPsAGgJvgibPDAEADAECZQAoLAKCEAwCgikMAhoCFCguFgQULegWCj8MAhQpDAQaAoIMAgoMAQIMAQBE/BMGAAYCAxFlpeAPGgJvgibPKCwCggAMAz8GAxQMAz8GAw4MA6FGYWlsZWQgdG8gcmVjb3ZlciBhZGRyZXNzLCBiYWQgc2lnbmF0dXJl+wBGOgoIAEAICjoMCTBYBgMIDAM/BgME/kTWRB8ANwJ3BzcADAEADAECKgAnDAYaAoIBAz/+XXDS9QA3BHcHByd3FzsEAjoMBTo6BgAAGwgAQAAMAQQMAQYCAxHsL+O+GgIEKCwAgiAIBAD+ZaXgDwI3AYcBNwN3RwAHNwBGNAAERjQAAgwDnwGBR4YJ0hFJn+CFQlrEPeWHXqVaT+74Y8OnTxa1W2+3OL1GNAAAYwABAz/+tIwWhAA3AAdTAAD+u6I84gA3AHcoLACCAP7J1mqUADcBdxcoLASCLxAAAP7YuSNvADcABygsAoIA/ug39H8CNwKXQJdvAYcCNwA3AZcofxQAAgD+7C/jvgI3Ayd3B3d3MwQABwwMNQYAADYGAgAYNAIEIDACBwwKOiQEAAYDBhoCBBsIBEABBBoJAAIXNQICBAYDAAA6GAAEBgMGAQEEuM8vChEGDT9QHW1pZ3JhdGURRNZEHxFpbml0EV1w0vVhY29udGFpbmVkX2luX21lcmtsZV90cmVlEWWl4A8tQ2hhaW4uZXZlbnQRtIwWhB1iYWxhbmNlEbuiPOIlcm9vdF9oYXNoEcnWapQtaXNfbWlncmF0ZWQR2Lkjb0FtaWdyYXRpb25zX2NvdW50Eeg39H9pLlRva2VuTWlncmF0aW9uLmdldF9zaWduZXIR7C/jvnkuVG9rZW5NaWdyYXRpb24uY2FsY3VsYXRlX3Jvb3SCLwCJNC4wLjAtcmM1AIUFMq4=
aeternity : ../aesophia_cli/priv/bin/v4.0.0-rc5/aesophia_cli --create_calldata test/contracts/token_migration.aes --calldata_fun init --calldata_args "\"E4DBC69BF2783B81B0423DA3F5B684C1D37CCFAE798474525C4001DB42C67669\", 0"
Calldata:
cb_KxFE1kQfKwEARTREQkM2OUJGMjc4M0I4MUIwNDIzREEzRjVCNjg0QzFEMzdDQ0ZBRTc5ODQ3NDUyNUM0MDAxREI0MkM2NzY2OQDtuA/R
```

The resulting data is then:
```
{
    "ct_eJhrbPPS4V97VLKEVbSCJFpdA4uyXiZujQyLqMFoYV88TzDe6" :
        { "amount"      : 29622067581238053773524138,
          "vm_version"  : 5,
          "abi_version" : 3,
          "nonce"       : 1,
          "code"        : "cb_+QP9RgOguo6qpyu/hfk5d+qq/tgbo35YmOc3e32WGzPH9WKYCUnAuQPLuQL0/gYNP1AANwUHRwAHJ3eXbwEHfAQCBwwgDAOBUGFzc2VkIEFFIGFkZHJlc3MgaXMgbm90IHBheWFibGX7ABoCb4ImzzwGAgI+CAI7ADogAjoOBGkZRXRoZXJldW0gU2lnbmVkIE1lc3NhZ2U6CgwBCBsIBAIDEeg39H8aAggIPggcHhoCCgwCCgIDEcnWapQmAAcMGgwDvVRoaXMgYWNjb3VudCBoYXMgYWxyZWFkeSB0cmFuc2ZlcnJlZCBpdHMgdG9rZW5z+wAaAm+CJs8MAQYMAQQMAQAMAgoCAxFdcNL1BwwYDAPFRnJvbSBwcm92aWRlZCBkYXRhLCBjYW5ub3QgYmUgZ2VuZXJhdGVkIHNhbWUgcm9vdPsAGgJvgibPDAEADAECZQAoLAKCEAwCgikMAhoCFCguFgQULegWCj8MAhQpDAQaAoIMAgoMAQIMAQBE/BMGAAYCAxFlpeAPGgJvgibPKCwCggAMAz8GAxQMAz8GAw4MA6FGYWlsZWQgdG8gcmVjb3ZlciBhZGRyZXNzLCBiYWQgc2lnbmF0dXJl+wBGOgoIAEAICjoMCTBYBgMIDAM/BgME/kTWRB8ANwJ3BzcADAEADAECKgAnDAYaAoIBAz/+XXDS9QA3BHcHByd3FzsEAjoMBTo6BgAAGwgAQAAMAQQMAQYCAxHsL+O+GgIEKCwAgiAIBAD+ZaXgDwI3AYcBNwN3RwAHNwBGNAAERjQAAgwDnwGBR4YJ0hFJn+CFQlrEPeWHXqVaT+74Y8OnTxa1W2+3OL1GNAAAYwABAz/+tIwWhAA3AAdTAAD+u6I84gA3AHcoLACCAP7J1mqUADcBdxcoLASCLxAAAP7YuSNvADcABygsAoIA/ug39H8CNwKXQJdvAYcCNwA3AZcofxQAAgD+7C/jvgI3Ayd3B3d3MwQABwwMNQYAADYGAgAYNAIEIDACBwwKOiQEAAYDBhoCBBsIBEABBBoJAAIXNQICBAYDAAA6GAAEBgMGAQEEuM8vChEGDT9QHW1pZ3JhdGURRNZEHxFpbml0EV1w0vVhY29udGFpbmVkX2luX21lcmtsZV90cmVlEWWl4A8tQ2hhaW4uZXZlbnQRtIwWhB1iYWxhbmNlEbuiPOIlcm9vdF9oYXNoEcnWapQtaXNfbWlncmF0ZWQR2Lkjb0FtaWdyYXRpb25zX2NvdW50Eeg39H9pLlRva2VuTWlncmF0aW9uLmdldF9zaWduZXIR7C/jvnkuVG9rZW5NaWdyYXRpb24uY2FsY3VsYXRlX3Jvb3SCLwCJNC4wLjAtcmM1AIUFMq4=",
          "call_data"   : "cb_KxFE1kQfKwEARTREQkM2OUJGMjc4M0I4MUIwNDIzREEzRjVCNjg0QzFEMzdDQ0ZBRTc5ODQ3NDUyNUM0MDAxREI0MkM2NzY2OQDtuA/R"
        }
}
```
