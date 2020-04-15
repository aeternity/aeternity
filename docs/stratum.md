Introduction
==========

Please note that this application is in **alpha** state.

By setting the configuration value `stratum > enabled` to `true`, the node starts Aestratum app.
Aestratum app is an implementation of server side part of the stratum protocol suited for Aeternity network [https://github.com/aeternity/protocol/blob/master/STRATUM.md].
The purpose of the protocol is to formalize the coordination of information exchange between pool server and pool clients.

Aestratum app is part of the Aeternity node, but in the future we may move this functionality to a separate executable.
Enabling stratum server causes that the node doesn't mine the solutions for proof of work locally, but allows user to form mining pools and act as a pool operator.
For providing the pool to individual miners, a pool operator can specifying the percentage of the reward they withhold from mined block rewards.

An Aestratum client [https://github.com/aeternity/aestratum_client] connects to operator pool node (Stratum server), and solves cryptographic puzzles.
The server sends jobs to clients, they try to solve them and send their results (called shares) back. The server keeps targets for each client in line with its computational power and distributes rewards accordingly.
When miner finds the result artefacts of the Cuckoo Cycle algorithm - nonce and proof of work, the miner is awarded a share in a mining epoch happening between two subsequent key blocks of the chain.
A share can be understood as a smallest unit of eligible right to be paid out if the share was created in the mining epoch to be rewarded.
If the difficulty of the solution matches the difficulty of the main network, we can combine the block candidate with the computed nonce and proof of work, construct a next block and publish it.

If the network accepts our new block, the node which constructed that next block will receive a mining reward. This reward will be used for paying out the participants which submitted shares during mining epochs.
When stratum app notices that the latest keyblock has changed, stratum looks back 180 (`BENEFICIARY_REWARD_DELAY`) epochs behind the top keyblock and locates two (configurable) subsequent mining epochs.
Miners who contributed their shares to these mining epochs will be paid out proportionally to their contribution, with miner difficulty also taken into account.
The rewarding scheme is called PPLNS - Pay Per Last N Shares [https://virtopia.ca/what-is-pplns/].

The payout of the rewards to pool operators and miners is executed by payout contract deployed to the main net at [https://explorer.aepps.com/#/tx/th_2raHdPQ8xtbE6oKh3z1pFmUpyFC5H7ZTBkNB8TuVydJjwedduL].
Submitting of contract call transaction to the operator node invokes the contract functionality and changes the balances of the rewarded participants.
The contract's source code can be seen here: [https://github.com/aeternity/aeternity/blob/master/apps/aestratum/priv/Payout.aes]

The frequency of payments depends on how often the clients of the pool find a share with the same difficulty as the main net. Ideally, miners with CUDA hardware are mining for the pool, as that significantly increases a chance to find a solution with acceptable difficulty.


Several improvements are planned for Aestratum, also depending on the feedback from community:
- proxy for miners (for a miner with multiple mining devices)
- separation of Aestratum from Aeternity Node
- simple web based gui for better insight


Configuration
==========

Example configuration section in your aeternity.yaml configuration file could look as follows:

```
stratum:
    enabled: true
    connection:
        port: 9999
        max_connections: 1024
        num_acceptors: 100
    session:
        extra_nonce_bytes: 4
        initial_share_target: 115790322390251417039241401711187164934754157181743688420499462401711837019160
        max_share_target: 115790322390251417039241401711187164934754157181743688420499462401711837020160
        desired_solve_time: 30
        max_solve_time: 60
        share_target_diff_threshold: 5.0
        max_jobs: 20
        msg_timeout: 15
    reward:
        reward_last_rounds: 2  # how many mining epochs to reward
        beneficiaries: ["ak_2hJJGh3eJA2v9yLz73To7P8LvoHdz3arku3WXvgbCfwQyaL4nK:3.3",
                        "ak_241xf1kQiexbSvWKfn5uve7ugGASjME93zDbr6SGQzYSCMTeQS:2.2"]
        keys:
            dir: stratum_keys
```

The `mining > autostart` must be set to `true`.

Since mining locally and enabled stratum are mutually exclusive, the `mining > beneficiary` is not used when stratum is enabled.
Instead, the `stratum > reward > beneficiaries` will receive the rewards from operating of the pool.

When stratum is started for the first time and no keypair is found in the configured location (`stratum > reward > keys > dir`),
the keypair is automatically generated and placed in the configured location for next time.

It is advised to backup this keypair, as it may hold some tokens received in the last 180 keyblocks.


Do not forget to put your accounts in `stratum > reward > beneficiaries`.


Connection
==========

`host` - (optional) - used only for identification of the pool (default "localhost")

`port` - (optional) - denotes what port should clients connect to (default 9999)

`max_connections` - (optional) - max number of connected mining clients (default 1024)

`num_acceptors` - (optional) - max number of active acceptor processes handling clients requests (default 100)

`transport` - (optional) - what mode of transport for data to use (default "tcp", to be tested: "ssl")


Session
==========

`extra_nonce_bytes` - (optional) - nonce (a number) has 8 bytes, and is composed of two parts - extra nonce and miner nonce.
Extra nonce is a randomly chosen (unique) number by the stratum server. When a client connects to stratum, it receives an extra nonce and tries to find a miner nonce.
There is an interdependence among `max_connections` and `extra_nonce_bytes`: `max_connections` must be at most (2^(extra_nonce_bytes * 8)) / 2.
For example - if you set `extra_nonce_bytes` to 1, `max_connections` shouldn't exceed 128.
(1 byte can represent 256 numbers, and since we need to choose from them, we divide that range by 2).
(default 4)

`skip_num_blocks` - (optional) - a new keyblock candidate is generated roughly every 3 seconds. `skip_num_blocks` determines how many of these candidates are skipped before a new job is sent to miner.
(default 10)

`initial_share_target` - when a miner connects, we cannot immediately determine his mining power. For this reason, we give this new miner a job with the lowest difficulty (highest target).

`max_share_target` - maximal target (or, minimal difficulty) of the shares a pool accepts.
If you lower the target, you raise the minimal difficulty which puts a strain on miners with weaker hardware.

`desired_solve_time` - (optional) - is the amount of time (in seconds) the server expects a miner to send a mined share.
If this time is too short for a miner to produce a result, the server adjusts difficulty of the next job sent to the miner.
(default 30)

`max_solve_time` - (optional) - specifies a time limit (in seconds) stratum app understands as time needed for producing a share, even when miner didn't submit a share at all.
(default 60)

`share_target_diff_threshold` - (optional) - stratum app measures how long it took a miner to produce a share, or, uses `max_solve_time` in case no share was submitted.
For the succeeding jobs being sent to the miner, a recalculation of the difficulty is in order. If the difference between the new difficulty and previous difficulty is larger than `share_target_diff_threshold` (in percentages), the new difficulty is used for succeeding jobs sent to miner. This is done by sending `set_target` notification to the miner.
If the difference is smaller than the threshold, the previous difficulty is used for the following jobs.
(default 5.0)

`edge_bits` - (optional) - a proof of work algorithm we use, called Cuckoo Cycle, looks for a cycle in randomly generated graph. `edge_bits` determines how long this cycle should be.
You should only change this value if you plan to deploy your own network, because in order for this change to work, you will need to distribute a new mining client.
(default 29)

`max_jobs` - (optional) - max number of jobs kept in queue for each client. Lower number of `max_jobs` results in more frequent changes via `set_target` notification.
(default 20)

`max_workers` - (optional) - max number of workers for one connection. This is not fully implemented yet, will be required for a proxy implementation.
(default 20)

`msg_timeout` - (optional) - timeout (in milliseconds) guards transitions between miner states (connected, configured, subscribed). If a transition doesn't happen within a specified time, the client session is closed.
(default 15)


Reward
==========

`reward_last_rounds` - (optional) - denoted how many epochs between keyblocks are rewarded out of the received mining reward. Higher number of epochs would cause that the mining reward will be distributed to more shares, but each partial reward will be smaller. Higher number incentivizes loyalty among miners, by raising a chance of rewarding miners who contributed shares for a longer period of time.
(default 2)

`beneficiaries` - is a list in format "account_address:percent_share" - denotes the pool operator addresses with their respective shares. The sum of shares must be less than or equal to 100.
The sum of shares equal to 100 effectively means that all rewards coming from mining are distributed to pool operators, while miners don't receive any reward.

`keys > dir` - points to a location where to a stratum account keypair is stored. Since we want to send tokens to miners from this account, we need to have access to its private key.
The stratum account holds reward(s) from mining which are scheduled to be paid out via a payment contract. While it may sound tempting, stratum account should never have any significant accumulated wealth since it only serves as a temporary place for holding rewards received, before they are distributed to the miners.
This is happening regularly and funds are not accumulated in this account for longer periods of time.
If `dir` represents a relative path, the directory will be in `priv` directory of estratum app. Providing an absolute path allows storing a keypair outside the aestratum app.
