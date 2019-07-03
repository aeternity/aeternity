AESTRATUM application (alpha)
==========

Introduction
==========

By setting of the configuration value `stratum -> enabled` to `true`, the node also starts the aestratum app.

Aestratum allows users to form mining pools where they act as a pool operator, specifying the percentage of the reward they take from mining reward for providing the pool to individual miners.
An Aestratum client [https://github.com/aeternity/aestratum_client] connects to operator pool node (Stratum server), and solves cryptographic puzzles.
The server sends jobs to the clients, they try to solve them and send their results (called shares) back. The server keeps targets for each client in line with its computational power and distributes rewards.
When miner finds the result artefacts of the cuckoo cycle algorithm - nonce and proof of work, the miner is awarded a share in a mining epoch happening between two subsequent key blocks of the chain.
A share is understood as a smallest unit of eligible right to be paid out if the share was created in the mining epoch to be rewarded.
If the difficulty of the solution matches the difficulty of the main network, we can combine the block candidate with the computed nonce and proof of work, construct a next block and publish it.

If the network accepts our new block, the node which constructed that next block will receive a mining reward. This reward will be used for paying out of the participants which submitted shares during mining epochs.
When straum app notices that the latest keyblock changed, stratum looks back 180 epochs behind top keyblock and locates two (configurable) subsequent mining epochs.
Miners which contributed their shares to these mining epochs will be paid out proportionally to their contribution, taking also miner difficulty into account.
The rewarding scheme is called PPLNS - Pay Per Last N Shares.


Configuration
==========

Example configuration section in your aeternity.yaml configuration file could look like follows:

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

Do not forget to put your accounts in `reward -> beneficiaries`.


CONNECTION section
==========

`host` - (optional) - used only for identification of the pool (default "localhost")

`port` - (optional) -  on what port should clients connect to (default 9999)

`max_connections` - (optional) - max number of connected mining clients (default 1024)

`num_acceptors` - (optional) - max number of active acceptor processes handling clients requests (default 100)

`transport` - (optional) - what transport of data to use (default "tcp", to be tested: "ssl")


SESSION section
==========

`extra_nonce_bytes` - (optional) - nonce (a number) has 8 bytes, and is composed of two parts - extra nonce and miner nonce.
Extra nonce is a randomly chosen (unique) number by the stratum server. When a client connects to stratum, it receives an extra nonce and tries to find a miner nonce.
There is an interdependence among `max_connections` and `extra_nonce_bytes`: `max_connections` must be at most (2^(extra_nonce_bytes * 8)) / 2.
For example - if you set `extra_nonce_bytes` to 1, `max_connections` shouldn't exceed 128.
(1 byte can represent 256 numbers, and since we need to choose from them, we divide that range by 2).
(default 4)

`skip_num_blocks` - (optional) - a new keyblock candidate is generated roughly every 3 seconds. `skip_num_blocks` determines how many of such candidates are skipped before a new job is sent to miner.
(default 10)

`initial_share_target` - when a miner connects, we don't know his mining power yet. For this reason, we give this new miner a job with the lowest difficulty (highest target).

`max_share_target` - maximal target (or, minimal difficulty) of the shares this pool accepts.
If you lower the target, you raise the minimal difficulty which puts a strain on miners with weaker hardware.

`desired_solve_time` - (optional) - is a time (in seconds) the server expects a miner to send a mined share.
If this time is too short for a miner to produce a result, the server adjusts difficulty of next job sent to the miner.
(default 30)

`max_solve_time` - (optional) - specifies a time limit (in seconds) stratum app understands as time needed for producing of a share, even when miner didn't submit a share at all.
(default 60)

`share_target_diff_threshold` - (optional) - stratum app measures how long did it took for a miner to produce a share, or, uses `max_solve_time` in case no share was submitted.
For the following jobs being sent to the miner, a recalculation of the difficulty is in order. If the difference between new diffuculty and previous difficulty is larger than `share_target_diff_threshold` (in percents), new difficulty is used for next jobs sent to miner, via sending `set_target` notification to miner.
If the difference is smaller than the threshold, previous difficulty is used for the following jobs.
(default 5.0)

`edge_bits` - (optional) - a proof of work algorithm we use is called Cuckoo Cycle - looks for a cycle in randomly generated graph. `edge_bits` determines how long this cycle should be.
You should only change this value if you plan to deploy your own network, as for this change to work, you will need to distribute a new mining client.
(default 29)

`max_jobs` - (optional) - max number of jobs kept in queue for each client. Lower number of `max_jobs` results in more frequent changes via `set_target` notification.
(default 20)

`max_workers` - (optional) - max number of workers for one connection. This is not fully implemented yet, will be required for a proxy implementation.
(default 20)

`msg_timeout` - (optional) - timeout (in milliseconds) guarding transitions between miner states (connected, configured, subscribed). If a transition doesn't happen in time, the client session is closed.
(default 15)


REWARD section
==========

`reward_last_rounds` - (optional) - how many epochs between keyblocks we reward from a received mining reward. Higher number of epochs would cause that the mining reward will be distributed to more shares, but each partial reward will be smaller. Higher number supports loayality of the miners, by raising a chance of rewarding miners who contributed shares for a longer period of time.
(default 2)

`beneficiaries` - a list in "account_address:percent_share" format - denoting the pool operator addresses with their respective shares. The sum of shares must be less than or equal to 100.
The sum of shares equal to 100 effectively means that all rewards coming from mining are distributed to pool operators, while miners don't receive any reward.

`keys -> dir` - points to a location where to store the stratum account keypair. Since we want to send tokens to miners from this account, we need to have access to its private key.
The stratum account holds reward(s) from mining which are scheduled to be payed out via a payment contract. While it may sound tempting, stratum account should never have any significant accumulated wealth since it only serves as a intermittent place for receiving of reward, out of which are then the funds distrubuted to miners soon.
If `dir` represents a relative path, the directory will be in `priv` directory of aestratum app. Providing absolute path allows storing of keypair outside of the aestratum app.
