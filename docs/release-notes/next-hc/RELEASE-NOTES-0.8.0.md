# Hyperchains

This is an Internal pre-release of Hyperchains as described in [Periodically Syncing HyperChains](https://github.com/aeternity/hyperchains-whitepaper/blob/master/Periodically-Syncing-HyperChains.md).
See also [aeternity Hyperchains](https://aeternity.com/hyperchains).

Hyperchains integrate with other blockchain networks, leveraging their security and consensus mechanisms while providing scalable, energy-efficient hyperchains.

An aeternity Hyperchain comes with all the normal features of aeternity:
* FATE VM: Efficient virtual machine for executing smart contracts.
* Sophia Language: A functional programming language for secure smart contracts.
* State Channels: Off-chain scaling for private and secure transactions.
* AENS: Æternity Naming System for human-readable addresses.
* Oracles: Bridging real-world data into the blockchain ecosystem.

See [aeternity.com](aeternity.com) for more information.

# About this release

This preliminary implementation of Hyperchains lays the foundation for a scalable and energy-efficient blockchain system. Key features introduced in this release include:

* Possible to spin up hyperchain on local machine with æternity as pinning chain also running on local machine.
* Introduction of Epochs.
* Introduction of a Staking Cycle.
* Future leader election.
* Leader schedule per epoch.
* Pinning. Instead of posting every block on parent chain, pin at most once per epoch.
* Fix block synchronization.
* Change from Bitcoin.NG to 1 microblock per generation.
* Change the order of keyblock and microblocks.
* Introduce block flags for placeholder for late blocks.
* Introduce a real notion of time.
* Hyperchains configuration including contracts and configurable coinbase and timings.


## Core Features
1. **Staking Mechanism**
   - Enables participants to stake tokens and qualify for block production and validation. The staking framework ensures fairness and rewards based on contributions and performance.

2. **Periodic Synchronization with Pinning Mechanism**
   - Introduces a semi-lockstep synchronization model that aligns the epochs of Hyperchains with the pinning chain.
   - A cryptographically secure **pinning mechanism** anchors the hyperchain state to the pinning chain, ensuring robust alignment and reduced operational overhead.

3. **Future Leader Election**
   - Implements a randomized leader election system, utilizing entropy derived from the pinning chain.
   - This ensures predictable and secure block production, enhancing efficiency and reducing the need for frequent consensus actions.

4. **Finality Guarantees**
   - **Fast Finality:** When all validators are online and behaving correctly, blocks achieve instant finality.

5. **High Throughput**
   - Optimized system design supports processing a high volume of transactions, meeting the demands of modern blockchain applications.

## Governance and Consensus Enhancements

1. **Efficient Consensus Mechanism**
   - Randomized leader election reduces the need for frequent votes, streamlining the consensus process and lowering computational overhead.

## Key Improvements Over Previous Releases
1. **Reduced pinning-Chain Dependency**
   - Optimized the relationship between the hyperchains and pinning chains, minimizing interaction and operational costs while maintaining security.


## Known Limitations
This pre-release does not yet address all advanced features proposed in the whitepaper, such as enhanced fork resolution strategies and multi-epoch staking scenarios.

* Fork resolution is not fully implemented.
* Voting is not fully implemented.
* Fee distribution between block producers is not optimal.
* Default values for token economics are not yet fully balanced.
* Performance is not fully tested and optimized.

# Plans for Version 1.0.0

The 1.0 release aims to achieve the following:
1. **Comprehensive Fork Handling:** Implement robust solutions for fork scenarios.
2. **Enhanced Staking Options:** Introduce multi-cycle locking and flexible staking schedules.
3. **Optimized Block Production:** Refine reward distribution to incentivize timely and accurate block creation.
4. **Enhanced Token Economics:** Refine fees, rewards and penalties in default configuration and present configuration suggestion for different scenarios.
5. **Improved API:** Depending on community feedback the hyperchain API might be changed in version 1.0.0

## Future Releases

Planned updates will focus on:

* Performance
* Enhanced governance through voting
* More advanced staking and delegated staking mechanisms
* Improve end of Epoch voting and progress
* Clean up Bitcoin.NG legacy.

### Feedback and Collaboration

We encourage the aeternity foundation to engage with this release and provide feedback through Æternity’s GitHub repository. Together, we aim to refine Hyperchains into a robust and scalable blockchain solution.