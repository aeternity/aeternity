# Introduction

Hardware requirements may vary based in what mode the node is running, the network and it is supposed to run full sync or restore from database snapshot. Running a full node to do a full sync being the most resource intensive.
Requirements would also vary based on the node configuration, API requests and eventually on future consensus protocol and features.

This recommendations are for nodes part of the **Aeternity Mainnet**

Last update: **October 1, 2024**

## Summary

Running a full node with full sync now and then (i.e. once per month):

- Intel(R) CPU 2 cores @ 3.0Ghz
- 8GB of RAM
- 500GB SSD storage
- 500GB bandwidth

## Processor

A recent generation of Intel(R) 2 CPU logical cores @ 2.5Ghz should cover normal operations.
However, during initial full sync the process is also CPU bound,
so more cores and higher frequency is recommended, for example 2 CPU cores @ 3.0GHz.

## Memory

It is recommended to use at least 8GB RAM.
However 4GB of RAM should be also good enough for modest cases.

## Storage

During initial full sync the node is very disk I/O intensive.
A disk with good I/O throughput is recommended in this case, that's usually a SSDs.
However, after the initial sync the node can run perfectly fine on a commodity HDDs.

### Full Node

Database size as of the moment of this writing is 270GB and grows with 13.5GB per month.
So a reasonable disk space allocation for 1 year horizon would be **450GB**.

### Lightweight Node

Database size as of the moment of this writing is 60GB and grows with 3GB per month.
So a reasonable disk space allocation for 1 year horizon would be **100GB**.

## Bandwidth

During the normal operations the node needs up to 30KB/s inbound and up to 30KB/s outbound traffic.
That's about 78GB/mo per direction or 156GB/mo total.

Notice that, fully utilized chain might go up to 300-500KB/s for gossip traffic.

However during initial full sync this requirement goes as high as 1MB/s but 400KB/s on average inbound traffic.
