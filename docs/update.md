# Introduction

This document describes how to update an Aeternity node using a release binary:

* [Manual update](#manual-update);

## Manual update

### Ubuntu 18.04

Be sure to have a backup of your files, don't remove your node database.

Download the latest version of the æternity node

```
curl -Lf -o aeternity.tar.gz  https://releases.aeternity.io/aeternity-{version}-ubuntu-x86_64.tar.gz
```

Once done, stop your node by:

```
/path/to/your/node/bin/aeternity stop
```

Decompress your new node version overwritting the old version:

```
tar -xzf aeternity.tar.gz -C /path/to/your/node;
```

And start your node again:

```
/path/to/your/node/bin/aeternity daemon
```

Now check that your node is up and running by :


```
curl localhost:3013/v2/blocks/top
```
