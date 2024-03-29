# Introduction

The Ubuntu release packages ships with a CUDA miner.
This documentation describes how to use the CUDA miner shipped in the Ubuntu release package, how to build it yourself (if you prefer to do so), and the related configuration of the Aeternity node.

The documentation below assumes that an Aeternity node is already installed either by [release package](installation.md) or [from source](build.md), thus its dependencies are also installed.
The documentation below assumes that the Aeternity node's source code resides in `~/aeternity/node` directory.

## Pre-built miner

This section describes how to use the CUDA miner shipped in the Ubuntu release package.

### CUDA Driver installation

In order to use the CUDA miner shipped in the Ubuntu release package, you need to install the CUDA driver version 10.

Please refer to the NVIDIA documentation for how to [download][cuda-downloads] and [install][cuda-installation] the CUDA driver.

### Configuration of the pre-built CUDA miner

Once the CUDA driver is installed, one should change the node configuration to start using the CUDA miner.
The `mining.cuckoo.miner` section of `~/aeternity/node/aeternity.yaml` should be changed to:

```yaml
mining:
    cuckoo:
        edge_bits: 29
        miners:
            - executable_group: aecuckooprebuilt
              executable: cuda29
              extra_args: ""
              hex_encoded_header: true
```

Notice the `executable_group`.

After configuration the node could be started (or restarted if it's already running):
```
~/aeternity/node/bin/aeternity start
```

Available executables are: `cuda29`, `lcuda29`.

## Building

This section describes how to build the CUDA miner yourself.

The Ubuntu release package ships with a CUDA miner that is already built, so you do not need to built the CUDA miner.
Advanced users may prefer to build the CUDA miner themselves, though most users do not need to and can therefore skip this section.

You can build the CUDA miner yourself by following these steps:

- CUDA toolkit installation
- CUDA miner install
- Aeternity node configuration

The documentation in this section is tested on:
- Aeternity node version 2.0.0-rc.1
- CUDA toolkit version 9.2
- AWS p2.xlarge instance with 16GB EBS
- Ubuntu 16.04.4
- non-root user with `ALL` sudo privileges

Make sure the Aeternity node is stopped to speedup the installation process.

### CUDA toolkit installation

Download the official CUDA repository package and install it. A sudo user should be used:

```bash
cd ~
wget https://developer.nvidia.com/compute/cuda/9.2/Prod/local_installers/cuda-repo-ubuntu1604-9-2-local_9.2.88-1_amd64
sudo dpkg -i cuda-repo-ubuntu1604-9-2-local_9.2.88-1_amd64
sudo apt-key add /var/cuda-repo-9-2-local/7fa2af80.pub
```

After the apt repository is set, install CUDA:

```bash
sudo apt-get update && sudo apt-get install cuda
```

### Miner install

At this point the CUDA toolkit is installed. Next step is to build the cuckoo CUDA miner.

The source code of the CUDA miner is in `aecuckoo` git repository:
```bash
cd ~
git clone https://github.com/aeternity/aecuckoo.git aecuckoo_source && cd aecuckoo_source
git checkout fa3d13e   # This commit is used in Aeternity node
```

Cuckoo CUDA build assumes CUDA compiler (`nvcc`) is install in `PATH`, however it is installed by the above steps in `/usr/local/cuda-9.2/bin` which is not in the `PATH` by default. To add CUDA compiler to the `PATH` environment variable run:

```bash
export PATH=/usr/local/cuda-9.2/bin${PATH:+:${PATH}}
```

Compilation of CUDA miner is done in `aecuckoo_source` directory by invoking:

```bash
make cuda29
```

Finally the actual installation of the miner binary is copying it to the node corresponding path, the documentation assumes the Aeternity node is installed in `~/aeternity/node` directory.

The exact path where to copy the binary depends on the version of the node: you can find it by calling `ls -d ~/aeternity/node/lib/aecuckoo-*/priv/bin`.
E.g. it may be something like `~/aeternity/node/lib/aecuckoo-1.0.0/priv/bin`: the following assumes this path though you may need to adapt it.

```bash
cp priv/bin/cuda29 ~/aeternity/node/lib/aecuckoo-1.0.0/priv/bin
```

### Configuration of the manually built CUDA miner

Once the CUDA miner is in place, one should change the node configuration to start using it. The `mining.cuckoo.miner` section of `~/aeternity/node/aeternity.yaml` should be changed to:

```yaml
mining:
    cuckoo:
        edge_bits: 29
        miners:
            - executable: cuda29
              extra_args: ""
              hex_encoded_header: true
```

After updating the configuration, the node should be started (or restarted if it's already running):

```
~/aeternity/node/bin/aeternity start
```

## Configuration

The examples in this section use the CUDA miner that is shipped in the release package.
If you prefer to use a manually built CUDA miner, please amend the configuration accordingly.

### Mining efficiency

There is quite a bit of overhead starting the GPU miner, thus running single
mining attempts is not the best option. Therefore there is the configuration
option `repeats: N` which will make multiple mining attempts (with different
nonces) in one miner context. However, this option has to be used with CAUTION,
the total run-time of the miner should preferably not exceed 5 seconds. The
reason being that with the short block interval of BitCoin NG micro blocks (3
seconds) - we risk mining on old blocks otherwise. (And thereby missing out on
the reward collected from the transaction fees in the micro blocks.)

To fine tune the parameter, you should try running the miner in a shell
```
$ time ~/aeternity/node/lib/aecuckooprebuilt-*/priv/cuda29 -r 5
...
real 0m4.634s
user ...
sys  ...
```
Here it took 4.6s, which is rather close to 5s, so maybe `4` is the best value
for this node.

Repeats are configured like this:
```yaml
mining:
    cuckoo:
        edge_bits: 29
        miners:
            - executable_group: aecuckooprebuilt
              executable: cuda29
              repeats: 4
              extra_args: ""
              hex_encoded_header: true
```

**Don't be tempted to use `-r` as `extra_args`** the Aeternity node will **not**
handle nonces correctly in this case.

### Multiple GPU devices

The address of a GPU device used by the miner can be set with `-d` argument, for example to set device with address 0:

```yaml
mining:
    cuckoo:
        edge_bits: 29
        miners:
            - executable_group: aecuckooprebuilt
              executable: cuda29
              extra_args: "-d 0"
              hex_encoded_header: true
```

The address of the device can be obtained by running `nvidia-smi`

However, if you want to use multiple cards for GPU mining you can add another
configuration option, `instances`, for example if you have two (2) GPU-cards:

```
mining:
    cuckoo:
        edge_bits: 29
        miners:
            - executable_group: aecuckooprebuilt
              executable: cuda29
              extra_args: ""
              hex_encoded_header: true
              instances: [0,1]
              repeats: 5
```

*Note:* You should not have `-d` in `extra_args` if you are using the `instances` configuration option, it will be added automatically
by the node.

*Note:* If you are combining `repeats` and `instances`, it is the *number of repeats per instance* that is configured! I.e. with 2
instances and repeats 5 each GPU will run 5 attempts per run.

### Multiple GPU miners

If you want to address different GPU-cards with different miners' configurations, you can set up multiple miners in the config.
For example, if you have 4 GPU-cards, and you want to address 0 and 1 with different config than 2 and 3, you can use the following config:
```yaml
mining:
    cuckoo:
        edge_bits: 29
        miners:
            - executable_group: aecuckooprebuilt
              executable: cuda29
              extra_args: ""
              hex_encoded_header: true
              addressed_instances: [0,1]
              repeats: 2
            - executable_group: aecuckooprebuilt
              executable: cuda29
              extra_args: ""
              hex_encoded_header: true
              addressed_instances: [2,3]
              repeats: 5
```

## References

- [NVIDIA CUDA Installation Guide for Linux][cuda-installation]
- [CUDA Downloads][cuda-downloads]
- [Cuckoo Cycle Documentation](https://github.com/tromp/cuckoo)

[cuda-installation]: https://docs.nvidia.com/cuda/cuda-installation-guide-linux/index.html
[cuda-downloads]: https://developer.nvidia.com/cuda-downloads?target_os=Linux&target_arch=x86_64&target_distro=Ubuntu&target_version=1604
