# CUDA Miner

The release packages do not ship with a CUDA miner, but you can build it yourself by following these steps:

- CUDA toolkit installation
- CUDA miner install
- Epoch node configuration

The documentation below is tested on:
- Epoch version 0.15.0
- CUDA toolkit version 9.2
- AWS p2.xlarge instance with 16GB EBS
- Ubuntu 16.04.4
- non-root user with `ALL` sudo privileges

The documentation also assumes that an `epoch` node is already installed either by [release package](installation.md) or [from source](build.md), thus it's dependencies are also installed.
Make sure the `epoch` node is stopped to speedup the installation process.

## CUDA toolkit installation

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

## Miner install

At this point the CUDA toolkit is installed. Next step is to build the cuckoo CUDA miner. If the node has been installed (build) from source, the same source tree can be used. Otherwise if the binary package has been used for installation, **the same version** of epoch source code must be downloaded.

Epoch source code can be downloaded by cloning the git repository:
```bash
cd ~
git clone https://github.com/aeternity/epoch.git epoch && cd epoch
git checkout tags/v0.15.0
```

The documentation below assumes that the `epoch` source code resides in `~/epoch` directory.

Cuckoo CUDA build assumes CUDA compiler (`nvcc`) is install in `PATH`, however it is installed by the above steps in `/usr/local/cuda-9.2/bin` which is not in the `PATH` by default. To add CUDA compiler to the `PATH` environment variable run:

```bash
export PATH=/usr/local/cuda-9.2/bin${PATH:+:${PATH}}
```

Compilation of CUDA miner is done by invoking:

```bash
cd apps/aecuckoo && make c_src/.git
cd c_src/src && make libblake2b.so cuda28
```

Finally the actual installation of the miner binary is copying it to the node corresponding path, the documentation assumes the `epoch` node is installed in `~/node` directory.

```bash
cp cuda28 ~/node/lib/aecuckoo-0.1.0/priv/bin
```

## Configuration

Once the CUDA miner is in place, one should change the node configuration to start using it. The `mining.cuckoo.miner` section of `~/node/epoch.yaml` should be changed to:

```yaml
mining:
    autostart: true
    expected_mine_rate: 300000
    cuckoo:
        miner:
            executable: cuda28
            extra_args: "-t 16384"
            node_bits: 28
```

After configuration could be started (or restarted if it's already running):

```
~/node/bin/epoch start
```

### Miner threads

The `extra_args: "-t 16384"` in the configuration file denotes how many threads will be run on the GPU. The optimal value can be experimentally determined by running:

```bash
cd ~/epoch/apps/aecuckoo/c_src/src
for t in 65536 32768 16384 8192 4096 2048 1024; do echo $t; time ./cuda28 -t $t -r 10; done;
```

Look for "elapsed" time in the output, the shorter the better.

## References

- [NVIDIA CUDA Installation Guide for Linux](https://docs.nvidia.com/cuda/cuda-installation-guide-linux/index.html)
- [CUDA Downloads](https://developer.nvidia.com/cuda-downloads?target_os=Linux&target_arch=x86_64&target_distro=Ubuntu&target_version=1604)
- [Cuckoo Cycle Documentation](https://github.com/tromp/cuckoo)
