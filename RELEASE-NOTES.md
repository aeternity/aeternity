### About this release

[This release](https://github.com/aeternity/epoch/releases/tag/v0.3.1-big-spenders) is focused on sending tokens from an account to another.

The best way to see this in action is to set up a local node and connect it to the public test network of nodes.

Please follow the instructions below and let us know if you have any problems by [opening a ticket](https://github.com/aeternity/epoch/issues).

### Inspecting the public test network

Information (e.g. height) on the top block of the longest chain as seen by the core nodes of the public test network can be obtained by opening in the browser any of the following URLs:
* http://31.13.248.102:3013/v1/top
* http://31.13.248.103:3013/v1/top
* http://31.13.248.105:3013/v1/top

### Downloading the binaries

Download the [release binary](https://github.com/aeternity/epoch/releases/tag/v0.3.1-big-spenders) corresponding to your platform, e.g. `epoch-0.3.1-osx-10.12.6.tar.gz`. You would normally find the downloaded package in `~/Downloads` on macOS.

Open a Terminal window or get to the command-line.

Create a directory and unpack the downloaded package.

    cd /tmp
    mkdir node1
    cd node1
    tar xf ~/Downloads/epoch-0.3.1-osx-10.12.6.tar.gz

### Configuring

Adjust the configuration for the node as documented in the [wiki](https://github.com/aeternity/epoch/wiki/User-provided-configuration).  An example is provided below.

Let's assume that your public IP address is `1.2.3.4` (replace it with your real IP address) and the listening TCP port on that public address is `9999` (replace it with your real TCP port - considering also your router / NAT configuration if any).
Create the file `/tmp/node1/epoch.yaml` with the following content:

```yaml
---
peers:
    - "http://31.13.248.102:3013/"

http:
    external:
        peer_address: 1.2.3.4
        port: 9999

mining:
    autostart: true

chain:
    persist: true
    db_path: "."
```

### Starting node

    cd /tmp/node1
    ./bin/epoch start

### Stopping mining and node

Please remember to stop the node by running the following command in the `/tmp/node1` directory

    ./bin/epoch stop
