
## Customizing this release.

Currently, there are two basic ways of customizing epoch:

* Updating the official `sys.config` file
* Specifying your own config file at startup

Both methods require complying with the format of Erlang/OTP config files,
which will be briefly described by example below.

### The official `sys.config` file

The location of the `sys.config`, relative to the directory where `epoch`
is installed, is `releases/0.2.0/sys.config`.

### Specifying your own config

Normally, you would start epoch calling `bin/epoch start`.
You can add a config file by calling

```
ERL_FLAGS="-config <full_path_to_config>" bin/epoch start
```

(Use a fully qualified name to avoid confusion)

The file name must end with `.config`. The extension can be specified
or left out in `<full_path_to_config>` above.

### Things you can configure

The main things you may want to configure are:

* The port number used for the HTTP service end point (default: `3013`)
* The list of peers to contact at startup (default: none)

While some other things are configurable in theory, please leave them alone
unless you fully understand what you're doing.


The following example sets the port to `9900` and adds two peer URIs:

```erlang
[
 {aecore,
  [
   {peers, ["http://somehost.net:4000/",
            "http://someotherhost.net:5000/"]}
  ]},
 {aehttp,
  [
   {swagger_port_external, 9900}
  ]}
].
```

Note that whitespace and indentation are ignored by Erlang, but it's
picky about delimiters. The format is one of:

```erlang
[ App1, ..., Appn ].
```

(Be sure to remember the period (`'.'`) at the end.)

where `aecore` and `aehttp` above are application names. Each application
config is on the form:

```erlang
{ AppName, [ Var1, ..., Varn ] }
```

where a Var is a `{ Key, Value }` pair.

note that there is no trailing comma after the last element.

For clarity, specifying only the peers would look like this:

```erlang
[
 {aecore,
  [
   {peers, ["http://somehost.net:4000/",
            "http://someotherhost.net:5000/"]}
  ]}
].
```

(Never a trailing comma directly before a closing `}` or `]`.)

### Manually connecting to a peer

It is possible to instruct a running `epoch` node to connect to another
peer, even if it didn't know about it before. Using one of the example
peers above:

```
bin/epoch rpc aec_sync connect_peer http://somehost.net:4000/
```

If the connected peer knows of other peers, it will share them in the
handshake, and your node will try to connect to them as well.

If you want to check if something is happening when you do this, you
may follow the output in `log/epoch.log`. It may appear noisy unless you're
used to Erlang logs, but at least there will be activity.
