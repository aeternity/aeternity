# Quick Guide to Rebar3

Rebar3 is the standard Erlang build tool

Common commands are:
 - `rebar3 help`
 - `rebar3 get-deps`
 - `rebar3 compile`
 - `rebar3 eunit`
 - `rebar3 ct`
 - `rebar3 shell`
 - `rebar3 release`

## Configuration

Rebar3 reads the `rebar.config` file to know what to do. This consists of
one or more plain Erlang terms (tuples: `{...}`, lists: `[...]`, symbols,
numbers, and strings), each terminated by a full stop and newline, for
example:
```erlang
{erl_opts, [debug_info]}.
```
This sets compilation options and other details, lists dependencies (other
Erlang apps to be fetched automatically) in the `deps` section:
```erlang
{deps, [{gproc, "0.9.0"},
        ...]}.
```
defines how a Release is put together in the `relx` section:
```erlang
{relx, [{release, { aeternity, ...},
        [ ... included-applications ... ]},
        ...
       ]}.
```
and specifies the different Rebar3 Profiles in the `profiles` section:
```erlang
{profiles, [{prod, [... prod-specific-options ...
                   ]},
            {test, [... test-specific-options ...
                   ]},
           ]}.
```

In addition, the file `rebar.config.script` (an Erlang script) will be
executed by Rebar3 if it exists, to perform dynamic configuration.

## Generated files

Rebar3 does not write into the source directories, and instead outputs all
generated files under a separate directory, by default `_build/`. It's
always safe to delete the whole build directory and rebuild everything, if
needed.

## Source files

Rebar3 expects that applications follow the standard Erlang application
structure with a `src/` subdirectory etc. A Rebar3 project can be either a
single application with a `rebar.config` file in the app directory, or it
can consist of a collection of applications under a subdirectory named
`apps/` or `lib/`, with the main `rebar.config` in the root directory. Such
a collection is called an "umbrella project".

An umbrella application is usually published as a Release - a complete
Erlang system to run on some target machine. Often, only a top level
`rebar.config` file is needed, but individual apps (`apps/app1/`,
`apps/app2/`, ...) may have their own `rebar.config` files in order to use
specific build options, pre-build or post-build hooks, etc., for that app
only.

For a single application, it may also be published as a standalone library
(that others can use as a dependency), or turned in to an escript (a
standalone executable).

## Releases

A release is a package that can be installed and run on a target machine,
where the operator doesn't necessarily know anything about the
implementation. When Rebar3 builds a release, typically `rebar3 as prod
release` or `rebar3 as prod tar`, it puts the files under
`_build/$PROFILE/rel/$RELNAME` (see Profiles below). A typical release
specification looks something like this:

```erlang
{relx, [{release, { RELNAME, DEFAULT_VERSION_STRING },
         [app1, app2, ...]},
        {sys_config, "./config/sys.config"},
        {vm_args, "./config/vm.args"},
        {overlay, [{copy, "LICENSE" , "LICENSE"},
                   {copy, "docs/README.md", "docs/REAME.md"}
                  ]}
       ]}.
```

A start script `bin/$RELNAME` will be generated automatically, providing
standard commands like `$RELNAME start`. The listed Erlang apps will be
included in the release package and will be started when the script runs,
using the included `sys_config` and `vm_args` configuration files.

## External Dependencies
Dependencies can be specified either just by name and version, as in
`{deps, [{gproc, "0.9.0"},...]}`, in which case they are downloaded via the
[Hex package manager](https://hex.pm/), or as a Git URL, as in `{deps,
[{cowboy, {git, "https://github.com/ninenines/cowboy.git",
{tag,"2.11.0"}}},...]}`, in which case they are checked out and built. See
Profiles below for details about where the code ends up.

Note that listing an app as a dependency does not automatically include it
in the final Release package - for that to happen, it must also be included
in the `relx` specification (see above). For instance, libraries only used
for building or testing may be listed as dependencies but should not be in
the release spec. Vice versa, just listing an app in the release spec does
not tell Rebar3 how to download that app.

The `relx` section does not however need to list every app that should be
included in the Release. If an app `a` declares (in its `*.app` metadata
file) that it has a runtime dependency on app `b`, then if Rebar3 includes
`a`, it will automatically also include `b`, and so on, transitively, so
that the Release package will contain all apps required for running.

### Dependency pinning
When new dependencies have been fetched, Rebar3 updates the `rebar.lock`
file with more exact information about the version, such as the Git hash,
and not just the branch or tag name used in the `deps` declaration. This
file should typically be kept under version control to ensure repeatable
builds. See the [Rebar3
documentation](https://www.rebar3.org/docs/configuration/dependencies/#lock-files)
for details.

### Checkout dependencies
You can also create a subdirectory or symbolic link named `_checkouts`,
containing apps or links to apps that you have as local files, not yet
published or committed, such as a library that you're currently making
changes to. Apps found under `_checkouts` take precedence over any other
apps with the same names, even if they already exist under `_build`.

## Profiles

The default profile is just the basic `rebar.config` without any other
profile applied. This is used if you e.g. just say `rebar3 compile`. To
apply a profile to a command, say e.g. `rebar3 as prod compile`. You can
use any profile names you like, but some names have special meaning to
Rebar3:
 - The `prod` profile will automatically apply the `prod` mode (see below).
 - When running the commands `rebar3 eunit` or `rebar3 ct`, the profile
   named `test` will be automatically applied.
   - For example, if your tests require the `meck` library to run, you can
   add it as dependency to only the test profile, like this:
   ```{profiles, [{test, [{deps, [meck]}]}]}.```

When Rebar3 builds things, it puts the generated files under
`_build/$PROFILE/`. For example, Erlang apps compiled with `rebar3 compile`
go under `_build/default/lib`, but when compiled with `rebar3 as prod
compile` the files are placed under `_build/prod/lib`.

*Note that external dependencies, as specified in `{deps, []}`, are *always
built using their individual `prod` profiles, no matter what profile Rebar3
has been told to use currently*. The files are however placed under
`_build/default/lib`, not `_build/prod/lib`, because they should be
available under the default profile.

Also note that when Rebar builds other profiles than the default, it does
not rebuild the external dependencies. Instead it creates symbolic links
from `_build/$PROFILE/lib` into `_build/default/lib`. The exception is
dependencies specified as part of an individual profile, as in `{profiles,
[{test, [{deps, [meck]}]}]}`, which get stored under that profile (in this
case `_build/test/lib`) since they should not be available under the
default profile.

These locations are typically not the final destination for the compiled
files. Usually, they will later get copied into a Release package under
`_build/$PROFILE/rel` for distribution as a tarball or similar.

## Modes

Modes are shortcuts for some basic settings, for example `{mode, prod}`
sets some typical options for production. The builtin modes are:
 - `prod`: Include the Erlang Runtime System in the release package, don't
    include source code, and strip any debug information. Copy files into
    the release package instead of using symbolic links.
 - `minimal`: Like `prod` but does not include the Erlang Runtime System.
 - `dev`: The inverse of `prod`.

In particular, `{mode, dev}` implies the `{dev_mode, true}` option, which
creates symbolic links instead of copying files when composing a release.
This means that you don't need to rebuild the release when you make a small
change during development; just recompiling is enough.
