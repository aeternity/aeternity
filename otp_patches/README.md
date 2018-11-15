# Emergency patching of OTP modules

The purpose of this directory is to hold patches for OTP modules.
The patch files are generated using `git diff`, e.g.

## Usage

```
git diff OTP-20.1 -- lib/mnesia/src/mnesia_index.erl > $OTP_PATCHES/20.1/mnesia_index.erl.patch
```

Patch files are stored in version-specific subdirectories. In each such
subdirectory, a `versions.eterm` file contains patterns identifying which
OTP application versions are compatible with the patches.

The `versions.eterm` file is read with `file:consult/1`, and shall
contain `{AppName, BaseNameRegexp}` tuples. For example:

```erlang
{mnesia, "mnesia-4\\.15\\.[123]"}.
```

which matches mnesia versions `"4.15.1"`, `"4.15.2"` and `"4.15.3"`.

The compile script will test each subdirectory, and pick the first one
for which all application patterns match. It will then copy the relevant
source files from the current OTP to the `otp_patches` directory, apply
the patches and compile the result into `otp_patches/ebin`. This directory
will then be copied into the release and pre-pended to the code path of
the system.

## rebar3 hook

In the `rebar.config` file, the patch compilation can be specified as
a `pre_hook`, e.g.

```
{pre_hooks, [{compile, "escript ./otp_patches/compile.escript"},
             ...]}.
```

## Compile options

Currently, files are compiled with `debug_info` and include paths to the
`src/` and `include/` directories of the source file's application.
More compile options, and support for specifying options per-application
or per-module may have to be added later.