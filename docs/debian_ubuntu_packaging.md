Debian (and Ubuntu as its derivative) provides in depth documentation
about creating packages for software. As such, this guide will not
attempt to serve as a replacement. The steps provided here are meant
as a brief introduction into packaging for Debian (and Ubuntu) and the
issues one might came across.

Note: Following the official Debian documentation for packaging, the
information here might refer to `Debian (packaging)`, but it is
implied that the same applies to Ubuntu as a Debian derivative. All
the steps are tested on Ubuntu 16.04 and 18.04.

Debian provides various tools for packaging that have solutions for
most issues a package maintainer might encounter. All of them solve
the same issue - some on higher level, other on more lower level. At
the lowest level of operation they all rely on the same low-level
programs.

The steps on this guide use the `debuild` command for packaging.

For packaging purposes, Debian provides tools (Debian helpers ; dh-*)
for many languages to ease the task.

For software programmed in Erlang and using rebar, there is the
`dh-rebar` helper. However this cannot be used with the current
Aeternity Erlang code-base. The `dh-rebar` uses rebar 2.x, while
Aeternity's code-base uses reabr3. Additionally, during the creation
of this guide no official in-detail Debian documentation and best
practises were found related to packaging Erlang software (with or
without rebar), however there is an official Debian packaging Erlang
team and project.

This is probably not the best approach to package simple Erlang package
that uses rebar, which might be easier to package with the dh-rebar
helper.

Having that in mind, packaging is achieved following the official
building documentation around the `make prod-build` Makefile rules. In
that way, the automated Debian packaging process is overridden to
fine-tune some steps, while using `make prod-build` as a basis for
building the binary.


The packaging tools try to create robust packages and as such, try to
follow best practises and check a lot of details of the final
package. As such, software not installed from a package usually does
not work (well) with the packaging tools. For that reason, special
measures must be taken for Ubuntu 16.04, so a package can be compiled
and packaged with the Debian packaging tools.

To be able to build packages, the `devscripts` package is needed. It
provides various tools needed for the packaging process.

To build the source code Erlang is also needed. The build
documentation states that version `20.1` is the minimal requirement.

Ubuntu 16.04 however does not provide a recent version of Erlang that
could be used for building.

Ubuntu 16.04 fixes for the packaging and building process.

Create an apt preference file that constrains the version to all Erlang packages.

/etc/apt/preferences.d/erlang

#```
Package: erlang* esl-erlang
Pin: version 1:20.2-1*
Pin-Priority: 501

# End
#```


Add additional package repository for Erlang

/etc/apt/sources.list.d/erlangsolutions.list

#```
deb https://packages.erlang-solutions.com/ubuntu xenial contrib

# End
#```

This repository will provide recent Erlang version for Ubuntu 16.04.

Add the repository key to apt

```
cd /tmp
wget https://packages.erlang-solutions.com/ubuntu/erlang_solutions.asc
sudo apt-key add erlang_solutions.asc
```

Update the sources

```
sudo apt-get update
```

# TODO : Add steps for libsodium work-around on Ubuntu 16.04


The rest of the steps should work both on Ubuntu 16.04 and 18.04.

To install all build and packaging dependencies run:

```
apt-get install wget ncurses-dev libssl-dev libsodium-dev git curl debhelper apt-transport-https autoconf build-essential devscripts erlang debhelper dh-autoreconf
```


Packaging

Create the Debian package directory in the source directory structure

```
  cd ae_node_code/
  mkdir debian
```

Create debhelper compatibility file and level

```
echo 8 > debian/compat
```

Note: 

Level 8 is deprecated according to Debian standards. It should not be
used for new packages. However there are issues in levels 9 and 10
with Aeternity Erlang node code-base, which require a lot of patching
in all dependencies. Debian package wrappers set compiler flags for
hardening the code (against memory attacks for example). Those prevent
building the code and all dependencies. Disabling with counter flags
(-Wno-error=some-warning-flag) is error-prone, so the older
compatibility level approach was chosen for an easier start.

Create a control file

touch debian/control

```
Source: aeternity-node
Build-Depends: erlang ( >=1:20.2-1),  erlang-base ( >=1:20.2-1), erlang-dev ( >=1:20.2-1),
               erlang-edoc ( >=1:20.2-1), ncurses-dev, libssl-dev, git, curl
Maintainer: Ivaylo Valkov <ivaylo.valkov@quanterall.com>

Package: aeternity-node
Architecture: any
Depends: ${shlibs:Depends}, ${misc:Depends}
Description: AEternity blockchain implementation in Erlang.
 Optimized for scalability via smart contracts inside
 state-channels. Has a built-in oracle for integration with real-world
 data.
```

This file defines the package meta-information such as name,
dependencies, build dependencies.

Create debian/rules

```
#!/usr/bin/make -f
# -*- makefile -*-

# Uncomment this to turn on verbose mode.
export DH_VERBOSE=1

export DEB_BUILD_MAINT_OPTIONS=hardening=-all

%:
        dh $@

override_dh_auto_build:
        $(MAKE) prod-build

override_dh_auto_clean:
       $(MAKE) prod-clean
       $(MAKE) clean
       $(MAKE) distclean

override_dh_auto_test:
#override_dh_shlibdeps:
#       dh_shlibdeps --exclude=libsodium $@
```

This file is the main recipe that creates a package. It specifies how
to build (compile) the source and can provide rules to override
standard packaging steps (if required). It is a Makefile in
essence. An empty rule skips a step completely.

Create debian/package-name.install if the package source does not have
a `make install` rule. Packaging wrappers relies on `make install` to
collect the files required for the package.

```
_build/prod/rel/*/bin /opt/aeternity/node/
_build/prod/rel/*/data /opt/aeternity/node/
_build/prod/rel/*/erts* /opt/aeternity/node/
_build/prod/rel/*/lib /opt/aeternity/node/
_build/prod/rel/*/patches /opt/aeternity/node/
_build/prod/rel/*/releases /opt/aeternity/node/
_build/prod/rel/*/VERSION /opt/aeternity/node/
_build/prod/rel/*/REVISION /opt/aeternity/node/
```

Every line contains a source file or directory and a destination
separated by space. The file supports wildcards. During the
preparation of this guide the Aeternity Erlang implementation was in a
name change phase (it was called epoch) and the new name was
unclear. Because of this a precise list of the files to be included in
the package was required. Otherwise a simple one-line could be
used. It could depend on the source and building system. For example

```
_build/prod/rel/_some_name_ /opt/aeternity/
```

Create debian/package-name.docs file if there are documentations files
to be included in the package. Those usually go in
`/usr/share/doc/package-name` in Debian and Ubuntu.

```
_build/prod/rel/*/docs/

```

In the case of the Erlang implementation of Aeternity, the
documentation (installation, build, operations etc.) from the `docs/`
directory are installed separately.

Create debian/package-name.links if symbolic links are required for
some reason (purely aesthetic or for operation purposes).

```
var/log/aeternity/node opt/aeternity/node/log
usr/share/doc/aeternity-node/docs/ opt/aeternity/node/docs
```

Create debian/changelog file. This one is essential to package
building. Packaging policy requires this file to be present to track
changes introduced in newer versions and requires a special
format. See `man dch`.

```
aeternity-node (1.1.0) unstable; urgency=medium

  * Initial Debianization

 -- Name Family <name.family@aeternity.com>  Tue, 18 Dec 2018 09:43:00 +0200
```

Building the package.

Debian (and Ubuntu as its derivative) provide several tools to build a
package from higher to low level. For our purposes `debuild` is used.

If all the required files in the `debian/` directory are present,
building the package requires only runnung `debuild`.

```
debuild -b -uc -us
```
