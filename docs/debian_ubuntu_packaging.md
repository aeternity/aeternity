Debian and Ubuntu as its derivative both provide in depth
documentation about creating packages for software. As such, this
guide will not attempt to serve as a replacement. The steps provided
here are meant as a brief introduction into packaging for Debian (and
Ubuntu) and the issues one might came across. Most of the focus of
this document is towards issues related to packaging Aeternity code.


**Note:** 

Following the official Debian documentation for packaging, the
information here might refer to `Debian (packaging)`, but it is
implied that the same applies to Ubuntu as a Debian derivative. All
the steps are tested on Ubuntu 16.04 and 18.04.

For full documentation about Debian packages, please review the
following documents:

 * [Debian New Maintainers' Guide](https://www.debian.org/doc/manuals/maint-guide/)
 * [Debian Policy](https://www.debian.org/doc/debian-policy/)
 * [Ubuntu Packaging Guide](http://packaging.ubuntu.com/html/)

# High-level overview

Debian provides various tools for packaging that have solutions for
most issues, a package maintainer might encounter. All of them solve
the same issue - some on higher level, other on more lower level. At
the lowest level of operation they all rely on the same low-level
tools.

All tools require additional files to be added to the source code for
the generation of packages. Those files usually have strict
format, and could be observed as a meta-config files. One exception is
the `rules` file of a package, that in essence is a `Makefile`.

All files required generally reside in the `debian/` directory in the
software source tree.

The steps on this guide use the `debuild` command for packaging.

## Erlang packaging notes
For the purpose of packaging, Debian provides tools (Debian helpers ;
dh-*) for many languages to ease the task.

For software programmed in Erlang and using `rebar`, there exists a
helper `dh-rebar`. However this cannot be used with the current
Aeternity Erlang code-base.

The `dh-rebar` helper uses rebar 2.x, while Aeternity's code-base
requires rebar3. Additionally, during the creation of this guide no
official in-detail Debian documentation and best practises were found
related to packaging Erlang software (with or without rebar), however
there is an official Debian packaging Erlang team and project.

The approach taken by this guide might not be the best approach to
package simple Erlang package that uses rebar, which might be easier
to package with the `dh-rebar` helper.

Having that in mind, packaging is achieved following the official
[building documentation](build.md) around the `make prod-build`
Makefile rules. In that way, the automated Debian packaging process is
overridden to fine-tune some steps, while using `make prod-build` as a
base for building the binary.

# Requirements

The packaging tools attempt to generate robust packages and as such,
try to follow best practises, and check a lot of details of the final
package. This includes shared libraries and dependencies checks,
packaging config files issues etc. 

As such, software not installed from a package usually does not work
(well) with the packaging tools. For that reason, special measures
must be taken for Ubuntu 16.04, so a package can be compiled and
packaged with the Debian packaging tools.

To be able to build packages, the `devscripts` package is required. It
provides various tools needed for the packaging process.

To build the source code of Aeternity, Erlang is also required. The
[build documentation](build.md) states that version `20.1` is the
minimal requirement.

Ubuntu 16.04, however does not provide a recent version of Erlang that
can be used for building.

## Ubuntu 16.04 fixes

As mentioned earlier, it is best to have all requirements for a
software to be packaged as a Debian package. This requires some
work-arounds for Ubuntu 16.04, so the packaging process can be
successful.

### Erlang version

A suitable version of Erlang should be installed from the package
repositories that Erlang Solutions provides.

However simply adding a repository source in Debian or Ubuntu will not
work in this case. To avoid specifying the exact version of every
Erlang related package, required, an apt preference and pinning is
required.

Create an apt preference file, that will constrain the version to all
Erlang packages.

*/etc/apt/preferences.d/erlang*

```
Package: erlang* esl-erlang
Pin: version 1:20.2-1*
Pin-Priority: 501
```

Install the https apt transport:

```
sudo apt-get install apt-transport-https

```

Add the additional package repository for Erlang.

*/etc/apt/sources.list.d/erlangsolutions.list*

```
deb https://packages.erlang-solutions.com/ubuntu xenial contrib
```

This repository will provide recent Erlang version for Ubuntu 16.04.

Add the repository key to apt

```
cd /tmp
wget https://packages.erlang-solutions.com/ubuntu/erlang_solutions.asc
sudo apt-key add erlang_solutions.asc
```

Update the sources:

```
sudo apt-get update
```

### TODO : Add steps for libsodium work-around on Ubuntu 16.04


## General requirements

The rest of the steps should work on both Ubuntu 16.04 and 18.04.

To install all build and packaging dependencies run:

```
sudo apt-get install wget ncurses-dev libssl-dev libsodium-dev git curl debhelper apt-transport-https autoconf build-essential devscripts erlang debhelper dh-autoreconf
```

## Package internals

Create the Debian package directory in the source directory structure

```
  cd ae_node_code/
  mkdir debian
```

Create debhelper compatibility file and level

*debian/compat* [[1]](https://www.debian.org/doc/manuals/maint-guide/dother.en.html#compat)


```
echo 8 > debian/compat
```

**Note: Level 8 is deprecated according to Debian standards**.

It should not be used for new packages. However there are issues in
levels 9 and 10 with Aeternity Erlang node code-base, which require a
lot of patching in all dependencies. Debian package wrappers set
compiler flags for hardening the code (against memory attacks for
example).

Those prevent building the code and all dependencies. Disabling with
counter flags (-Wno-error=some-warning-flag) is error-prone, so the
older compatibility level approach was chosen for an easier start.

### control file

*debian/control* [[1]](https://www.debian.org/doc/manuals/maint-guide/dreq.en.html#control) [[2]](https://www.debian.org/doc/debian-policy/ch-controlfields.html)

```
Source: aeternity-node
Section: misc
Priority: optional
Maintainer: Full Name <full.name@aeternity.com>
Build-Depends: erlang ( >=1:20.2-1),  erlang-base ( >=1:20.2-1), erlang-dev ( >=1:20.2-1),
               erlang-edoc ( >=1:20.2-1), ncurses-dev, libssl-dev, git, curl

Package: aeternity-node
Architecture: any
Depends: ${shlibs:Depends}, ${misc:Depends}
Description: Aeternity blockchain implementation in Erlang.
 Optimized for scalability via smart contracts inside
 state-channels. Has a built-in oracle for integration with real-world
 data.
```

This file defines the package meta-information such as name,
dependencies, build dependencies.

### rules file

*debian/rules* [[1]](https://www.debian.org/doc/manuals/maint-guide/dreq.en.html#rules) [[2]](https://www.debian.org/doc/debian-policy/ch-source.html#s-debianrules)


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
```

This file is the main recipe that creates a package. It specifies how
to build (compile) the source and can provide rules to override
standard packaging steps (if required). It is a Makefile in
essence. An empty rule skips a step completely.

### install file

*debian/package_name.install* [[1]](https://www.debian.org/doc/manuals/maint-guide/dother.en.html#install)

Create `debian/package-name.install` if the package source does not have
a `make install` rule. Packaging wrappers rely on `make install` to
collect the files required for the package.

```
_build/prod/rel/aeternity/* /opt/aeternity/node/
```

Every line contains a source file or directory and a destination
separated by space. The file supports wildcards.

### docs file

*debian/pacakge_name.docs* [[1]](https://www.debian.org/doc/manuals/maint-guide/dother.en.html#docs)
Create `debian/package-name.docs` file if there are documentation files
to be included in the package. Those usually go in
`/usr/share/doc/package-name` in Debian and Ubuntu.

```
_build/prod/rel/*/docs/

```

In the case of the Erlang implementation of Aeternity, documentation
files are not installed in that directory. However a symbolic link is
created in `/usr/share/docs/aeternity-node/docs` to point to the files
in `/opt/aeternity/node/docs/`. This is required since the `install`
file does not provide a robust way to separate or exclude directories.


### links file

*debian/pacakge_name.links* [[1]](https://www.debian.org/doc/manuals/maint-guide/dother.en.html#links)

Create `debian/package-name.links` if symbolic links are required for
some reason (purely aesthetic or for operation purposes).

```
var/log/aeternity/node opt/aeternity/node/log
opt/aeternity/node/docs usr/share/doc/aeternity-node/docs
```

### changelog file

*debian/changelog* [[1]](https://www.debian.org/doc/manuals/maint-guide/dreq.en.html#changelog) [[2]](https://www.debian.org/doc/debian-policy/ch-source.html#debian-changelog-debian-changelog)

Create `debian/changelog` file. This one is essential to package
building. Packaging policy requires this file to be present to track
changes introduced in newer versions and requires a special format. It
is not advisable to try to create it by hand. It is best to see `man
dch` or `man debchange`.

Example:

```
aeternity-node (1.1.0) unstable; urgency=medium

  * Initial Debianization

 -- Full Name <full.name@aeternity.com>  Tue, 18 Dec 2018 09:43:00 +0200
```

## Generating changelog file.


[Debian New Maintainers' Guide](https://www.debian.org/doc/manuals/maint-guide/dreq.en.html#changelog)
and
[Debian Policy](https://www.debian.org/doc/debian-policy/ch-docs.html#changelog-files-and-release-notes)
require an up to date changelog file.

The best solution would be to add the changes for every Aeternity
release to the changelog file.  However there is no easy way to do it
from existing data, as the changelog file has a special format.

One way would be to use Git commit messages and the `git-buildpackage`
tools. However this is not very practical for Aeternity codebase. This
would require special branches in the Git repository and this is
something that (potentially) should be avoided.

Another approach would be to convert the RELEASE-NOTES file for every
release into a Debian changelog file. However this is impractical. The
RELEASE-NOTES file is a Markdown file with vague formatting (sections,
topics etc.) and can't be converted to a Debian changelog file
straight-forward.

The simplest solution is to add a single comment in the Debian
changelog file pointing to the RELEASE-NOTES file on disk (in the
package) for the release. Even it might not be strictly in sync with
the Debian changelog file policy. This is the approach used.

### Using dch

```
cd ae_node_source/
AE_VERSION=$(cat VERSION)
AE_VERSION_NOTE="Release notes are available in /usr/share/doc/aeternity-node/docs/release-notes/RLEASENODES-$AE_VERSION.md"

dch -v $AE_VERSION $AE_VERSION_NOTE
dch -r  $AE_VERSION_NOTE
```

Both commands are required and cannot be combined. The `-v` option
adds a new version to the changelog file. The `-r` option finalizes
the release (version) in the changelog file.

## Building binary package.

Debian (and Ubuntu as its derivative) provide several tools to build a
package from higher to low level. For our purposes `debuild` is used.

If all the required files in the `debian/` directory are present,
building the package requires only running `debuild`.

```
debuild -b -uc -us
```

This command will build the binary package and will skip signing with
GPG the changes (`-uc`) file and source package (`-us`). This is
useful during the initial Debianization and when testing changes. 

Debuild calls additional tools to prepare the package. The `-us` and
`-uc` options are for the `dpkg-buildpackage` command.


**Note: Command line options passed to `debuild` have strict order.**

First are `debuild` arguments, then `dpkg-buildpackage` ones, and last
options to `lintian`. For full description and workflow check
`debuild` documentation (`man debuild`).**

To build a source package, change the `-b` option to `-S`

## Building source package

```
debuild -S -uc -us
```


**TODO:**

# Distributing packages
## PPA creation (Launchpad)
[[1]](https://help.launchpad.net/Teams/CreatingAndRunning)
[[2]](https://launchpad.net/people/+newteam)

Launchpad cannot be used for Debian package building with the current
Aeternity codebase. Source code building requires external
dependencies from GitHub. Launchpad accepts only source Debian
packages and builds them. However Launchpad is restricted (DNS) for
obvious reasons and cannot build our packages.
