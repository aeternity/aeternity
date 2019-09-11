# Introduction

This document describes how to build an Aeternity node from source on Windows using
[MSYS2][msys2].

NOTES:
 - Only 64-bit versions of Windows 10 and Windows Server 2016 or later are supported and tested.
 - Make sure you pull with `git config --global autocrlf input` as windows scripts might not work
 - Administrative privileges are recommended.

## Dependencies

You will need (minimal required):
 - Visual Studio CLI Build tools (Community Edition 2017+)
 - Msys2
 - Erlang/OTP (20.1 or 20.3 is officially supported)
 - Java/OpenJDK (11.0.2+)

Note: You might consider easier to use [Chocolatey][chocolatey] package manager to
install the requirements

### [Install Visual Studio 2019][vs_install]
First you will need to install Visual Studio in its default installation path, else the next steps will fail.

To install only the required components of Visual Studio 2019 Community Edition using [Chocolatey][chocolatey] run:

```
> choco install -y visualstudio2019-workload-vctools --params "--add Microsoft.VisualStudio.Component.VC.CLI.Support --locale en-US"
```

or manually

**Download:** [Visual Studio 2019 Installer][vs_dl]

Make sure to include the following components (use the VCTools workload as base):

- Command-line compiler support
- Windows 10 SDK

Alternatively you can use [vs_buildtools.exe][vs_buildtools] to reduce install size (no GUI).

Note: Visual Studio 2017 Community Edition or later is supported

### Quick install of MSYS2, Erlang/OTP, Java

The easiest way to setup the environment is to run `scripts/windows/msys2_prepare.bat`.

It will install any missing tools to their default locations, except Visual Studio.

If you do so, you may skip to [Setup](#Setup)

## Custom install of dependencies

### [Install MSYS2][msys2] (optional)

Note: If you don't want to install [MSYS2][msys2] manually, the preparation script
will do it automatically for you.

Else you can use the provided helper script `scripts\windows\install_msys2.bat`.

It will download and install all the dependencies and will dump the ENV vars used by the build scripts.
You may optionally provide installation path as an argument. 

*The default <install_path> could be specified in `WIN_MSYS2_ROOT` env var or fallback to "C:\tools\msys64"* 

```
> scripts\windows\install_msys2.bat C:\tools\msys64
```

Or if you prefer you can install Msys2 using [Chocolatey][chocolatey] 

```
choco install -y msys2
```

or manually

**Download:** [MSYS2 Windows Installer][msys2_dl]

- Execute installer and follow instructions
- Keep note of the install folder

*Note: You will need to set properly the var (e.g.):*
```
> SET "WIN_MSYS2_ROOT=C:\tools\msys64"
```
*Note: Odd quoting is not a typo*

### [Erlang/OTP 20.3][otp] (optional)

If you don't want to install [Erlang/OTP][otp] manually, the preparation script
will do it automatically for you.

Else you can use `scripts\windows\install_erlang.bat`.
 
It will download and install Erlang/OTP and will dump the ENV vars used by the build scripts

```
> scripts\windows\install_erlang.bat 20.3 C:\tools\erl9.3
...
SET "WIN_OTP_PATH=C:\tools\erl9.3"
SET OTP_VERSION=20.3
SET ERTS_VERSION=9.3
```

Alternatively you can install Erlang/OTP using [Chocolatey][chocolatey] 

```
choco install -y erlang --version=20.3
```

or manually

**Download:** [Erlang/OTP 20.3 Windows Installer][otp203_dl]

- Execute installer and follow instructions
- Keep note of install folder

*Note: You will need to set properly the vars (e.g.):*
```
SET "WIN_OTP_PATH=C:\Program Files\erl9.3"
SET OTP_VERSION=20.3
SET ERTS_VERSION=9.3
```

### [Java Development Kit 11+][jdk] (not required)

JDK in no longer essential for building Aeternity and will not be installed by the preparation script.

## Setup

Now the [MSYS2][msys2] environment needs to be prepared. This can be done 
automatically by the helper script `scripts/windows/msys2_prepare`.
It will set the environment variables and download any missing essential tools.

This script uses the following environment variables and default values:

```
SET "WIN_MSYS2_ROOT=C:\tools\msys64"
SET "WIN_OTP_PATH=C:\tools\erl21.3"
SET ERTS_VERSION=10.3
SET OTP_VERSION=21.3
```
*Note: Odd quoting is used to escape any spaces in the values. 
Make sure paths do not include quotes and trailing slashes.*

If your local setup differs, you need to set the proper values yourself before running the preparation script.

It is recommend to persist these vars into the user registry, so you don't need to set it every session, e.g.:
```
SETX WIN_MSYS2_ROOT C:\tools\msys64
SETX WIN_OTP_PATH C:\Program Files\erl9.3
```

*Note: In contrast of SET, do not put quotes in SETX commands, as they will end up in the values*

### First time installation

The helper scripts will try to detect where `msys2.exe` is installed searching `WIN_MSYS2_ROOT`
or the system paths (`%PATH%`). If the detection fails, `install_msys2` script will be used to install it.

Similarly, if Erlang/OTP is missing it will be installed using `install_erlang` script.

After that script will download, install and update the Msys2 dependencies and tools.
Any consecutive runs will check for new updates.

### Additional environment config (optional)

Msys2 specific paths can be specified in `MSYS_INCLUDE_PATH` which are in POSIX(Unix-style) form, separated by ":".

For instance, to add Java in msys2 you have to set the path to its `bin` directory (the one that includes `java.exe`)

```
SET "MSYS_INCLUDE_PATH=/c/Program Files/OpenJDK/jdk-12.0.2/bin:%MSYS_INCLUDE_PATH%"
```

You can execute the script directly in a `cmd` window.

## Building

Open a [MSYS2][msys2] shell with the proper paths set. 

Use the helper script `scripts/windows/msys2_shell.bat` to do so.

That script uses the following environment variables (defaults):

```
WIN_MSYS2_ROOT=C:\tools\msys64
WIN_OTP_PATH=C:\tools\erl21.3
PLATFORM=x64
ERTS_VERSION=10.3
OTP_VERSION=21.3
```

In the opened shell (MinGW64) go into your build directory and build the system like on
any other UNIX system:

```bash
cd PATH_TO_REPO_CHECKOUT
make
```

NOTE: Disk drives are mounted in the root folder (i.e. `C:` is `/c`)

Note: For a release package build you can use `.circleci\windows\build.cmd` which will build and produce ready-to-install packages

Refer to `docs/build.md` for more information on how to build.

[chocolatey]: https://chocolatey.org/docs/installation#installing-chocolatey
[msys2]: https://www.msys2.org/
[jdk]: https://download.java.net/java/GA/jdk11/9/GPL/openjdk-11.0.2_windows-x64_bin.zip
[msys2_dl]: http://repo.msys2.org/distrib/x86_64/msys2-x86_64-20180531.exe
[otp]: http://www.erlang.org/
[otp203_dl]: http://erlang.org/download/otp_win64_20.3.exe
[vs_install]: https://docs.microsoft.com/en-us/visualstudio/install/install-visual-studio
[vs_dl]: https://visualstudio.microsoft.com/downloads/
[vs_buildtools]: https://visualstudio.microsoft.com/downloads/#build-tools-for-visual-studio-2019
