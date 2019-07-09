# Build from source

This document describes how to build an Aeternity node from source on Windows using
[MSYS2][msys2].

NOTE: Only 64-bit versions of Windows 10 and Windows Server 2016 are supported and tested.
 
Administrative privileges are required.

## Dependencies

Note: You might consider easier to use [Chocolatey][chocolatey] package manager to
install the requirements

### [MSYS2][msys2]

Install Msys2 using [Chocolatey][chocolatey] 

```
cinst msys2 -y
```

or manually

**Download:** [MSYS2 Windows Installer][msys2_dl]

- Execute installer and follow instructions
- Keep note of install folder

### [Visual Studio 2017/2019][vs2017]

Install only the required components of Visual Studio 2019 using [Chocolatey][chocolatey]

```
cinst visualstudio2019-workload-vctools -y --params "--add Microsoft.VisualStudio.Component.VC.CLI.Support --locale en-US"
```

or manually

**Download:** [Visual Studio 2019 Installer][vs2019_dl]

Make sure to include the following components (use the VCTools workload as base):

- Command-line compiler support
- Windows 10 SDK

Alternatively can use [vs_buildtools.exe][vs_buildtools] to reduce install size.

### [Erlang/OTP 20.3][otp] (optional)

If you don't want to install [Erlang/OTP][otp] manually, the preparation script
will do it automatically for you.

**Download:** [Erlang/OTP 20.3 Windows Installer][otp203_dl]

- Execute installer and follow instructions
- Keep note of install folder

### [Java Development Kit 11][jdk] (optional)

If you don't want to install Open JDK manually, the preparation script will do
it automatically for you.

## Setup

Now the [MSYS2][msys2] environment needs to be prepared. This can be done 
automatically by the helper script `scripts/windows/msys2_prepare.bat`.

This script uses the following environment variable default values. If your
local setup differs, you need to set these variables yourself.

```
ERTS_VERSION=9.3
FORCE_STYRENE_REINSTALL=false
JDK_URL=https://download.java.net/java/GA/jdk11/9/GPL/openjdk-11.0.2_windows-x64_bin.zip
OTP_VERSION=20.3
PLATFORM=x64
WIN_JDK_BASEPATH=C:\Program Files\Java
WIN_JDK_PATH=C:\Program Files\Java\jdk-11.0.2
WIN_OTP_PATH=C:\Program Files\erl9.3
JAVA_VERSIOIN=11.0.2

```

Note: The helper scripts will try to detect where `msys2` is installed.
If this fails, you can set `WIN_MSYS2_ROOT` environment variable with the proper path, i.e.:

```
WIN_MSYS2_ROOT=C:\msys64
```

You can execute the script directly in a `cmd` window.

## Building

Open a [MSYS2][msys2] shell with the proper paths set. 

Use the helper script `scripts/windows/msys2_shell.bat` to do so.

That script uses the following environment variables (defaults):

```
PLATFORM=x64
ERTS_VERSION=9.3
JAVA_VERSION=11.0.2
```

In the opened shell (MinGW64) go into your build directory and build the system like on
any other UNIX system:

```bash
cd PATH_TO_REPO_CHECKOUT
make
```

NOTE: Disk drives are mounted in the root folder (i.e. `C:` is `/c`)

Refer to `docs/build.md` for more information on how to build.

[chocolatey]: https://chocolatey.org/docs/installation#installing-chocolatey
[msys2]: https://www.msys2.org/
[jdk]: https://download.java.net/java/GA/jdk11/9/GPL/openjdk-11.0.2_windows-x64_bin.zip
[msys2_dl]: http://repo.msys2.org/distrib/x86_64/msys2-x86_64-20180531.exe
[otp]: http://www.erlang.org/
[otp203_dl]: http://erlang.org/download/otp_win64_20.3.exe
[vs2017]: https://docs.microsoft.com/en-us/visualstudio/install/install-visual-studio
[vs2019_dl]: https://visualstudio.microsoft.com/downloads/
[vs_buildtools]: https://visualstudio.microsoft.com/downloads/#build-tools-for-visual-studio-2019
