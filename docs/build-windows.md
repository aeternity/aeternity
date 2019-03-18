# Build from source

This document describes how to build an Aeternity node from source on Windows using
[MSYS2][msys2].

NOTE: Only 64-bit versions of Windows 10 and Windows Server 2016 are supported and tested.

## Dependencies

### [MSYS2][msys2]

**Download:** [MSYS2 Windows Installer][msys2_dl]

- Execute installer and follow instructions
- Keep note of install folder

### [Visual Studio 2017][vs2017]

Install Vistual Studio 2017 and make sure to include the following components:

- Command-line compiler support
- Windows OS Kit 10

### [Erlang/OTP 20.3][otp] (optional)

If you don't want to install [Erlang/OTP][otp] manually, the preparation script will do it automatically for you.

**Download:** [Erlang/OTP 20.3 Windows Installer][otp203_dl]

- Execute installer and follow instructions
- Keep note of install folder

## Setup

Now the [MSYS2][msys2] environment needs to be prepared. This can be done
automatically through the helper script `scripts/windows/msys2_prepare.bat`.
This script uses the following environment variable default values. If your
local setup differs, you need to set these variables yourself.

```
ERTS_VERSION=9.3
OTP_VERSION=20.3
WIN_OTP_PATH=C:\Program Files\erl
WIN_MSYS2_ROOT=C:\msys64
PLATFORM=x64
```

You can execute the script directly in a `cmd` window.

## Building

Open a [MSYS2][msys2] shell with the proper paths set. You can use the helper
script `scripts/windows/msys2_shell.bat` to do so.

That script uses the following environment variables:

```
WIN_MSYS2_ROOT=C:\msys64
PLATFORM=x64
```

In the opened shell go into your build directory and build the system like on
any other UNIX system:

```bash
cd PATH_TO_REPO_CHECKOUT
make
```

Refer to `docs/build.md` for more information on how to build.

[msys2]: https://www.msys2.org/
[msys2_dl]: http://repo.msys2.org/distrib/x86_64/msys2-x86_64-20180531.exe
[otp]: http://www.erlang.org/
[otp203_dl]: http://erlang.org/download/otp_win64_20.3.exe
[vs2017]: https://docs.microsoft.com/en-us/visualstudio/install/install-visual-studio
