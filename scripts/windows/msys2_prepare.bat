@echo on
@rem Script to prepare msys2 environment for builds.
@rem Required vars:
@rem    ERTS_VERSION
@rem    OTP_VERSION
@rem    OTP_PATH
@rem    MSYS2_ROOT
@rem    PLATFORM

SETLOCAL ENABLEEXTENSIONS

rem Set required vars defaults
IF "%ERTS_VERSION%"=="" SET "ERTS_VERSION=9.3"
IF "%OTP_VERSION%"=="" SET "OTP_VERSION=20.3"
IF "%OTP_PATH%"=="" SET "OTP_PATH=C:\Program Files\erl"
IF "%MSYS2_ROOT%"=="" SET "MSYS2_ROOT=C:\msys64"
IF "%PLATFORM%"=="" SET "PLATFORM=x64"

@echo Current time: %time%
rem Set the paths appropriately

call "C:\Program Files (x86)\Microsoft Visual Studio\2017\Community\VC\Auxiliary\Build\\vcvarsall.bat" %PLATFORM%
@echo on
SET PATH=%MSYS2_ROOT%\mingw64\bin;%MSYS2_ROOT%\usr\bin;%PATH%

@echo Current time: %time%
rem Upgrade the MSYS2 platform

bash -lc "pacman --noconfirm --needed -Sy pacman"
bash -lc "pacman --noconfirm -Su"

@echo Current time: %time%
rem Install required tools

bash -lc "pacman --noconfirm --needed -S base-devel cmake curl gcc gcc-fortran git make patch"
bash -lc "pacman --noconfirm --needed -S mingw-w64-x86_64-SDL mingw-w64-x86_64-libsodium mingw-w64-x86_64-ntldd-git mingw-w64-x86_64-yasm"
bash -lc "pacman --noconfirm --needed -S mingw-w64-x86_64-gcc mingw-w64-x86_64-gcc-ada mingw-w64-x86_64-gcc-fortran mingw-w64-x86_64-gcc-objc"

@echo Current time: %time%
rem Ensure Erlang/OTP %OTP_VERSION% is installed

IF EXIST "%OTP_PATH%%ERTS_VERSION%\bin\" GOTO OTPINSTALLED
SET OTP_PACKAGE=otp_win64_%OTP_VERSION%.exe
PowerShell -Command "Invoke-WebRequest http://erlang.org/download/%OTP_PACKAGE% -OutFile %TMP%\%OTP_PACKAGE%"
START "" /WAIT "%TMP%\%OTP_PACKAGE%" /S
:OTPINSTALLED

@echo Current time: %time%
rem Set up msys2 env variables

COPY msys2_env_build.sh %MSYS2_ROOT%\etc\profile.d\env_build.sh

@echo Current time: %time%
rem Remove link.exe from msys2, so it does not interfere with MSVC's link.exe

bash -lc "rm -f /bin/link.exe /usr/bin/link.exe"

@echo Current time: %time%
rem Finished preparation
