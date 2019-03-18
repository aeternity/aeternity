@echo on
@rem Script to prepare msys2 environment for builds.
@rem Required vars:
@rem    ERTS_VERSION
@rem    OTP_VERSION
@rem    WIN_OTP_PATH
@rem    WIN_MSYS2_ROOT
@rem    PLATFORM

SETLOCAL ENABLEEXTENSIONS

rem Set required vars defaults
IF "%ERTS_VERSION%"=="" SET "ERTS_VERSION=9.3"
IF "%OTP_VERSION%"=="" SET "OTP_VERSION=20.3"
IF "%WIN_OTP_PATH%"=="" SET "WIN_OTP_PATH=C:\Program Files\erl"
IF "%WIN_MSYS2_ROOT%"=="" SET "WIN_MSYS2_ROOT=C:\msys64"
IF "%PLATFORM%"=="" SET "PLATFORM=x64"
SET BASH_BIN="%WIN_MSYS2_ROOT%\usr\bin\bash"
SET PACMAN=pacman --noconfirm --needed -S
SET PACMAN_RM=pacman --noconfirm -Rsc
SET WIN_STYRENE_PATH=%TMP%\styrene

SET PACMAN_PACKAGES=base-devel ^
cmake ^
curl ^
gcc ^
git ^
make ^
mingw-w64-i686-binutils ^
mingw-w64-i686-gcc ^
mingw-w64-i686-nsis ^
mingw-w64-i686-python3 ^
mingw-w64-i686-python3-pip ^
mingw-w64-x86_64-SDL ^
mingw-w64-x86_64-binutils ^
mingw-w64-x86_64-gcc ^
mingw-w64-x86_64-libsodium ^
mingw-w64-x86_64-nsis ^
mingw-w64-x86_64-ntldd-git  ^
mingw-w64-x86_64-python3 ^
mingw-w64-x86_64-python3-pip ^
mingw-w64-x86_64-yasm ^
patch ^
zip

SET PACMAN_PACKAGES_REMOVE=gcc-fortran ^
mingw-w64-i686-gcc-ada ^
mingw-w64-i686-gcc-fortran ^
mingw-w64-i686-gcc-libgfortran ^
mingw-w64-i686-gcc-objc ^
mingw-w64-x86_64-gcc-ada ^
mingw-w64-x86_64-gcc-fortran ^
mingw-w64-x86_64-gcc-libgfortran ^
mingw-w64-x86_64-gcc-objc

@echo Current time: %time%
rem Set the paths appropriately

call "C:\Program Files (x86)\Microsoft Visual Studio\2017\Community\VC\Auxiliary\Build\\vcvarsall.bat" %PLATFORM%
@echo on
SET PATH=%WIN_MSYS2_ROOT%\mingw64\bin;%WIN_MSYS2_ROOT%\usr\bin;%PATH%

@echo Current time: %time%
rem Upgrade the MSYS2 platform

%BASH_BIN% -lc "%PACMAN% -y pacman"
@echo Current time: %time%
%BASH_BIN% -lc "%PACMAN% -u"

@echo Current time: %time%
rem Remove breaking tools

%BASH_BIN% -lc "%PACMAN_RM% %PACMAN_PACKAGES_REMOVE% || true"

@echo Current time: %time%
rem Install required tools

%BASH_BIN% -lc "%PACMAN% %PACMAN_PACKAGES%"

@echo Current time: %time%
rem Ensure Erlang/OTP %OTP_VERSION% is installed

IF EXIST "%WIN_OTP_PATH%%ERTS_VERSION%\bin\" GOTO OTPINSTALLED
SET OTP_PACKAGE=otp_win64_%OTP_VERSION%.exe
PowerShell -Command "Invoke-WebRequest http://erlang.org/download/%OTP_PACKAGE% -OutFile %TMP%\%OTP_PACKAGE%"
START "" /WAIT "%TMP%\%OTP_PACKAGE%" /S
:OTPINSTALLED

@echo Current time: %time%
rem Ensure Styrene is installed

IF EXIST "%WIN_STYRENE_PATH%" GOTO STYRENEINSTALLED
%BASH_BIN% -lc "git clone https://github.com/achadwick/styrene.git \"${ORIGINAL_TEMP}/styrene\""
%BASH_BIN% -lc "cd \"${ORIGINAL_TEMP}/styrene\" && git fetch origin && git checkout v0.3.0"
%BASH_BIN% -lc "cd \"${ORIGINAL_TEMP}/styrene\" && /mingw64/bin/pip3 install ."
:STYRENEINSTALLED

@echo Current time: %time%
rem Set up msys2 env variables

COPY %~dp0\msys2_env_build.sh %WIN_MSYS2_ROOT%\etc\profile.d\env_build.sh

@echo Current time: %time%
rem Remove link.exe from msys2, so it does not interfere with MSVC's link.exe

%BASH_BIN% -lc "rm -f /bin/link.exe /usr/bin/link.exe"

@echo Current time: %time%
rem Finished preparation
