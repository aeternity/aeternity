@echo on
@rem Script to prepare msys2 environment for builds.
@rem Required vars:
@rem    ERTS_VERSION
@rem    FORCE_STYRENE_REINSTALL
@rem    JDK_URL
@rem    OTP_VERSION
@rem    PLATFORM
@rem    WIN_JDK_BASEPATH
@rem    WIN_JDK_PATH
@rem    WIN_MSYS2_ROOT
@rem    WIN_OTP_PATH

SETLOCAL ENABLEEXTENSIONS

rem Set required vars defaults

IF "%ERTS_VERSION%"=="" SET "ERTS_VERSION=9.3"
IF "%FORCE_STYRENE_REINSTALL%"=="" SET "FORCE_STYRENE_REINSTALL=false"
IF "%JDK_URL%"=="" SET "JDK_URL=https://download.java.net/java/GA/jdk11/9/GPL/openjdk-11.0.2_windows-x64_bin.zip"
IF "%OTP_VERSION%"=="" SET "OTP_VERSION=20.3"
IF "%PLATFORM%"=="" SET "PLATFORM=x64"
IF "%WIN_JDK_BASEPATH%"=="" SET "WIN_JDK_BASEPATH=C:\Program Files\Java"
IF "%WIN_JDK_PATH%"=="" SET "WIN_JDK_PATH=C:\Program Files\Java\jdk11"
IF "%WIN_MSYS2_ROOT%"=="" SET "WIN_MSYS2_ROOT=C:\msys64"
IF "%WIN_OTP_PATH%"=="" SET "WIN_OTP_PATH=C:\Program Files\erl"

SET "BASH_BIN=%WIN_MSYS2_ROOT%\usr\bin\bash"
SET "PACMAN=pacman --noconfirm --needed -S"
SET "PACMAN_RM=pacman --noconfirm -Rsc"
SET "PIP=/mingw64/bin/pip3"
SET "WIN_STYRENE_PATH=%TMP%\styrene"

SET PACMAN_PACKAGES=base-devel ^
cmake ^
curl ^
gcc ^
isl ^
git ^
libcurl ^
libopenssl ^
make ^
mingw-w64-x86_64-SDL ^
mingw-w64-x86_64-binutils ^
mingw-w64-x86_64-gcc ^
mingw-w64-x86_64-libsodium ^
mingw-w64-x86_64-nsis ^
mingw-w64-x86_64-ntldd-git  ^
mingw-w64-x86_64-yasm ^
patch ^
unzip ^
zip

SET PACMAN_PACKAGES_REMOVE=gcc-fortran ^
mingw-w64-x86_64-gcc-ada ^
mingw-w64-x86_64-gcc-fortran ^
mingw-w64-x86_64-gcc-libgfortran ^
mingw-w64-x86_64-gcc-objc

:: WINDOWS_PYTHON
:: These package are dependencies for the release tests as originally defined in py/requirements.txt
:: On Windows/msys2 we need to install these via the OS package manager though.
SET PACMAN_PYTHON_PACKAGES=mingw-w64-x86_64-python3-nose ^
mingw-w64-x86_64-python3-pip ^
mingw-w64-x86_64-python3-pynacl ^
mingw-w64-x86_64-python3-yaml

@echo Current time: %time%
rem Set the paths appropriately

call "C:\Program Files (x86)\Microsoft Visual Studio\2017\Community\VC\Auxiliary\Build\\vcvarsall.bat" %PLATFORM%
@echo on
SET "PATH=%WIN_MSYS2_ROOT%\mingw64\bin;%WIN_MSYS2_ROOT%\usr\bin;%PATH%"

@echo Current time: %time%
rem Set up msys2 env variables

COPY %~dp0\msys2_env_build.sh %WIN_MSYS2_ROOT%\etc\profile.d\env_build.sh

@echo Current time: %time%
rem Remove 32-bit tools

%BASH_BIN% -lc "pacman -Qet | grep i686 | awk '{ print $1; }' | xargs %PACMAN_RM% || true"
%BASH_BIN% -lc "pacman -Qdt | grep i686 | awk '{ print $1; }' | xargs %PACMAN_RM% || true"

@echo Current time: %time%
rem Remove breaking tools

%BASH_BIN% -lc "%PACMAN_RM% %PACMAN_PACKAGES_REMOVE% || true"

@echo Current time: %time%
rem Upgrade the MSYS2 platform

%BASH_BIN% -lc "%PACMAN% -yuu"

@echo Current time: %time%
rem Install required tools
%BASH_BIN% -lc "%PACMAN% %PACMAN_PACKAGES% %PACMAN_PYTHON_PACKAGES%"

@echo Current time: %time%
rem Ensure Erlang/OTP %OTP_VERSION% is installed

IF EXIST "%WIN_OTP_PATH%%ERTS_VERSION%\bin\" GOTO OTPINSTALLED
SET "OTP_PACKAGE=otp_win64_%OTP_VERSION%.exe"
SET "OTP_URL=http://erlang.org/download/%OTP_PACKAGE%"
PowerShell -Command "(New-Object System.Net.WebClient).DownloadFile(\"%OTP_URL%\", \"%TMP%\%OTP_PACKAGE%\")"
START "" /WAIT "%TMP%\%OTP_PACKAGE%" /S
:OTPINSTALLED

@echo Current time: %time%
rem Ensure JDK is installed

IF EXIST "%WIN_JDK_PATH%\bin\" GOTO JDKINSTALLED
SET "JDK_PACKAGE=jdk_package.zip"
PowerShell -Command "(New-Object System.Net.WebClient).DownloadFile(\"%JDK_URL%\", \"%TMP%\%JDK_PACKAGE%\")"
PowerShell -Command "Expand-Archive -LiteralPath \"%TMP%\%JDK_PACKAGE%\" -DestinationPath \"%WIN_JDK_BASEPATH%\""
:JDKINSTALLED

@echo Current time: %time%
rem Ensure Styrene is installed

IF EXIST "%WIN_STYRENE_PATH%" IF "%FORCE_STYRENE_REINSTALL%" NEQ "true" GOTO STYRENEINSTALLED
%BASH_BIN% -lc "rm -rf \"${ORIGINAL_TEMP}/styrene\""
%BASH_BIN% -lc "git clone https://github.com/achadwick/styrene.git \"${ORIGINAL_TEMP}/styrene\""
%BASH_BIN% -lc "cd \"${ORIGINAL_TEMP}/styrene\" && git fetch origin && git checkout v0.3.0"
%BASH_BIN% -lc "cd \"${ORIGINAL_TEMP}/styrene\" && %PIP% uninstall -y styrene"
%BASH_BIN% -lc "cd \"${ORIGINAL_TEMP}/styrene\" && %PIP% install ."
:STYRENEINSTALLED

@echo Current time: %time%
rem Remove link.exe from msys2, so it does not interfere with MSVC's link.exe

%BASH_BIN% -lc "rm -f /bin/link.exe /usr/bin/link.exe"

@echo Current time: %time%
rem Finished preparation
