@echo off
:: Pass -v as an argument to enable echo
@rem Script to prepare msys2 environment for builds.
@rem Required vars:
@rem    WIN_MSYS2_ROOT
@rem    WIN_OTP_PATH
@rem Optional vars:
@rem    OTP_VERSION
@rem    ERTS_VERSION
@rem    FORCE_STYRENE_REINSTALL
@rem Set VCVARSALL to the proper location of vcvarsall.bat to skip autodetection
@rem Set MSVC_VERSION to the proper value to skip autodetection

@echo."%*" | findstr /C:"-v" >nul && SET VFLAG=-v && echo on & echo

:: Set required vars defaults

:: Auto-detect OTP_VERSION from the installation in WIN_OTP_PATH
IF "%OTP_VERSION%"=="" IF NOT "%WIN_OTP_PATH%"=="" call:get_otp_version OTP_VERSION ERTS_VERSION
:: default if autodetect failed
IF "%ERTS_VERSION%"=="" SET "ERTS_VERSION=10.3"
IF "%OTP_VERSION%"=="" SET "OTP_VERSION=21.3"
IF "%WIN_OTP_PATH%"=="" SET "WIN_OTP_PATH=C:\tools\erl%OTP_VERSION%"
IF "%PLATFORM%"=="" SET "PLATFORM=x64"
IF "%WIN_MSYS2_ROOT%"=="" FOR /F %%F IN ('where msys2') DO SET "WIN_MSYS2_ROOT=%%~dpF"
:: default if autodetect failed
IF "%WIN_MSYS2_ROOT%"=="" SET "WIN_MSYS2_ROOT=C:\tools\msys64"

IF "%ERLANG_HOME%"=="" SET "ERLANG_HOME=%WIN_OTP_PATH%"

:: Setup MSVC env
@call:log Find and set MSVC_VERSION
call:MSVC_VERSION
IF NOT "%ERRORLEVEL%"=="0" exit /b %ERRORLEVEL%

:: Dump detected version
@echo SET MSVC_VERSION=%MSVC_VERSION%

@call:log Find and execute the VS env preparation script for %PLATFORM%
call:VCVARSALL || exit /b %ERRORLEVEL%

:: Set the paths appropriately avoiding duplicates
SET "_PATHS=%WIN_MSYS2_ROOT%\mingw64\bin;%WIN_MSYS2_ROOT%\usr\bin;%WIN_MSYS2_ROOT%"
path | findstr "%_PATHS%">nul || SET "PATH=%_PATHS%;%PATH%"

@call:log Ensure Msys2 is installed in %WIN_MSYS2_ROOT%
call %~dp0install_msys2 %VFLAG% %WIN_MSYS2_ROOT% || exit /b %ERRORLEVEL%

@call:log Prepare Msys2 env
call:PREPARE_MSYS2

@call:log Ensure Erlang/OTP %OTP_VERSION% is installed in %WIN_OTP_PATH%
call:INSTALL_OTP

@call:log Set the paths appropriately
:: Set the paths appropriately avoiding duplicates
SET "_PATHS=%WIN_OTP_PATH%\bin;%WIN_OTP_PATH%\erts-%ERTS_VERSION%\bin;%WIN_OTP_PATH%\erts-%ERTS_VERSION%"
path | findstr "%_PATHS%">nul || SET "PATH=%_PATHS%;%PATH%"
SET "_PATHS="
SET VFLAG=

@call:log Finished preparation

exit /b 0

:: Subroutines

:INSTALL_OTP
:: Detect VARS from existing OTP installation
SETLOCAL
call:get_otp_version OTP_DETECTED ERTS_DETECTED
:: Check if versions match
IF "%OTP_DETECTED%"=="%OTP_VERSION%" @call:log Ok. && exit /b 0
:: Not installed or version is different force reinstall
call %~dp0install_erlang %VFLAG% %OTP_VERSION% "%WIN_OTP_PATH%"
ENDLOCAL
:: Update version vars
call:get_otp_version OTP_VERSION ERTS_VERSION
exit /b %ERRORLEVEL%

:MSVC_VERSION
::locate the path to the version file and import into a var
IF NOT "%MSVC_VERSION%"=="" exit /b 0
FOR /F "tokens=* USEBACKQ delims=" %%F IN (`where /r "%ProgramFiles(x86)%\Microsoft Visual Studio" Microsoft.VCToolsVersion.default.txt`) DO SET /p MSVC_VERSION=<"%%F" && exit /b 0
exit /b %ERRORLEVEL%

:VCVARSALL
IF NOT "%DevEnvDir%"=="" exit /b 0
IF NOT "%VCVARSALL%"=="" GOTO:VCVARSALLFOUND
FOR /F "usebackq delims=" %%F IN (`where /r "%ProgramFiles(x86)%\Microsoft Visual Studio" vcvarsall`) DO SET "VCVARSALL=%%F"
IF "%VCVARSALL%"=="" @call:log MSVC not installed && exit /b 2
:VCVARSALLFOUND
:: 1>nul to hide the banner
call "%VCVARSALL%" %PLATFORM% 1>nul
exit /b %ERRORLEVEL%

:PREPARE_MSYS2
SETLOCAL ENABLEEXTENSIONS

SET "PIP=/mingw64/bin/pip3"
SET "WIN_STYRENE_PATH=%WIN_MSYS2_ROOT%\mingw64\bin\styrene"

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

@call:log Set up msys2 env variables
:: 1>nul to hide the Copied 1 file message
COPY %~dp0msys2_env_build.sh %WIN_MSYS2_ROOT%\etc\profile.d\env_build.sh 1>nul

SET BASH="%WIN_MSYS2_ROOT%\usr\bin\bash.exe"

@call:log Remove 32-bit tools

%BASH% -lc "pacman -Qdt > /dev/null | grep i686 | awk '{ print $1; }' | xargs pacman --noconfirm -Rsc || true"

@call:log Remove breaking tools

%BASH% -lc "pacman --noconfirm -Rsc %PACMAN_PACKAGES_REMOVE% > /dev/null || true"

@call:log Install required tools
%BASH% -lc "pacman --noconfirm --needed -S %PACMAN_PACKAGES% %PACMAN_PYTHON_PACKAGES%"

@call:log Remove link.exe from msys2, so it does not interfere with MSVC's link.exe

%BASH% -lc "rm -f /bin/link.exe /usr/bin/link.exe"

@call:log Fix home path resolution in msys2

%BASH% -lc "sed -i -e 's/^db_home:.*/db_home: windows/g' /etc/nsswitch.conf"

@call:log Ensure Styrene is installed in %WIN_STYRENE_PATH%
IF EXIST "%WIN_STYRENE_PATH%" IF NOT "%FORCE_STYRENE_REINSTALL%"=="true" @call:log Ok. && exit /b
@call:log Install Styrene
%BASH% -lc "chown -R $USER $HOME/.ssh"
%BASH% -lc "rm -rf \"${ORIGINAL_TEMP}/styrene\""
%BASH% -lc "git clone https://github.com/achadwick/styrene.git \"${ORIGINAL_TEMP}/styrene\""
%BASH% -lc "cd \"${ORIGINAL_TEMP}/styrene\" && git fetch origin && git checkout v0.3.0"
%BASH% -lc "cd \"${ORIGINAL_TEMP}/styrene\" && %PIP% uninstall -y styrene"
%BASH% -lc "cd \"${ORIGINAL_TEMP}/styrene\" && %PIP% install ."

ENDLOCAL

exit /b

:: Detect the versions of an existing OTP/ERTS installed in WIN_OTP_PATH and update the arguments passed by reference
:get_otp_version ::OTP_VERSION ERTS_VERSION
IF NOT EXIST "%WIN_OTP_PATH%\Install.ini" exit /b
FOR /F "tokens=* skip=1 USEBACKQ" %%F IN (`type "%WIN_OTP_PATH%\Install.ini"`) DO SET "_%%F"
SET "_OTP_RELEASE_FILE=%WIN_OTP_PATH%\releases\%_SYSTEM_VSN%\OTP_VERSION"
IF EXIST "%_OTP_RELEASE_FILE%" SET /p _OTP_VERSION= 0<"%_OTP_RELEASE_FILE%" 2>nul
SET "%~1=%_OTP_VERSION%"
SET "%~2=%_VSN%"
SET _OTP_VERSION=
SET _SYSTEM_VSN=
SET _VSN=
SET _OTP_RELEASE_FILE=
exit /b

:log :: Display a log message
@echo :: [1;33m %time% : %* [0m>con && exit /b
