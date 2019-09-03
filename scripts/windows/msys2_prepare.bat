@echo off
:: Pass -v as argument to enable echo
@ECHO."%*" | FINDSTR /C:"-v" > nul && SET VFLAG=-v && echo on & echo
@rem Script to prepare msys2 environment for builds.
@rem Required vars:
@rem    ERTS_VERSION
@rem    FORCE_STYRENE_REINSTALL
@rem    JDK_URL
@rem    OTP_VERSION
@rem    PLATFORM
@rem    WIN_JDK_BASEPATH
@rem    JAVA_VERSION
@rem    WIN_JDK_PATH
@rem    WIN_MSYS2_ROOT
@rem    WIN_OTP_PATH
@rem Set VCVARSALL to the proper location of vcvarsall.bat to skip autodetection
@rem Set MSVC_VERSION to the proper value to skip autodetection

:: Set required vars defaults

IF "%ERTS_VERSION%"=="" SET "ERTS_VERSION=9.3"
IF "%OTP_VERSION%"=="" SET "OTP_VERSION=20.3"
IF "%PLATFORM%"=="" SET "PLATFORM=x64"
IF "%WIN_MSYS2_ROOT%"=="" FOR /F %%F IN ('where msys2') DO SET "WIN_MSYS2_ROOT=%%~dpF"
:: default if autodetect failed
IF "%WIN_MSYS2_ROOT%"=="" SET "WIN_MSYS2_ROOT=C:\tools\msys64"

IF "%FORCE_STYRENE_REINSTALL%"=="" SET "FORCE_STYRENE_REINSTALL=false"
IF "%JDK_URL%"=="" SET "JDK_URL=https://download.java.net/java/GA/jdk11/9/GPL/openjdk-11.0.2_windows-x64_bin.zip"
IF "%WIN_JDK_BASEPATH%"=="" SET "WIN_JDK_BASEPATH=C:\Program Files\Java"
IF "%JAVA_VERSION%"=="" SET "JAVA_VERSION=11.0.2"
IF "%WIN_JDK_PATH%"=="" SET "WIN_JDK_PATH=%WIN_JDK_BASEPATH%\jdk-%JAVA_VERSION%"
IF "%ERLANG_HOME%"=="" SET "ERLANG_HOME=%WIN_OTP_PATH%"

:: Setup MSVC env
@call:log Find and set MSVC_VERSION
call:MSVC_VERSION
IF NOT "%ERRORLEVEL%"=="0" exit /b %ERRORLEVEL%

:: Dump detected version
@echo SET MSVC_VERSION=%MSVC_VERSION%

@call:log Find and execute the VS env preparation script for %PLATFORM%
call:VCVARSALL
IF NOT "%ERRORLEVEL%"=="0" exit /b %ERRORLEVEL%

:: Set the paths appropriately avoiding duplicates
SET "_PATHS=%WIN_MSYS2_ROOT%\mingw64\bin;%WIN_MSYS2_ROOT%\usr\bin;%WIN_MSYS2_ROOT%"
ECHO."%PATH%" | FINDSTR /V /C:"%_PATHS%">nul && SET "PATH=%_PATHS%;%PATH%"
SET "_PATHS="

@call:log Ensure Msys2 is installed in %WIN_MSYS2_ROOT%
call %~dp0install_msys2 %VFLAG% %WIN_MSYS2_ROOT%
IF NOT "%ERRORLEVEL%"=="0" exit /b %ERRORLEVEL%

@call:log Prepare Msys2 env
call:PREPARE_MSYS2

@call:log Ensure Erlang/OTP %OTP_VERSION% is installed in %WIN_OTP_PATH%
call %~dp0install_erlang %VFLAG% %OTP_VERSION% "%WIN_OTP_PATH%"
IF NOT "%ERRORLEVEL%"=="0" exit /b %ERRORLEVEL%

@call:log Ensure JDK is installed in %WIN_JDK_PATH%
call:INSTALL_JDK
IF NOT "%ERRORLEVEL%"=="0" exit /b %ERRORLEVEL%

@call:log Set the paths appropriately
:: Set the paths appropriately avoiding duplicates
SET "_PATHS=%WIN_OTP_PATH%\bin;%WIN_OTP_PATH%\erts-%ERTS_VERSION%\bin;%WIN_OTP_PATH%\erts-%ERTS_VERSION%"
ECHO."%PATH%" | FINDSTR /V /C:"%_PATHS%">nul && SET "PATH=%_PATHS%;%PATH%"
SET "_PATHS="
SET VFLAG=

@call:log Finished preparation

exit /b 0

:: Subroutines

:INSTALL_JDK
IF EXIST "%WIN_JDK_PATH%\bin\" @call:log Ok. && exit /b 0
SET "JDK_PACKAGE=jdk_package.zip"
@call:log Download from %JDK_URL%
PowerShell -Command "(New-Object System.Net.WebClient).DownloadFile(\"%JDK_URL%\", \"%TMP%\%JDK_PACKAGE%\")"
@call:log Install to %WIN_JDK_PATH%
PowerShell -Command "Expand-Archive -LiteralPath \"%TMP%\%JDK_PACKAGE%\" -DestinationPath \"%WIN_JDK_BASEPATH%\""
exit /b %ERRORLEVEL%

:MSVC_VERSION
::locate the path to the version file and import into a var
IF NOT "%MSVC_VERSION%"=="" exit /b 0
FOR /F "tokens=* USEBACKQ delims=" %%F IN (`where /r "C:\Program Files (x86)\Microsoft Visual Studio" Microsoft.VCToolsVersion.default.txt`) DO SET /p MSVC_VERSION=<"%%F" && exit /b 0
exit /b %ERRORLEVEL%

:VCVARSALL
IF NOT "DevEnvDir%"=="" exit /b 0
IF NOT "%VCVARSALL%"=="" GOTO:VCVARSALLFOUND
FOR /F "usebackq delims=" %%F IN (`where /r "C:\Program Files (x86)\Microsoft Visual Studio" vcvarsall`) DO SET "VCVARSALL=%%F"
IF "%VCVARSALL%"=="" @call:log MSVC not installed && exit /b 2
:VCVARSALLFOUND
:: 1>nul to hide the banner
call "%VCVARSALL%" %PLATFORM% 1>nul
exit /b %ERRORLEVEL%

:PREPARE_MSYS2
:: Switch to local ENV
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

%BASH% -lc "pacman -Qdt | grep i686 | awk '{ print $1; }' | xargs pacman --noconfirm -Rsc || true"

@call:log Remove breaking tools

%BASH% -lc "pacman --noconfirm -Rsc %PACMAN_PACKAGES_REMOVE% || true"

@call:log Install required tools
%BASH% -lc "pacman --noconfirm --needed -S %PACMAN_PACKAGES% %PACMAN_PYTHON_PACKAGES%"

@call:log Remove link.exe from msys2, so it does not interfere with MSVC's link.exe

%BASH% -lc "rm -f /bin/link.exe /usr/bin/link.exe"

@call:log Ensure Styrene is installed in %WIN_STYRENE_PATH%
IF EXIST "%WIN_STYRENE_PATH%" IF NOT "%FORCE_STYRENE_REINSTALL%"=="true" @call:log Ok. && exit /b
@call:log Install Styrene
%BASH% -lc "chown -R $USER $HOME/.ssh"
%BASH% -lc "rm -rf \"${ORIGINAL_TEMP}/styrene\""
%BASH% -lc "git clone https://github.com/achadwick/styrene.git \"${ORIGINAL_TEMP}/styrene\""
%BASH% -lc "cd \"${ORIGINAL_TEMP}/styrene\" && git fetch origin && git checkout v0.3.0"
%BASH% -lc "cd \"${ORIGINAL_TEMP}/styrene\" && %PIP% uninstall -y styrene"
%BASH% -lc "cd \"${ORIGINAL_TEMP}/styrene\" && %PIP% install ."

:: Revert ENV changes
ENDLOCAL

exit /b

:log :: Display a log message
@echo :: [1;33m %time% : %* [0m>con && exit /b
