@echo on
@rem Matrix-driven Appveyor CI package script
@rem Currently only supports MSYS2 builds.
@rem See https://www.appveyor.com/docs/installed-software#mingw-msys-cygwin
@rem Required vars:
@rem    WIN_MSYS2_ROOT
@rem    PLATFORM
@rem    PACKAGE_PATH

SETLOCAL ENABLEEXTENSIONS

rem Set required vars defaults
IF "%PACKAGE_PATH%"=="" SET "PACKAGE_PATH=/tmp/win_package_build"
IF "%WIN_MSYS2_ROOT%"=="" SET "WIN_MSYS2_ROOT=C:\msys64"
IF "%PLATFORM%"=="" SET "PLATFORM=x64"
SET BASH_BIN="%WIN_MSYS2_ROOT%\usr\bin\bash"
SET RELEASE_PATH=%BUILD_PATH%/_build/prod/rel/aeternity

@echo Current time: %time%
rem Set the paths appropriately

call "C:\Program Files (x86)\Microsoft Visual Studio\2017\Community\VC\Auxiliary\Build\vcvarsall.bat" %PLATFORM%
@echo on
SET PATH=%WIN_MSYS2_ROOT%\mingw64\bin;%WIN_MSYS2_ROOT%\usr\bin;%PATH%

:PACKAGESTART

@echo Current time: %time%
rem Build production release
%BASH_BIN% -lc "cd %BUILD_PATH% && make prod-build"

@echo Current time: %time%
rem Remove erl.ini files from release
%BASH_BIN% -lc "find \"%RELEASE_PATH%\" -name erl.ini -type f -delete"

@echo Current time: %time%
rem Remove any previous package environment
%BASH_BIN% -lc "rm -rf \"%PACKAGE_PATH%\""

@echo Current time: %time%
rem Build package environment
%BASH_BIN% -lc "/mingw64/bin/styrene -o \"%PACKAGE_PATH%\" \"%BUILD_PATH%/ci/appveyor/package.cfg\""

@echo Current time: %time%
rem Copy release into package environment
%BASH_BIN% -lc "mkdir -p \"%PACKAGE_PATH%/aeternity-windows-w64/usr/lib\""
%BASH_BIN% -lc "cp -R \"%RELEASE_PATH%\" \"%PACKAGE_PATH%/aeternity-windows-w64/usr/lib/\""
%BASH_BIN% -lc "mkdir -p \"%PACKAGE_PATH%/aeternity-windows-w64/data/aecore\""
%BASH_BIN% -lc "cp -R \"%RELEASE_PATH%/data/aecore/.genesis\" \"%PACKAGE_PATH%/aeternity-windows-w64/data/aecore\""

@echo Current time: %time%
rem Build packages
%BASH_BIN% -lc "/mingw64/bin/styrene -o \"%PACKAGE_PATH%\" \"%BUILD_PATH%/ci/appveyor/package.cfg\""

@echo Current time: %time%
rem Copy packages
SET /p PACKAGE_VERSION=<%~dp0%\..\..\REVISION
%BASH_BIN% -lc "cp \"%PACKAGE_PATH%\"/*.zip \"%BUILD_PATH%/aeternity-%PACKAGE_VERSION%-windows-x86_64.zip\""
%BASH_BIN% -lc "cp \"%PACKAGE_PATH%\"/*.exe \"%BUILD_PATH%/aeternity-%PACKAGE_VERSION%-windows-x86_64.exe\""

:PACKAGEDONE

@echo Current time: %time%
rem Finished package phase
