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
IF "%WIN_MSYS2_ROOT%"=="" FOR /F %%F IN ('where msys2') DO SET "WIN_MSYS2_ROOT=%%~dpF"
IF "%PLATFORM%"=="" SET "PLATFORM=x64"
IF "%BUILD_PATH%"=="" GOTO :PACKAGEABORT_MISSINGBUILDPATH

SET RELEASE_PATH=%BUILD_PATH%/_build/prod/rel/aeternity

@echo Current time: %time%

rem SET the appropriate MSVC_VERSION
IF NOT "%MSVC_VERSION%"=="" GOTO MSVC_VERSION_SET
@FOR /F "tokens=* USEBACKQ delims=" %%F IN (`where /r "C:\Program Files (x86)\Microsoft Visual Studio" Microsoft.VCToolsVersion.default.txt`) DO SET "ToolsVerFile=%%F"
@FOR /F "tokens=* USEBACKQ delims=" %%F IN (`type "%ToolsVerFile%"`) DO SET MSVC_VERSION=%%F
:MSVC_VERSION_SET

rem Find and execute the VS env preparation script
@FOR /F "tokens=* USEBACKQ delims=" %%F IN (`where /r "C:\Program Files (x86)\Microsoft Visual Studio" vcvarsall`) DO SET vcvarsall="%%F"
call %vcvarsall% %PLATFORM%

rem Set the paths appropriately
SET PATH=%WIN_MSYS2_ROOT%\mingw64\bin;%WIN_MSYS2_ROOT%\usr\bin;%PATH%

:PACKAGESTART

@echo Current time: %time%
rem Build production release
bash -lc "cd %BUILD_PATH% && make prod-build"

@echo Current time: %time%
rem Remove erl.ini files from release
bash -lc "find \"%RELEASE_PATH%\" -name erl.ini -type f -delete"

@echo Current time: %time%
rem Remove any previous package environment
bash -lc "rm -rf \"%PACKAGE_PATH%\""

@echo Current time: %time%
rem Build package environment
bash -lc "/mingw64/bin/styrene -o \"%PACKAGE_PATH%\" \"%BUILD_PATH%/ci/appveyor/package.cfg\""

@echo Current time: %time%
rem Copy release into package environment
bash -lc "mkdir -p \"%PACKAGE_PATH%/aeternity-windows-w64/usr/lib\""
bash -lc "cp -R \"%RELEASE_PATH%\" \"%PACKAGE_PATH%/aeternity-windows-w64/usr/lib/\""
bash -lc "mv -f \"%PACKAGE_PATH%/aeternity-windows-w64/usr/lib/aeternity/REVISION\" \"%PACKAGE_PATH%/aeternity-windows-w64/usr/lib/aeternity/VERSION\" \"%PACKAGE_PATH%/aeternity-windows-w64/\""
bash -lc "mkdir -p \"%PACKAGE_PATH%/aeternity-windows-w64/data/aecore\""

@echo Current time: %time%
rem Copy genisis and hard-fork account migrations into top-level data folder
bash -lc "cp -R \"%RELEASE_PATH%/data/aecore/.genesis\" \"%PACKAGE_PATH%/aeternity-windows-w64/data/aecore\""
bash -lc "cp -R \"%RELEASE_PATH%/data/aecore/.minerva\" \"%PACKAGE_PATH%/aeternity-windows-w64/data/aecore\""
bash -lc "cp -R \"%RELEASE_PATH%/data/aecore/.fortuna\" \"%PACKAGE_PATH%/aeternity-windows-w64/data/aecore\""
bash -lc "cp -R \"%RELEASE_PATH%/data/aecore/.lima\" \"%PACKAGE_PATH%/aeternity-windows-w64/data/aecore\""

@echo Current time: %time%
rem Build packages
bash -lc "/mingw64/bin/styrene -o \"%PACKAGE_PATH%\" \"%BUILD_PATH%/ci/appveyor/package.cfg\""

@echo Current time: %time%
rem Copy packages
SET /p PACKAGE_VERSION=<%~dp0%\..\..\REVISION
bash -lc "cp \"%PACKAGE_PATH%\"/*.zip \"%BUILD_PATH%/aeternity-%PACKAGE_VERSION%-windows-x86_64.zip\""
bash -lc "cp \"%PACKAGE_PATH%\"/*.exe \"%BUILD_PATH%/aeternity-%PACKAGE_VERSION%-windows-x86_64.exe\""

:PACKAGEDONE

@echo Current time: %time%
rem Finished package phase
exit /B 0

:PACKAGEABORT_MISSINGBUILDPATH
@echo ERROR: Missing environment variable BUILD_PATH
exit /B 1
