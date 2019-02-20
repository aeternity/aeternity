@echo on
@rem Matrix-driven Appveyor CI build script
@rem Currently only supports MSYS2 builds.
@rem See https://www.appveyor.com/docs/installed-software#mingw-msys-cygwin
@rem Required vars:
@rem    BUILD_STEP
@rem    WIN_MSYS2_ROOT
@rem    PLATFORM

SETLOCAL ENABLEEXTENSIONS
cd %APPVEYOR_BUILD_FOLDER%

rem Set required vars defaults
IF "%ERTS_VERSION%"=="" SET "ERTS_VERSION=9.3"
IF "%WIN_MSYS2_ROOT%"=="" SET "WIN_MSYS2_ROOT=C:\msys64"
IF "%PLATFORM%"=="" SET "PLATFORM=x64"
IF "%BUILD_STEP%"=="" SET "BUILD_STEP=build"
SET BASH_BIN="%WIN_MSYS2_ROOT%\usr\bin\bash"

@echo Current time: %time%
rem Set the paths appropriately

call "C:\Program Files (x86)\Microsoft Visual Studio\2017\Community\VC\Auxiliary\Build\vcvarsall.bat" %PLATFORM%
@echo on
SET PATH=%WIN_MSYS2_ROOT%\mingw64\bin;%WIN_MSYS2_ROOT%\usr\bin;%PATH%

:BUILDSTART
GOTO BUILD_%BUILD_STEP%

:BUILD_build
@echo Current time: %time%
rem Run build: build
%BASH_BIN% -lc "cd %BUILD_PATH% && make KIND=test local-build"

GOTO BUILD_DONE

:BUILD_
:BUILD_DONE

@echo Current time: %time%
rem Finished build phase
