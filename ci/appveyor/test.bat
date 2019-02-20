@echo on
@rem Matrix-driven Appveyor CI test script
@rem Currently only supports MSYS2 builds.
@rem See https://www.appveyor.com/docs/installed-software#mingw-msys-cygwin
@rem Required vars:
@rem    TEST_STEP
@rem    WIN_MSYS2_ROOT
@rem    PLATFORM

SETLOCAL ENABLEEXTENSIONS
cd %APPVEYOR_BUILD_FOLDER%

rem Set required vars defaults
IF "%ERTS_VERSION%"=="" SET "ERTS_VERSION=9.3"
IF "%WIN_MSYS2_ROOT%"=="" SET "WIN_MSYS2_ROOT=C:\msys64"
IF "%PLATFORM%"=="" SET "PLATFORM=x64"
IF "%TEST_STEP%"=="" SET "TEST_STEP=ct"
SET BASH_BIN="%WIN_MSYS2_ROOT%\usr\bin\bash"

@echo Current time: %time%
rem Set the paths appropriately

call "C:\Program Files (x86)\Microsoft Visual Studio\2017\Community\VC\Auxiliary\Build\vcvarsall.bat" %PLATFORM%
@echo on
SET PATH=%WIN_MSYS2_ROOT%\mingw64\bin;%WIN_MSYS2_ROOT%\usr\bin;%PATH%

:TESTSTART
GOTO TEST_%TEST_STEP%

:TEST_ct
@echo Current time: %time%
rem Run test: ct
bash -lc "cd %BUILD_PATH% && epmd -daemon && make ct"

GOTO TEST_DONE

:TEST_eunit
@echo Current time: %time%
rem Run test: eunit
bash -lc "cd %BUILD_PATH% && epmd -daemon && make eunit"

GOTO TEST_DONE

:TEST_
:TEST_DONE

@echo Current time: %time%
rem Finished test phase
