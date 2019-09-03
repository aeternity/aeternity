@echo on
@rem Required vars:
@rem    WIN_OTP_PATH
@rem    WIN_MSYS2_ROOT
@rem    ERTS_VERSION
@rem    PACKAGE_TESTS_DIR
@rem    PACKAGE_ZIPARCHIVE
@rem    TEST_STEPS
@rem    PROJECT_ROOT

SETLOCAL ENABLEEXTENSIONS
@call:log Set the paths appropriately

:: Appropriately set paths
SET "PATH=%WIN_MSYS2_ROOT%\mingw64\bin;%WIN_MSYS2_ROOT%\usr\bin;%WIN_MSYS2_ROOT%;%PATH%"
SET "PATH=%WIN_OTP_PATH%\bin;%WIN_OTP_PATH%\erts-%ERTS_VERSION%\bin;%WIN_OTP_PATH%\erts-%ERTS_VERSION%;%PATH%"

:: Construct unix paths
FOR /f %%i IN ('cygpath -a %~dp0..\..') DO SET "PROJECT_ROOT=%%i"
:: remove trailing /
SET "PROJECT_ROOT=%PROJECT_ROOT:~0,-1%"

IF "%ARTIFACTS_PATH%"=="" SET "ARTIFACTS_PATH=%~1"
IF "%ARTIFACTS_PATH%"=="" SET "ARTIFACTS_PATH=%~dp0..\..\packages"
IF NOT "%ARTIFACTS_PATH%"=="" FOR /f %%i IN ('cygpath -a %ARTIFACTS_PATH%') DO SET "PACKAGES_PATH=%%i"

IF "%PACKAGES_PATH%"=="" SET "PACKAGES_PATH=%PROJECT_ROOT%/packages"

SET BASH="%WIN_MSYS2_ROOT%\usr\bin\bash.exe"

%BASH% -lc "cd ${PROJECT_ROOT} && make REVISION"

SET /p PACKAGE_VERSION=<%~dp0..\..\REVISION

:: Set required vars defaults

IF "%ERTS_VERSION%"=="" SET "ERTS_VERSION=9.3"
IF "%PACKAGE_TESTS_DIR%"=="" SET "PACKAGE_TESTS_DIR=/tmp/package_tests"
IF "%PACKAGE_ZIPARCHIVE%"=="" SET "PACKAGE_ZIPARCHIVE=aeternity-%PACKAGE_VERSION%-windows-x86_64.zip"
IF "%TEST_STEPS%"=="" SET "TEST_STEPS=release"

@for /f "tokens=1* delims=, " %%i in ("%TEST_STEPS%") do @(
    @call:log Run test %%i
	call :TEST_%%i
)

@call:log Finished test phase
exit /b %ERRORLEVEL%

:: Subroutines
:TEST_ct
%BASH% -lc "cd ${PROJECT_ROOT} && epmd -daemon && make ct"
exit /b %ERRORLEVEL%

:TEST_eunit
%BASH% -lc "cd ${PROJECT_ROOT} && epmd -daemon && make eunit"
exit /b %ERRORLEVEL%

:TEST_release
%BASH% -lc ^"cd ${PROJECT_ROOT} ^&^& ^
           make python-env PIP=/mingw64/bin/pip3 ^&^& ^
           rm -rf ${PACKAGE_TESTS_DIR} ^&^& ^
           mkdir -p ${PACKAGE_TESTS_DIR} ^&^& ^
           make python-release-test WORKDIR=${PACKAGE_TESTS_DIR} PACKAGE=${PACKAGES_PATH}/${PACKAGE_ZIPARCHIVE} PYTHON=/mingw64/bin/python3 ^&^& ^
           echo done. ^
           ^"
exit /b %ERRORLEVEL%

:log :: Display a log message
@echo :: [1;33m %time% : %* [0m>con && exit /b
