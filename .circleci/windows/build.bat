@echo on
@rem Required vars:
@rem    WIN_MSYS2_ROOT
@rem    WIN_OTP_PATH
@rem    ERTS_VERSION
@rem Target dir for artifacts can be passed as first argument or set in ARTIFACTS_PATH

SETLOCAL ENABLEEXTENSIONS

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

CALL "%~dp0vcvarsall.bat"

@call:log Build production release
"%WIN_MSYS2_ROOT%\usr\bin\bash.exe" -lc "${PROJECT_ROOT}/.circleci/windows/build.sh %PACKAGES_PATH%"
@call:log Build done.

exit /b %ERRORLEVEL%

:log :: Display a log message
@echo :: [1;33m %time% : %* [0m>con && exit /b
