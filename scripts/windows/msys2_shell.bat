:: Pass -v as argument to enable echo
@echo off
@rem Script to open a msys2 shell ready for building.
@rem Required vars:
@rem    PLATFORM
@rem    WIN_MSYS2_ROOT
@rem    WIN_OTP_PATH
@rem    ERTS_VERSION
@rem    JAVA_VERSION

SETLOCAL ENABLEEXTENSIONS

@ECHO."%*" | FINDSTR /C:"-v" > nul && SET VFLAG=-v && echo on

:: Set the required vars defaults
IF "%PLATFORM%"=="" SET "PLATFORM=x64"
IF "%WIN_MSYS2_ROOT%"=="" FOR /F %%F IN ('where msys2') DO SET "WIN_MSYS2_ROOT=%%~dpF"
IF "%ERTS_VERSION%"=="" SET "ERTS_VERSION=9.3"
IF "%JAVA_VERSION%"=="" SET "JAVA_VERSION=11.0.2"
:: Use 'defterm' to avoid opening separate shell window
IF "%MSYSCON%"=="" SET "MSYSCON=defterm"

:: Set the appropriate MSVC_VERSION
IF NOT "%MSVC_VERSION%"=="" GOTO MSVC_VERSION_SET
FOR /F "tokens=* USEBACKQ delims=" %%F IN (`where /r "C:\Program Files (x86)\Microsoft Visual Studio" Microsoft.VCToolsVersion.default.txt`) DO SET /p MSVC_VERSION=<"%%F"
:MSVC_VERSION_SET

:: Find and execute the VS env preparation script
IF NOT "%VCVARSALL%"=="" GOTO VCVARSALLFOUND
FOR /F "tokens=* USEBACKQ delims=" %%F IN (`where /r "C:\Program Files (x86)\Microsoft Visual Studio" vcvarsall`) DO SET "VCVARSALL=%%F"
:VCVARSALLFOUND
call "%VCVARSALL%" %PLATFORM%

:: Set the paths appropriately avoiding duplicates
SET "_PATHS=%WIN_MSYS2_ROOT%\mingw64\bin;%WIN_MSYS2_ROOT%\usr\bin;%WIN_MSYS2_ROOT%"
ECHO."%PATH%" | FINDSTR /V /C:"%_PATHS%">nul && PATH=%_PATHS%;%PATH%
SET "_PATHS="

SET "_PATHS=%WIN_OTP_PATH%\bin;%WIN_OTP_PATH%\erts-%ERTS_VERSION%\bin;%WIN_OTP_PATH%\erts-%ERTS_VERSION%"
ECHO."%PATH%" | FINDSTR /V /C:"%_PATHS%">nul && PATH=%_PATHS%;%PATH%
SET "_PATHS="

echo :: You can set these vars to skip autodetection and speed up bootstrap
echo SET "WIN_MSYS2_ROOT=%WIN_MSYS2_ROOT%"
echo SET "VCVARSALL=%VCVARSALL%"
echo SET MSVC_VERSION=%MSVC_VERSION%
echo SET WIN_OTP_PATH=%WIN_OTP_PATH%
echo SET ERTS_VERSION=%ERTS_VERSION%
echo SET JAVA_VERSION=%JAVA_VERSION%
echo SET PLATFORM=%PLATFORM%
echo SET MSYSCON=%MSYSCON%

:: Open shell

"%WIN_MSYS2_ROOT%\mingw64.exe"
