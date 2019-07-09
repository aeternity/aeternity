@echo off
@rem Script to open a msys2 shell ready for building.
@rem Required vars:
@rem    PLATFORM
@rem    WIN_MSYS2_ROOT
@rem    ERTS_VERSION
@rem    JAVA_VERSION

SETLOCAL ENABLEEXTENSIONS

rem Set required vars defaults
IF "%PLATFORM%"=="" SET "PLATFORM=x64"
IF "%WIN_MSYS2_ROOT%"=="" FOR /F %%F IN ('where msys2') DO SET "WIN_MSYS2_ROOT=%%~dpF"
IF "%ERTS_VERSION%"=="" SET "ERTS_VERSION=9.3"
IF "%JAVA_VERSION%"=="" SET "JAVA_VERSION=11.0.2"

echo Current time: %time%

rem SET the appropriate MSVC_VERSION
IF NOT "%MSVC_VERSION%"=="" GOTO MSVC_VERSION_SET
FOR /F "tokens=* USEBACKQ delims=" %%F IN (`where /r "C:\Program Files (x86)\Microsoft Visual Studio" Microsoft.VCToolsVersion.default.txt`) DO SET "ToolsVerFile=%%F"
FOR /F "tokens=* USEBACKQ delims=" %%F IN (`type "%ToolsVerFile%"`) DO SET MSVC_VERSION=%%F
:MSVC_VERSION_SET

rem Find and execute the VS env preparation script
IF NOT "%vcvarsall%"=="" GOTO VCVARSALLFOUND
FOR /F "tokens=* USEBACKQ delims=" %%F IN (`where /r "C:\Program Files (x86)\Microsoft Visual Studio" vcvarsall`) DO SET vcvarsall="%%F"
:VCVARSALLFOUND
call %vcvarsall% %PLATFORM%

echo MSVC_VERSION=%MSVC_VERSION%
echo ERTS_VERSION=%ERTS_VERSION%
echo JAVA_VERSION=%JAVA_VERSION%
echo PLATFORM=%PLATFORM%
echo WIN_MSYS2_ROOT=%WIN_MSYS2_ROOT%

rem Set the paths appropriately
SET "PATH=%WIN_MSYS2_ROOT%\mingw64\bin;%WIN_MSYS2_ROOT%\usr\bin;%PATH%"

echo Current time: %time%
rem Open shell

%WIN_MSYS2_ROOT%\mingw64.exe
