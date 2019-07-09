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
IF "%WIN_MSYS2_ROOT%"=="" FOR /F %%F IN ('where msys2') DO SET "WIN_MSYS2_ROOT=%%~dpF"
IF "%PLATFORM%"=="" SET "PLATFORM=x64"
IF "%BUILD_STEP%"=="" SET "BUILD_STEP=build"
IF "%BUILD_PATH%"=="" GOTO :BUILDABORT_MISSINGBUILDPATH

@echo Current time: %time%
rem Set the paths appropriately

@FOR /F "tokens=* USEBACKQ delims=" %%F IN (`where /r "C:\Program Files (x86)\Microsoft Visual Studio" Microsoft.VCToolsVersion.default.txt`) DO SET "ToolsVerFile=%%F"
@FOR /F "tokens=* USEBACKQ delims=" %%F IN (`type "%ToolsVerFile%"`) DO SET MSVC_VERSION=%%F
@FOR /F "tokens=* USEBACKQ delims=" %%F IN (`where /r "C:\Program Files (x86)\Microsoft Visual Studio" vcvarsall`) DO SET vcvarsall="%%F"
call %vcvarsall% %PLATFORM%

SET PATH=%WIN_MSYS2_ROOT%\mingw64\bin;%WIN_MSYS2_ROOT%\usr\bin;%PATH%

:BUILDSTART
@GOTO BUILD_%BUILD_STEP%

:BUILD_build
@echo Current time: %time%
rem Run build: build
bash -lc "cd %BUILD_PATH% && make KIND=test local-build"

@GOTO BUILD_DONE

:BUILD_
:BUILD_DONE

@echo Current time: %time%
rem Finished build phase
exit /B 0

:BUILDABORT_MISSINGBUILDPATH
@echo ERROR: Missing environment variable BUILD_PATH
exit /B 1
