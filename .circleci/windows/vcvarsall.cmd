@echo on
:: Helper script that finds and executes MSVC env preparation script and sets MSVC_VERSION
:: Manually set MSVC_VERSION and VCVARSALL env vars to speed up by skipping autodetection.
:: Values suitable for copy-paste are dumped at the end of the script.
:: Optional vars:
::    PLATFORM
::    VCVARSALL - full path to vcvarsall.bat
::    MSVC_VERSION

@IF "%PLATFORM%"=="" SET "PLATFORM=x64"

@call:log Find and set MSVC_VERSION
@IF NOT "%MSVC_VERSION%"=="" GOTO:MSVC_VERSION_SET
@FOR /F "tokens=* USEBACKQ delims=" %%F IN (`where /r "%ProgramFiles(x86)%\Microsoft Visual Studio" Microsoft.VCToolsVersion.default.txt`) DO SET /p MSVC_VERSION=<"%%F"
:MSVC_VERSION_SET

@call:log Find and execute the VS env preparation script
:: Avoid multiple executions
@IF NOT "%DevEnvDir%"=="" GOTO:DONE
@IF NOT "%VCVARSALL%"=="" GOTO:VCVARSALLFOUND
@FOR /F "tokens=* USEBACKQ delims=" %%F IN (`where /r "%ProgramFiles(x86)%\Microsoft Visual Studio" vcvarsall`) DO SET "VCVARSALL=%%F"
:VCVARSALLFOUND

call "%VCVARSALL%" %PLATFORM% || exit /b %ERRORLEVEL%

:DONE
@call:log Persist these env vars to speed up
@echo SET MSVC_VERSION=%MSVC_VERSION%
@echo SET VCVARSALL=%VCVARSALL%

@exit /b 0

:log :: Display a log message
@echo :: [1;33m %time% : %* [0m>con && exit /b
