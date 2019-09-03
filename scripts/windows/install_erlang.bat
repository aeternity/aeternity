@echo off
:: Pass -v as the first argument to enable debug echo
:: Script to download and install Erlang/OTP from %OTP_URL% which defaults to a fast mirror
:: Erlang/OTP versions could be switched by providing different <otp_version> for the same <install_path>
:: Output is suitable for piping scripts
:: ENV vars:
::      WIN_OTP_PATH
::      OTP_VERSION
::      ERTS_VERSION
::      OTP_URL
:: Usage: install_erlang [/?] | <otp_version> [<install_path>]

SETLOCAL ENABLEEXTENSIONS
@IF "%~1"=="-v" shift /1 & echo on & echo

IF NOT "%~1"=="/?" IF NOT "%~1"=="--help" IF NOT "%~1"=="" GOTO:START
@echo Download and install Erlang/OTP from erlang-solutions.com
@echo Usage: %0 [/?] ^| ^<otp_version^> [^<install_path^>]
@echo.
@echo Notes:
@echo     Default ^<install_path^> is "C:\tools\erl" or WIN_OTP_PATH env var
@echo     Define OTP_URL env var to use different download url
@echo     Any existing installs in ^<install_path^> will be uninstalled first
@echo     Env vars required by build scripts will be dumped
@IF NOT "%WIN_OTP_PATH%"=="" echo     WIN_OTP_PATH=%WIN_OTP_PATH%
exit /b 2

:START
IF NOT "%WIN_OTP_PATH%"=="" GOTO:CHECK_VERSION
IF NOT "%~2"=="" SET "WIN_OTP_PATH=%~2"
IF "%WIN_OTP_PATH%"=="" SET "WIN_OTP_PATH=C:\tools\erl%~1"

:: Check if we have old install
:CHECK_VERSION
call:get_version OTP_VERSION ERTS_VERSION

:: Installed version is different
IF NOT "%OTP_VERSION%"=="%~1" GOTO:INSTALL
IF EXIST "%WIN_OTP_PATH%\bin\" @call:log Erlang/OTP %OTP_VERSION% already installed. && GOTO:DONE
:INSTALL
@call:log Install Erlang/OTP %~1 into %WIN_OTP_PATH%
IF "%OTP_URL%"=="" SET "OTP_URL=https://packages.erlang-solutions.com/erlang/erlang/esl-erlang/FLAVOUR_1_general/esl-erlang_%~1~windows_amd64.exe"
@call:log Download from %OTP_URL%
PowerShell (New-Object System.Net.WebClient).DownloadFile('%OTP_URL%', '%TMP%\erl.exe')
@IF NOT "%ERRORLEVEL%"=="0" (call:log Download failed && exit /b %ERRORLEVEL%)

:: Uninstall the old version
IF EXIST "%WIN_OTP_PATH%\Uninstall.exe" @call:log Found existing %OTP_VERSION% - Uninstalling... && PowerShell start-process -wait '%WIN_OTP_PATH%\Uninstall.exe' -ArgumentList '/S'

@call:log Install to %WIN_OTP_PATH%
PowerShell start-process -wait '%TMP%\erl.exe' -ArgumentList '/S /D=%WIN_OTP_PATH%'
@IF NOT "%ERRORLEVEL%"=="0" (call:log Install failed && exit /b %ERRORLEVEL%)

@call:log Done.
:DONE
@call:get_version OTP_VERSION ERTS_VERSION

@call:log Make sure you have these ENV vars:
@echo SET "WIN_OTP_PATH=%WIN_OTP_PATH%"
@echo SET OTP_VERSION=%OTP_VERSION%
@echo SET ERTS_VERSION=%ERTS_VERSION%
@echo SET "PATH=%WIN_OTP_PATH%\bin;%WIN_OTP_PATH%\erts-%ERTS_VERSION%\bin;%WIN_OTP_PATH%\erts-%ERTS_VERSION%;%%PATH%%"

exit /b 0

:: This subroutine will detect the versions of the current install and update the vars passed by reference
:get_version ::OTP_VERSION ERTS_VERSION
IF EXIST "%WIN_OTP_PATH%\Install.ini" FOR /F "tokens=* skip=1 USEBACKQ" %%F IN (`type "%WIN_OTP_PATH%\Install.ini"`) DO SET "%%F"
SET "OTP_RELEASE_FILE=%WIN_OTP_PATH%\releases\%SYSTEM_VSN%\OTP_VERSION"
IF EXIST "%OTP_RELEASE_FILE%" SET /p OTP_VERSION= 0<"%OTP_RELEASE_FILE%" 2>nul
SET "%~1=%OTP_VERSION%"
SET "%~2=%VSN%"
exit /b

:log :: Display a log message
@echo :: [1;33m %time% : %* [0m>con && exit /b
