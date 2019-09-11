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

SETLOCAL
@IF "%~1"=="-v" shift /1 & echo on & echo

IF NOT "%~1"=="/?" IF NOT "%~1"=="--help" IF NOT "%~1"=="" GOTO:START
@echo Download and install Erlang/OTP from erlang-solutions.com
@echo Usage: %0 [/?] ^| ^<otp_version^> [^<install_path^>]
@echo.
@echo Notes:
@echo     Default ^<install_path^> is "C:\tools\erl<otp_version>"
@echo     Define "OTP_URL" env var to use custom download url
@echo     Any existing installs in ^<install_path^> will be uninstalled first
@echo     Env vars required by build scripts will be dumped
@IF NOT "%WIN_OTP_PATH%"=="" echo     Current: WIN_OTP_PATH=%WIN_OTP_PATH%
exit /b 2

:START
SET "WIN_OTP_PATH=C:\tools\erl%~1"
IF NOT "%~2"=="" SET "WIN_OTP_PATH=%~2"
IF "%OTP_URL%"=="" SET "OTP_URL=https://packages.erlang-solutions.com/erlang/erlang/esl-erlang/FLAVOUR_1_general/esl-erlang_%~1~windows_amd64.exe"

:: Check if we have an old install
:CHECK_VERSION
call:get_otp_version OTP_VERSION ERTS_VERSION

@call:log Install Erlang/OTP %~1 into %WIN_OTP_PATH%

:: Installed version is different
IF NOT "%OTP_VERSION%"=="%~1" GOTO:INSTALL
IF EXIST "%WIN_OTP_PATH%\bin\" @call:log Erlang/OTP %OTP_VERSION% already installed. && GOTO:DONE
:INSTALL
@call:log Download from %OTP_URL%
PowerShell (New-Object System.Net.WebClient).DownloadFile('%OTP_URL%', '%TMP%\erl.exe')
@IF NOT "%ERRORLEVEL%"=="0" (call:log Download failed && exit /b %ERRORLEVEL%)

:: Uninstall the old version
IF EXIST "%WIN_OTP_PATH%\Uninstall.exe" @call:log Found existing %OTP_VERSION% - Uninstalling... ^
 && PowerShell start-process -wait -WindowStyle hidden -FilePath '%WIN_OTP_PATH%\Uninstall.exe' -ArgumentList '/S'

@call:log Install to %WIN_OTP_PATH%
PowerShell start-process -wait -WindowStyle hidden -FilePath '%TMP%\erl.exe' -ArgumentList '/S /D=%WIN_OTP_PATH%'
@IF NOT "%ERRORLEVEL%"=="0" (call:log Install failed && exit /b %ERRORLEVEL%)

@call:log Done.

:DONE
@call:get_otp_version OTP_VERSION ERTS_VERSION

@call:log Make sure you have these ENV vars:
@echo SET "WIN_OTP_PATH=%WIN_OTP_PATH%"
@echo SET OTP_VERSION=%OTP_VERSION%
@echo SET ERTS_VERSION=%ERTS_VERSION%
@echo SET "PATH=%WIN_OTP_PATH%\bin;%WIN_OTP_PATH%\erts-%ERTS_VERSION%\bin;%WIN_OTP_PATH%\erts-%ERTS_VERSION%;%%PATH%%"

exit /b 0

:: Detect the versions of an existing OTP/ERTS installed in WIN_OTP_PATH and update the arguments passed by reference
:get_otp_version ::OTP_VERSION ERTS_VERSION
IF NOT EXIST "%WIN_OTP_PATH%\Install.ini" exit /b
FOR /F "tokens=* skip=1 USEBACKQ" %%F IN (`type "%WIN_OTP_PATH%\Install.ini"`) DO SET "_%%F"
SET "_OTP_RELEASE_FILE=%WIN_OTP_PATH%\releases\%_SYSTEM_VSN%\OTP_VERSION"
IF EXIST "%_OTP_RELEASE_FILE%" SET /p _OTP_VERSION= 0<"%_OTP_RELEASE_FILE%" 2>nul
SET "%~1=%_OTP_VERSION%"
SET "%~2=%_VSN%"
SET _OTP_VERSION=
SET _SYSTEM_VSN=
SET _VSN=
SET _OTP_RELEASE_FILE=
exit /b

:log :: Display a log message
@echo :: [1;33m %time% : %* [0m>con && exit /b
