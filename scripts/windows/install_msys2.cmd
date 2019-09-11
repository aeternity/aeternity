@echo off
:: Pass -v as the first argument to enable debug echo
:: Script to download and install Msys2 from %MSYS2_URL%
:: Output is suitable for piping scripts
:: ENV vars will be updated accordingly
::      WIN_MSYS2_ROOT
::      MSYS2_URL
:: Usage: install_msys2 /? | [<install_path>]

SETLOCAL
@IF "%~1"=="-v" shift /1 & echo on & echo

IF NOT "%~1"=="/?" IF NOT "%~1"=="--help" GOTO:START
@echo Download and install Msys2 from netcologne.dl.sourceforge.net
@echo Usage: %0 /? ^| [^<install_path^>]
@echo     Default ^<install_path^> is "C:\tools\msys64" or WIN_MSYS2_ROOT env var
@echo     Define MSYS2_URL env var to use different download url
@echo     Env vars required by build scripts will be dumped
exit /b 2

:START
@call:log MSYS2 installer
IF NOT "%~1"=="" SET "WIN_MSYS2_ROOT=%~1"
IF "%WIN_MSYS2_ROOT%"=="" SET "WIN_MSYS2_ROOT=C:\tools\msys64"
IF "%MSYS2_URL%"=="" SET "MSYS2_URL=https://netcologne.dl.sourceforge.net/project/msys2/Base/x86_64/msys2-base-x86_64-20190524.tar.xz"

IF EXIST "%WIN_MSYS2_ROOT%\msys2.exe" @call:log Already installed. && GOTO :MSYS2_INSTALLED
@call:log Target directory %WIN_MSYS2_ROOT%

IF EXIST "%WIN_MSYS2_ROOT%" rd /s/q "%WIN_MSYS2_ROOT%"
SET "PSAdmin=powershell Start-Process -wait -WindowStyle hidden 'powershell.exe'"

@call:log Download from %MSYS2_URL%
powershell (New-Object System.Net.WebClient).DownloadFile('%MSYS2_URL%', '%TMP%\msys2.tar.xz')

@call:log Download PowerShell 7z helper
%PSAdmin% 'Install-Package -Scope CurrentUser -Force 7Zip4PowerShell'

@call:log Msys2 extract
%PSAdmin% 'Expand-7Zip "%TMP%\msys2.tar.xz" "%TMP%" ; Expand-7Zip "%TMP%\msys2.tar" "%WIN_MSYS2_ROOT%\..\"'

:: Msys is located in a subfolder "msys64" so we need to move it to the desired path
move %WIN_MSYS2_ROOT%\..\msys64 %WIN_MSYS2_ROOT% && del /q %TMP%\msys2*.*

@call:log Msys2 Initial setup
:: First run installs only minimal deps required to run pacman
%WIN_MSYS2_ROOT%\usr\bin\bash -lc "echo Done."

@call:log Msys2 Install dev tools
:: Run pacman to install the msys2 components (first pass)
%WIN_MSYS2_ROOT%\usr\bin\bash -lc "pacman --noconfirm -Syuu"

:MSYS2_UPDATE
:: Run again pacman to update the msys2 components (second pass)
@call:log Msys2 Update
%WIN_MSYS2_ROOT%\usr\bin\bash -lc "pacman --noconfirm -Syuu"
:MSYS2_INSTALLED

@call:log Make sure you have these ENV vars:
@echo SET "WIN_MSYS2_ROOT=%WIN_MSYS2_ROOT%"
@echo SET "PATH=%WIN_MSYS2_ROOT%\mingw64\bin;%WIN_MSYS2_ROOT%\usr\bin;%WIN_MSYS2_ROOT%;%%PATH%%"

exit /b 0

:log :: Display a log message
@echo :: [1;33m %time% : %* [0m>con && exit /b
