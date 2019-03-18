@echo on
@rem Script to open a msys2 shell ready for building.
@rem Required vars:
@rem    WIN_MSYS2_ROOT
@rem    PLATFORM

SETLOCAL ENABLEEXTENSIONS

rem Set required vars defaults
IF "%WIN_MSYS2_ROOT%"=="" SET "WIN_MSYS2_ROOT=C:\msys64"
IF "%PLATFORM%"=="" SET "PLATFORM=x64"

@echo Current time: %time%
rem Set the paths appropriately

call "C:\Program Files (x86)\Microsoft Visual Studio\2017\Community\VC\Auxiliary\Build\\vcvarsall.bat" %PLATFORM%
@echo on
SET PATH=%WIN_MSYS2_ROOT%\mingw64\bin;%WIN_MSYS2_ROOT%\usr\bin;%PATH%

@echo Current time: %time%
rem Open shell

%WIN_MSYS2_ROOT%\mingw64.exe
