@echo off

:: Wrapper to inspect and change the runtime mode

set "PATH=%BINDIR%;%PATH%"
set "ERL_LIBS=%ROOTDIR%\lib"
set "APPS_VSN=%REL_VSN%"

"%ERTS_DIR%\bin\escript" "%ROOTDIR%\lib\aeutils-%APPS_VSN%\priv\runtime_mode" "-%NAME_TYPE%" "%NAME%" --setcookie "%COOKIE%" %*
