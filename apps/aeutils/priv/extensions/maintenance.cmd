@echo off

:: Wrapper to inspect and enable/diable maintenance mode

set "PATH=%BINDIR%;%PATH%"
set "ERL_LIBS=%ROOTDIR%\lib"
set "APPS_VSN=%REL_VSN%"

"%ERTS_DIR%\bin\escript" "%ROOTDIR%\lib\aeutils-%APPS_VSN%\priv\maintenance" "-%NAME_TYPE%" "%NAME%" --setcookie "%COOKIE%" %*
