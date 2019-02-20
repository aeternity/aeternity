@echo off

:: Wrapper to run peer_key

set "PATH=%BINDIR%;%PATH%"
set "ERL_LIBS=%ROOTDIR%\lib"
set "APPS_VSN=%REL_VSN%"

"%ERTS_DIR%\bin\escript" "%ROOTDIR%\lib\aeutils-%APPS_VSN%\priv\peer_key" "%NAME_TYPE%" "%NAME%" -setcookie "%COOKIE%" %*
