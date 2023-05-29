@echo off

:: Wrapper to build a CLI 

set "PATH=%BINDIR%;%PATH%"
set "ERL_LIBS=%ROOTDIR%\lib"
set "APPS_VSN=%REL_VSN%"

"%ERTS_DIR%\bin\escript" "%ROOTDIR%\lib\aeutils-%APPS_VSN%\priv\status" "-%NAME_TYPE%" "%NAME%" --setcookie "%COOKIE%" %*
