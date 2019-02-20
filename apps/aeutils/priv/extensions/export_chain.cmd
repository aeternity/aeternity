@echo off

:: Wrapper to run export_chain

set "PATH=%BINDIR%;%PATH%"
set "ERL_LIBS=%ROOTDIR%\lib"
set "APPS_VSN=%REL_VSN%"

"%ERTS_DIR%\bin\escript" "%ROOTDIR%\lib\aeutils-%APPS_VSN%\priv\export_chain" "%NAME_TYPE%" "%NAME%" -setcookie "%COOKIE%" %*
