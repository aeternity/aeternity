@echo off

:: Wrapper to run create_whitelist

set "PATH=%BINDIR%;%PATH%"
set "ERL_LIBS=%ROOTDIR%\lib"
set "APPS_VSN=%REL_VSN%"

"%ERTS_DIR%\bin\escript" "%ROOTDIR%\lib\aeutils-%APPS_VSN%\priv\create_whitelist" "%NAME_TYPE%" "%NAME%" -setcookie "%COOKIE%" %*
