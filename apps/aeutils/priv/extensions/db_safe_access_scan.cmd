@echo off

:: Wrapper to run db_safe_access_scan

set "PATH=%BINDIR%;%PATH%"
set "ERL_LIBS=%ROOTDIR%\lib"
set "APPS_VSN=%REL_VSN%"

"%ERTS_DIR%\bin\escript" "%ROOTDIR%\lib\aeutils-%APPS_VSN%\priv\db_safe_access_scan" "-%NAME_TYPE%" "%NAME%" --setcookie "%COOKIE%" %*
