@echo off

:: Wrapper to run keys_gen
:: Does not use relx_escript, as it does not work well with whitespaces in password, even in brackets.

set "PATH=%BINDIR%;%PATH%"
set "ERL_LIBS=%ROOTDIR%\lib"
set "APPS_VSN=%REL_VSN%"

"%ERTS_DIR%\bin\escript" "%ROOTDIR%\lib\aeutils-%APPS_VSN%\priv\keys_gen" %*
