@echo off

:: Simple wrapper to run check_config
:: Relx extensions cannot be binary files but sourced bash files only
:: Also append ERTS BINDIR to PATH to run the escript

set "PATH=%BINDIR%;%PATH%"

"%ERTS_DIR%\bin\escript" "%SCRIPT_DIR%\check_config" %*
