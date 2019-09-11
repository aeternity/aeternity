@echo on
:: Unset CircleCI default setting that prevents pulling Styrene from the public repo
git config --global --unset url."ssh://git@github.com".insteadOf "https://github.com"

:: Run Env preparation script in verbose mode (echo on)
call "%~dp0..\..\scripts\windows\msys2_prepare" -v
