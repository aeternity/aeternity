SETLOCAL ENABLEEXTENSIONS

SET REL_NAME=aeternity

@echo WARNING: This script named "%~nx0" is deprecated, please use the script named "%REL_NAME%.bat" instead.

call %~dp0/%REL_NAME%.bat %*
