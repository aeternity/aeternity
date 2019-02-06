@echo off

SETLOCAL ENABLEEXTENSIONS

SET REL_NAME=aeternity

echo WARNING: This script named "%~nx0" is deprecated, please use the script named "%REL_NAME%.cmd" instead.

call %~dp0\%REL_NAME%.cmd %*
