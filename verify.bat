@echo off
SETLOCAL
SET BEEBASM=..\..\bin\beebasm.exe
SET PYTHON=C:\Home\Python27\python.exe
make encrypt
make verify
