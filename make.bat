@echo off
SET BEEBASM=..\..\bin\beebasm.exe
SET PYTHON=C:\Home\Python27\python.exe

%BEEBASM% -i elite-source.asm -v > compile.txt
%BEEBASM% -i elite-bcfs.asm -v >> compile.txt
%BEEBASM% -i elite-loader.asm -v >> compile.txt
%PYTHON% elite-checksum.py
%BEEBASM% -i elite-disc.asm -do elite.ssd -boot ELITE
