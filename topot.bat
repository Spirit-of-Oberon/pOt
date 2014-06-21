@echo off
if "%1" == "" GOTO :ERROR
set POTLAND=%1/lib/;%1/lib/Core/
set PATH=%1/BIN;%PATH%
GOTO :END
:ERROR
  echo one command line parameter, designating the directory where
  echo pot resides must be specified, e.g.
  echo   pot c:/lang/pot
:END
