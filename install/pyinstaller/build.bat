@echo off
REM Simplify executing pyinstaller
REM
REM Copyright: 2014-2016 Carsten Grohmann
REM License: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY

echo Check requirements:

if not exist wxglade.py (
  echo ERROR: The file "wxglade.py" does not exists!
  echo        Please start this script from the base directory of the
  echo        wxGlade source code only!
  exit /b 1
) else (
  echo PASS:  Found wxglade.py
)

if not exist version.py (
  echo WARN:  Version file "version.py" not found!
  pause
) else (
  echo PASS:  Found version.py
)

echo Delete temporary Python bytecode files
del /S *.pyo
del /S *.pyc

echo Delete temporary directories "dist" and "build"
del /Q dist
del /Q build

echo Convert Python code into executable using PyInstaller
pyinstaller --noconfirm --clean --onedir ^
            --additional-hooks-dir=install/pyinstaller/hooks ^
            install\pyinstaller\wxglade.spec

if errorlevel 1 (
   echo PyInstaller finished with error code %errorlevel%
   exit /b %errorlevel%
) else (
   echo PyInstaller finished successfully.
)