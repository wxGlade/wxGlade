Building a wxGlade installer HOWTO
----------------------------------

Alberto Griggio  2007/04/20
Carsten Grohmann 2011/10/18

1. Setup PyInstaller
--------------------

Install the current version of PyInstaller from http://www.pyinstaller.org.
Follow the instruction published at PyInstallers homepage.


2. Generate an executable
-------------------------

- Open the wxglade.spec file with an editor, and edit the three variables
  PYTHON_DIR, WXGLADE_EXE_SCRIPTS_DIR and WXGLADE_DIR to reflect your
  environment. WXGLADE_EXE_SCRIPTS_DIR is the full path of the directory
  containing this README, whily WXGLADE_DIR is the full path of the wxGlade
  directory.

- From this directory, open a command prompt an type
  python <PATH_TO_PYTHON>\Lib\site-packages\Installer\Build.py wxglade.spec

XXXXXX  python.exe c:\Programme\pyinstaller-1.5.1\Build.py install\pyinstaller\wxglade.spec
XXXXXX  python.exe c:\Programme\pyinstaller-1.5.1\Build.py --noconfirm --buildpath c:\Programme\...\wxglade repo install\pyinstaller\wxglade.spec

  if everything works, you should now have two new subdirectories,
  "buildwxglade" and "distwxglade"

- Now you should have a working executable in "distwxglade", called
  "wxglade.exe". If it works, you can move on to the next step

- Optional: if you have upx installed, you might want to compress the
  DLLs and pyd files in "distwxglade\support", to obtain a smaller installer


3. Generate an installer
------------------------

- Download and install Inno Setup from here:
  http://www.jrsoftware.org/isinfo.php

- Open the "make_installer_script.py" file in this directory, and edit the
  WXGLADE_DIST_DIR variable to adjust it to your environment

- run the make_installer_script.py script, from this directory

- You should now have a "wxglade-installer.iss" file. Open it with Inno Setup
  and run it. If everything works, you should have a new "Output" directory,
  with a "xGlade-XXX-setup.exe" file

- All done!
