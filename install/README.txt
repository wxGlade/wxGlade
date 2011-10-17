Building a wxGlade installer HOWTO
----------------------------------

Alberto Griggio
2007/04/20

1. Setup McMillan Installer
---------------------------

Unpack the Installer.zip archive in the "site-packages" dir of your python
installation, and follow the instructions in the "Installer/README.txt" to set
it up correctly.


2. Generate an executable
-------------------------

- Open the wxglade.spec file with an editor, and edit the three variables
  PYTHON_DIR, WXGLADE_EXE_SCRIPTS_DIR and WXGLADE_DIR to reflect your
  environment. WXGLADE_EXE_SCRIPTS_DIR is the full path of the directory
  containing this README, whily WXGLADE_DIR is the full path of the wxGlade
  directory.

- From this directory, open a command prompt an type
  python <PATH_TO_PYTHON>\Lib\site-packages\Installer\Build.py wxglade.spec

  if everything works, you should now have two new subdirectories,
  "buildwxglade" and "distwxglade"

- Copy the subdirectories "icons", "res", "widgets", "locale", "codegen",
  "docs", "po", and the files "license.txt" and "credits.txt" of your wxGlade
  installation in the "distwxglade" directory. If you are using a CVS version,
  be sure to delete all the "CVS" subdirectories and ".cvsignore" files
  (otherwise the exe might not work)

- Now you should have a working executable in "distwxglade", called
  "wxglade.exe". If it works, you can move on to the next step

- Optional: if you have upx installed, you might want to compress the
  DLLs and pyd files in "distwxglade\support", to obtain a smaller installer

- WARNING: In the past I had troubles generating executables for Python >=
  2.4. I never investigated this issue in depth, but if your .exe does not
  work, I suggest installing python 2.3.6 (you can safely install multiple
  python versions, it shouldn't be a problem).


3. Generate an installer
------------------------

- Download and install Inno Setup from here:
  http://www.jrsoftware.org/isinfo.php

- Open the "make_installer_script.py" file in this directory, and edit the
  WXGLADE_DIST_DIR variable to adjust it to your environment

- run the make_installer_script.py script, from this directory

- You should now have a "wxglade-installer.iss" file. Open it with Inno Setup
  and run it. If everything works, you should have a new "Output" directory,
  with a "setup.exe" file

- Rename "setup.exe" to "wxGlade-XXX-setup.exe", where XXX is the version
  number

- All done!
