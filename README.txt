wxGlade: a GUI builder for wxPython/wxWindows
version: 0.1.3
license: Python 2.2 license (see license.txt)

THIS PROGRAM COMES WITH NO WARRANTY

* Requirements:
---------------
Python >= 2.2
wxPython >= 2.3.2.1 


* Installation:
---------------
If you are reding this file, you already did all the necessary :-)
To start the program, enter ``python wxglade.py'' in your shell


* Documentation:
----------------
In the docs/ subdir there's a short introductory tutorial.  In the samples/
subdir there are some sample wxGlade apps (in xml format, .wxg file extension)
and their equivalent Python code.


* Known Bugs:
-------------
- On Windows, sometimes the Properties window's layout is corrupted, especially
  when the active object is a sizer

- On Windows, selection tags may not be shown properly in some case

- On Windows, splitter windows in the generated code may not update their 
  layout until a resize occurs

- On GTK, if your last operation before exiting is a paste from the clipboard,
  wxGlade exits with a segfault

- On GTK, menubars give troubles: they produce a lot of Gtk-WARNING and 
  Gtk-FATAL messages and may cause segfaults

- On GTK, notebooks cause some Gtk-WARNING messages, but they seem to work 
  anyway


For any kind of question, there's a mailing list at 
https://lists.sourceforge.net/lists/listinfo/wxglade-general
If you don't want to follow the list, you can reach me at 
albgrig@tiscalinet.it

Enjoy!
Alberto Griggio
