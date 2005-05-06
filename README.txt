wxGlade: a GUI builder for wxPython/wxWindows
version: 0.4
license: MIT (see license.txt)

THIS PROGRAM COMES WITH NO WARRANTY

* Requirements:
---------------
Python >= 2.2
wxPython >= 2.4.2


* Installation:
---------------
If you are reding this file, you already did all the necessary :-)
To start the program, enter ``python wxglade.py'' in your shell


* Documentation:
----------------
In the docs/ subdir there's a short introductory tutorial.  In the examples/
subdir there are some sample wxGlade apps (in xml format, .wxg file extension).

NOTE FOR OLD USERS: even if you already know wxGlade, you should probably read
the last section of the tutorial, since it contains information that all users
should know.


* Known Bugs/Issues:
--------------------
- If you have PyXML installed, be sure to use at least version 0.8.1, as
  otherwise you will not be able to generate code and load saved wxg
  files. This seems to be a PyXML bug, see
  http://sourceforge.net/tracker/?func=detail&atid=106473&aid=573011&group_id=6473 

- I don't know if it works on the Mac (OS X): if you try it on such platform,
  please let me know if it works.

- On Windows, selection tags may not be shown properly in some cases.

- On GTK, if your last operation before exiting is a paste from the clipboard,
  wxGlade exits with a segfault.

- On GTK, menubars give troubles: they produce a lot of Gtk-WARNING and 
  Gtk-FATAL messages and may cause segfaults.

- On GTK, notebooks can cause some Gtk-WARNING messages, but they seem to work 
  anyway.


For any kind of question, there's a mailing list at 
    https://lists.sourceforge.net/lists/listinfo/wxglade-general
If you don't want to follow the list, you can reach me at 
    agriggio@users.sourceforge.net

Enjoy!
Alberto Griggio

$Id: README.txt,v 1.13 2005/05/06 21:48:26 agriggio Exp $
