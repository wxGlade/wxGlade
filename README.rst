=============================================
wxGlade: A GUI builder for wxPython/wxWidgets
=============================================

.. image:: docs/html/_images/wxglade_large.png
   :align: center
   
Version: 1.1.0a2
License: MIT (see LICENSE.txt)

THIS PROGRAM COMES WITH NO WARRANTY


Introduction
------------

wxGlade is a GUI builder for wxWidgets/wxPython.
It can create Python, C++, Perl, Lisp and XRC output.


Requirements
------------

* Python (http://www.python.org) 2.7, 3.4 or later
* wxPython (http://www.wxpython.org) >= 2.8
  (wxPython Phoenix, i.e. release >=4.0.0, is supported)

If you have a choice, you should prefer wxPython *Phoenix* running on Python 3.x.
See https://wxpython.org/pages/downloads/ for download and installation instructions.
On Windows and macOS it can be installed with ``python -mpip install wxPython``.


Installation
------------

Download the latest released version as zip archive from
https://sourceforge.net/projects/wxglade/

Unpack the archive to a **known** location and maybe add it to the path.

If you are familiar with git, you may instead just clone the repository from
https://github.com/wxGlade/wxGlade.git
The master branch should be stable enough for your work and I appreciate bug reports.


Running wxGlade
---------------

To start the program, change directory to the installation directory you unzipped to and
enter ``python3 wxglade.py`` or ``python wxglade.py`` in your shell or use whatever is
required to start a python application on your platform.

You may want to add a desktop shortcut to run the ``wxGlade.py`` file (or the ``wxGlade.pyw`` file).

If you want to build a GUI for wxPython Classic or Phoenix:
Start wxGlade in your target version - if it runs under Phoenix, it will
create Phoenix code. The .wxg file format is not affected by this. So you may
use a single .wxg file to generate code for both Classic and Phoenix.
The generated code should always run under both Python 2.7 and Python 3.


Documentation and Tutorial
--------------------------

The documentation is at docs/html/index.html and includes a tutorial.
You can view it from the help menu as well.

The target audience includes people who have not yet been using wxPython
or wxWidgets before.


A snapshot of the documentation including tutorial can be found here:
http://wxglade.sourceforge.net/docs/index.html
This one is not always up to date, though.


Issues / Bugs
-------------

For any kind of question, there's a mailing list:
 https://lists.sourceforge.net/lists/listinfo/wxglade-general

If you observe a bug, please check the bug tracker for current open bugs:
 https://github.com/wxGlade/wxGlade/issues

For new bugs, please open a bug report in the tracker.
You have to register and log in at GitHub to file a bug report.

Alternatively you can send the bug report to the wxGlade mailing list.
Keep in mind that you need a subscription for sending emails to this
mailing list.


Please include the following information:

* What did you? May you want to include a screenshot.
* What do you want to happen?
* What actually happened?
* Provide a short example to reproduce the issue.


Have fun!
Dietmar Schwertberger
