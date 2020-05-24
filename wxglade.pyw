#!/usr/bin/env python

"""
Entry point of wxGlade on windows

@copyright: 2002-2004 Alberto Griggio
@copyright: 2015-2016 Carsten Grohmann
@copyright: 2017 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import sys, traceback
import gettext
t = gettext.translation(domain="wxglade", localedir="locale", fallback=True)
t.install("wxglade")

import config
import wxglade

msg   = u''  # Message to show in the message box (see {show_error_details())
title = u''  # Title of the message box


def show_error_details():
    "Show a message box; msg and title have to be set before"

    # ctypes has introduced with Python 2.5, but wxGlade is supported with
    # Python 2.4 too. Thereby ctypes will be used if available only.
    try:
        import ctypes
    except ImportError:
        return

    MB_OK = 0x0
    ICON_STOP = 0x10
    MessageBox = ctypes.windll.user32.MessageBoxW
    MessageBox(None, msg, title, MB_OK | ICON_STOP)


try:
    # run the main function and exit on success
    wxglade.run_main()

except SystemExit as details:
    code = details.code
    title = u'Abnormal Termination of wxGlade'
    if isinstance(code, int) and code != 0:
        msg = u"""\
wxGlade is terminating abnormally with an error.

Please check the wxGlade log file to get more information.

The exit code is: %d
The log file is : %s""" % (code, config.log_file)
    elif isinstance(code, basestring):
        msg = u"""\
wxGlade is terminating abnormally with an error.

Please read the error section in the wxGlade manual for
more details. The wxGlade log file may contain additional
information.

The error message is: %s
The log file is: %s""" % (code, config.log_file)

except:
    exc_type, exc_value, exc_tb = sys.exc_info()
    if exc_type not in [None, SystemExit]:
        exc_traceback = '\r\n'.join(traceback.format_tb(exc_tb))

        title = u'Internal Error in wxGlade'
        msg = u"""\
An internal error occurred while starting wxGlade

This is a bug - please report it and attach the log file.

Error log file: %s
Error type:     %s
Error summary:  %s
Error details:
%s""" % (config.log_file, exc_type, exc_value, exc_traceback)

if msg:
    show_error_details()
