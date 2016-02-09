#!/usr/bin/env python2

"""
Entry point of wxGlade on windows

@copyright: 2002-2004 Alberto Griggio
@copyright: 2015-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import sys
import traceback
import types

import config
import wxglade

# ctypes has introduced with Python 2.5, but wxGlade is supported with
# Python 2.4 too. Thereby ctypes will be used if available only.
try:
    import ctypes
    has_ctypes = True
except ImportError:
    has_ctypes = False

msg = ''
title = ''


def show_message_box(title, msg):
    """\
    Show a message box with given details

    @param title: Message box title
    @type title:  str
    @param msg:   Message
    @type msg:    str
    """
    if not has_ctypes:
        return

    MB_OK = 0x0
    ICON_STOP = 0x10
    MessageBox = ctypes.windll.user32.MessageBoxA
    MessageBox(None, msg, title, MB_OK | ICON_STOP)


try:
    # run the main function and exit on success
    wxglade.run_main()

except SystemExit, details:
    code = details.code
    title = 'Abnormal Termination of wxGlade'
    if isinstance(code, types.IntType) and code != 0:
        msg = """\
wxGlade is terminating abnormally with an error.

Please check the wxGlade log file to get more information.

The exit code is: %d
The log file is :%s""" % (code, config.log_file)
    elif isinstance(code, types.StringTypes):
        msg = """\
wxGlade is terminating abnormally with an error.

Please read the error section in the wxGlade manual for
more details. The wxGlade log file may contain additional
information.

The error message is: %s
The log file is :%s""" % (code, config.log_file)

except:
    # show caught exceptions in a windows message box
    (exc_type, exc_value, exc_tb) = sys.exc_info()
    if exc_type not in [None, SystemExit]:
        exc_traceback = '\r\n'.join(traceback.format_tb(exc_tb))

        title = 'Internal Error in wxGlade'
        msg = """\
An internal error occurred while starting wxGlade

This is a bug - please report it and attach the log file.

Error log file: %s
Error type:     %s
Error summary:  %s
Error details:
%s""" % (config.log_file, exc_type, exc_value, exc_traceback)

if msg:
    show_message_box(title, msg)
