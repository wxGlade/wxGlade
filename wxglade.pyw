#!/usr/bin/env python2

"""
Entry point of wxGlade on windows

@copyright: 2002-2004 Alberto Griggio
@copyright: 2015 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import sys
import traceback

import wxglade

# ctypes has introduced with Python 2.5, but wxGlade is supported with
# Python 2.4 too. Thereby ctypes will be used if available only.
try:
    import ctypes
    has_ctypes = True
except ImportError:
    has_ctypes = False

try:
    # run the main function and exit on success
    wxglade.run_main()
except:
    pass

# show caught exceptions in a windows message box
(exc_type, exc_value, exc_tb) = sys.exc_info()
if has_ctypes and exc_type:
    exc_traceback = '\r\n'.join(traceback.format_tb(exc_tb))

    title = 'An internal error occurred in wxGlade'
    msg = """\
An internal error occurred while starting wxGlade

Error type: %s
Error summary: %s
Error details:
%s

This is a bug - please report it.""" % (exc_type, exc_value, exc_traceback)

    MB_OK = 0x0
    ICON_STOP = 0x10
    MessageBox = ctypes.windll.user32.MessageBoxA
    MessageBox(None, msg, title, MB_OK | ICON_STOP)
