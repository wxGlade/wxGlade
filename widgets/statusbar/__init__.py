"""
Statusbar widget module initialization

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""
from __future__ import absolute_import

def initialize():
    import config
    from . import codegen
    codegen.initialize()
    #if config.use_gui:
    from . import statusbar
    global EditStatusBar
    EditStatusBar = statusbar.EditStatusBar
    return statusbar.initialize()
