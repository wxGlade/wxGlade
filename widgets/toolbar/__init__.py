"""
Toolbar widget module initialization

@copyright: 2002-2007 Alberto Griggio
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""
from __future__ import absolute_import

def initialize():
    import config
    from . import codegen
    codegen.initialize()
    #if config.use_gui:
    from . import toolbar
    global EditToolBar
    EditToolBar = toolbar.EditToolBar
    return toolbar.initialize()
