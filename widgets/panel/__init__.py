"""
Panel widget module initialization

@copyright: 2002-2007 Alberto Griggio
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""
from __future__ import absolute_import

def initialize():
    import config
    from . import codegen
    codegen.initialize()
    #if config.use_gui:
    from . import panel
    global EditTopLevelPanel
    global EditPanel
    EditTopLevelPanel = panel.EditTopLevelPanel
    EditPanel = panel.EditPanel
    return panel.initialize()
