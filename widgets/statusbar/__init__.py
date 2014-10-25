"""
Statusbar widget module initialization

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


def initialize():
    import config
    import codegen
    codegen.initialize()
    if config.use_gui:
        import statusbar
        global EditStatusBar
        EditStatusBar = statusbar.EditStatusBar
        return statusbar.initialize()