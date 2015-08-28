"""
Menubar widget module initialization

@copyright: 2002-2007 Alberto Griggio
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


def initialize():
    import config
    import codegen
    codegen.initialize()
    if config.use_gui:
        import menubar
        global EditMenuBar
        EditMenuBar = menubar.EditMenuBar
        return menubar.initialize()
