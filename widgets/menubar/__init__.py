# __init__.py: menubar widget module initialization
# $Id: __init__.py,v 1.4 2005/05/06 21:48:20 agriggio Exp $
#
# Copyright (c) 2002-2005 Alberto Griggio <agriggio@users.sourceforge.net>
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

def initialize():
    import common
    import codegen
    codegen.initialize()
    if common.use_gui:
        import menubar
        global EditMenuBar; EditMenuBar = menubar.EditMenuBar
        return menubar.initialize()
