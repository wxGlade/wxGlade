# __init__.py: spin ctrl widget module initialization
# $Id: __init__.py,v 1.1 2004/09/23 11:47:56 crazyinsomniac Exp $
#
# Copyright (c) 2002-2004 Alberto Griggio <agriggio@users.sourceforge.net>
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

def initialize():
    import common
    import codegen
    codegen.initialize()
    if common.use_gui:
        import spin_button
        return spin_button.initialize()
