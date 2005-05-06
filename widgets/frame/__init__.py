# __init__.py: frame widget module initialization
# $Id: __init__.py,v 1.7 2005/05/06 21:48:21 agriggio Exp $
#
# Copyright (c) 2002-2005 Alberto Griggio <agriggio@users.sourceforge.net>
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

def initialize():
    import common
    import codegen
    codegen.initialize()
    if common.use_gui:
        import frame
        return frame.initialize()
