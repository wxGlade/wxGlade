# __init__.py: static bitmap widget module initialization
# $Id: __init__.py,v 1.5 2003/05/13 10:05:08 agriggio Exp $
#
# Copyright (c) 2002-2003 Alberto Griggio <albgrig@tiscalinet.it>
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

def initialize():
    import common
    import codegen
    codegen.initialize()
    if common.use_gui:
        import static_bitmap
        return static_bitmap.initialize()
