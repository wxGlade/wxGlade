# __init__.py: grid widget module initialization
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

# Dinogen: first try to create a widget handler
# obviously disonnected from CVS.

def initialize():
    import common
    import codegen
    codegen.initialize()
    if common.use_gui:
        import grid
        return grid.initialize()
