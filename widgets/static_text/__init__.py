# __init__.py: static text widget module initialization
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: GPL (see license.txt)

def initialize():
    import common
    import codegen
    codegen.initialize()
    if common.use_gui:
        import static_text
        return static_text.initialize()
