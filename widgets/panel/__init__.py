# __init__.py: panel widget module initialization
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: Python 2.2 license (see license.txt)

def initialize():
    import common
    import codegen
    codegen.initialize()
    if common.use_gui:
        import panel
        global EditTopLevelPanel; EditTopLevelPanel = panel.EditTopLevelPanel
        return panel.initialize()
