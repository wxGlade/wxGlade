"""
Notebook widget module initialization

@copyright: 2002-2007 Alberto Griggio <agriggio@users.sourceforge.net>
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


def initialize():
    import config
    import codegen
    codegen.initialize()
    if config.use_gui:
        import notebook
        return notebook.initialize()
