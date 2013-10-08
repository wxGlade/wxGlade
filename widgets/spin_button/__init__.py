"""
Spin ctrl widget module initialization

@copyright: 2002-2007 Alberto Griggio
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

def initialize():
    import config
    import codegen
    codegen.initialize()
    if config.use_gui:
        import spin_button
        return spin_button.initialize()
