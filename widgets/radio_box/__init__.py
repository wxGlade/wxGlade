"""
wxRadioBox widget module initialization

@copyright: 2002-2007 Alberto Griggio <agriggio@users.sourceforge.net>
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


def initialize():
    import common
    import codegen
    codegen.initialize()
    if common.use_gui:
        import radio_box
        return radio_box.initialize()
