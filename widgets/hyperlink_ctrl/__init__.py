"""
HyperlinkCtrl widget module initialization

@copyright: 2012 Carsten Grohmann <mail@carstengrohmann.de>
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


def initialize():
    import config
    import codegen
    codegen.initialize()
    if config.use_gui:
        import hyperlink_ctrl
        return hyperlink_ctrl.initialize()
