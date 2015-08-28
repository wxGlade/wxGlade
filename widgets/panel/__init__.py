"""
Panel widget module initialization

@copyright: 2002-2007 Alberto Griggio
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


def initialize():
    import config
    import codegen
    codegen.initialize()
    if config.use_gui:
        import panel
        global EditTopLevelPanel
        global EditPanel
        EditTopLevelPanel = panel.EditTopLevelPanel
        EditPanel = panel.EditPanel
        return panel.initialize()
