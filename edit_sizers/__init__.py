# __init__.py: sizers module initialization
#
# Copyright (c) 2002-2003 Alberto Griggio <albgrig@tiscalinet.it>
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

def init_all():
    import sizers_codegen
    sizers_codegen.initialize()
    import common
    if common.use_gui:
        import edit_sizers
        global SizerSlot, SizerBase, _builder
        SizerSlot = edit_sizers.SizerSlot
        SizerBase = edit_sizers.SizerBase
        _builder = edit_sizers._builder
        return edit_sizers.init_all()
