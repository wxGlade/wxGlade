# __init__.py: sizers module initialization
# $Id: __init__.py,v 1.7 2003/05/13 10:05:15 agriggio Exp $
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
        global Sizer, SizerSlot, SizerBase, _builder
        Sizer = edit_sizers.Sizer
        SizerSlot = edit_sizers.SizerSlot
        SizerBase = edit_sizers.SizerBase
        _builder = edit_sizers._builder
        return edit_sizers.init_all()
