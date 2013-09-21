"""
Sizers module initialization

@copyright: 2002-2007 Alberto Griggio <agriggio@users.sourceforge.net>
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

Sizer = None
"""\
Shortcut for L{edit_sizers.Sizer}
"""

SizerSlot = None
"""\
Shortcut for L{edit_sizers.SizerSlot}
"""

SizerBase = None
"""\
Shortcut for L{edit_sizers.SizerBase}
"""

_builder = None
"""\
Shortcut for L{edit_sizers._builder}
"""

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
