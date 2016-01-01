"""
Sizers module initialization

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import config

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


def init_gui():
    if not config.use_gui:
        return

    import edit_sizers
    global Sizer, SizerSlot, SizerBase, _builder
    Sizer = edit_sizers.Sizer
    SizerSlot = edit_sizers.SizerSlot
    SizerBase = edit_sizers.SizerBase
    _builder = edit_sizers._builder
    return edit_sizers.init_all()
