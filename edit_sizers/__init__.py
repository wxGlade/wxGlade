"""
Sizers module initialization

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import config

Sizer = None          # Shortcut for edit_sizers.Sizer
SizerSlot = None      # Shortcut for edit_sizers.SizerSlot
SizerBase = None      # Shortcut for edit_sizers.SizerBase
GridSizerBase = None  # Shortcut for edit_sizers.GridSizerBase
_builder = None       # Shortcut for edit_sizers._builder


def init_gui():
    #if not config.use_gui:
    #    return {}

    from . import edit_sizers
    global Sizer, SizerSlot, SizerBase, GridSizerBase, _builder
    Sizer = edit_sizers.Sizer
    SizerSlot = edit_sizers.SizerSlot
    SizerBase = edit_sizers.SizerBase
    GridSizerBase = edit_sizers.GridSizerBase
    _builder = edit_sizers._builder
    return edit_sizers.init_all()
