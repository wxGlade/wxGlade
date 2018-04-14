"""
Lisp generator functions for the various wxSizerS

@copyright: 2002-2004 D.H. aka crazyinsomniac on sourceforge.net
@copyright: 2013-2016 Carsten Grohmann
@copyright: 2017 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
from .edit_sizers import BaseSizerBuilder


class BaseLispSizerBuilder(BaseSizerBuilder):
    "Lisp base class for all sizer code generators"

    language = 'lisp'

    tmpl_SetSizer = '(wxWindow_SetSizer %(parent_widget)s (%(sizer_name)s obj))\n'
    tmpl_Fit = '(wxSizer_Fit (%(sizer_name)s obj) %(parent_widget)s)\n'
    tmpl_SetSizeHints = '(wxSizer_SetSizeHints (slot-%(sizer_name)s obj) %(parent_widget)s)\n'

    tmpl_wparent = '(slot-frame obj)'
    """Only in Lisp the widgets parent statement differs between
    C{slot-frame obj} and C{slot-top-window obj}.

    @todo: Clarify why the widget parent differs between different sizers in Lisp.

    see: L{_get_wparent()}
    """

    def _get_wparent(self, obj):
        while obj.is_sizer:
            obj = obj.node.parent.widget
        if not obj.is_toplevel:
            parent = '(slot-%s obj)' % self.codegen._format_name(obj.name)
        else:
            parent = self.tmpl_wparent
        return parent


class LispBoxSizerBuilder(BaseLispSizerBuilder):
    klass = 'wxBoxSizer'

    tmpl = '(setf (%(sizer_name)s obj) (wxBoxSizer_Create %(orient)s))\n'

    tmpl_wparent = '(slot-top-window obj)'

class LispWrapSizerBuilder(LispBoxSizerBuilder):
    klass = 'wxWrapSizer'
    tmpl = '(setf (%(sizer_name)s obj) (wxWrapSizer_Create %(orient)s))\n'


class LispStaticBoxSizerBuilder(BaseLispSizerBuilder):
    klass = 'wxStaticBoxSizer'
    tmpl = '(setf (%(sizer_name)s obj) (StaticBoxSizer_Create (wxStaticBox:wxStaticBox_Create %(parent_widget)s %(label)s) %(orient)s))\n'


class LispGridSizerBuilder(BaseLispSizerBuilder):
    klass = 'wxGridSizer'
    tmpl = '(setf (%(sizer_name)s obj) (wxGridSizer_Create %(rows)s %(cols)s %(vgap)s %(hgap)s))\n'


class LispFlexGridSizerBuilder(LispGridSizerBuilder):
    klass = 'wxFlexGridSizer'

    tmpl_AddGrowableRow = '(wxFlexGridSizer_AddGrowableRow (%(sizer_name)s obj) %(row)s)\n'
    tmpl_AddGrowableCol = '(wxFlexGridSizer_AddGrowableCol (%(sizer_name)s obj) %(col)s)\n'


class LispGridBagSizerBuilder(LispGridSizerBuilder):
    klass = 'wxGridBagSizer'
    tmpl = '(setf (%(sizer_name)s obj) (wxGridSizer_Create %(vgap)s %(hgap)s))\n'


import wcodegen

class LispSizerSlotGenerator(wcodegen.LispWidgetCodeWriter):
    # spacers and empty sizer slots are generally handled by a hack:
    # The the implementations of add_sizeritem() contains more details.
    # The code generation code is already implemented in base class.
    pass


def initialize():
    cn = common.class_names
    cn['EditBoxSizer'] = 'wxBoxSizer'
    cn['EditWrapSizer'] = 'wxWrapSizer'
    cn['EditStaticBoxSizer'] = 'wxStaticBoxSizer'
    cn['EditGridSizer'] = 'wxGridSizer'
    cn['EditFlexGridSizer'] = 'wxFlexGridSizer'
    cn['EditGridBagSizer'] = 'wxGridBagSizer'

    lispgen = common.code_writers.get("lisp")
    if lispgen:
        awh = lispgen.add_widget_handler
        awh('wxBoxSizer', LispBoxSizerBuilder())
        awh('wxWrapSizer', LispWrapSizerBuilder())
        awh('wxStaticBoxSizer', LispStaticBoxSizerBuilder())
        awh('wxGridSizer', LispGridSizerBuilder())
        awh('wxFlexGridSizer', LispFlexGridSizerBuilder())
        awh('wxGridBagSizer', LispGridBagSizerBuilder())
        
    common.register('lisp', "sizerslot", LispSizerSlotGenerator("sizerslot"))
