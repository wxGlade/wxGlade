"""
Lisp generator functions for the various wxSizerS

@copyright: 2002-2004 D.H. aka crazyinsomniac on sourceforge.net
@copyright: 2013-2016 Carsten Grohmann
@copyright: 2017-2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
from .edit_sizers import BaseSizerBuilder, SlotGenerator


class BaseLispSizerBuilder(BaseSizerBuilder):
    "Lisp base class for all sizer code generators"

    language = 'lisp'

    tmpl_SetSizer = '(wxWindow_SetSizer %(parent_widget)s (%(sizer_name)s obj))\n'
    tmpl_Fit = '(wxSizer_Fit (%(sizer_name)s obj) %(parent_widget)s)\n'
    tmpl_Realize = '(wxSizer_Realize (%(sizer_name)s obj))\n'
    tmpl_SetSizeHints = '(wxSizer_SetSizeHints (slot-%(sizer_name)s obj) %(parent_widget)s)\n'

    tmpl_wparent = '(slot-frame obj)'
    """Only in Lisp the widgets parent statement differs between 'slot-frame obj' and 'slot-top-window obj'.

    todo: Clarify why the widget parent differs between different sizers in Lisp.

    see: _get_wparent()"""

    def _get_wparent(self, obj):
        window = obj.parent_window
        if not window.IS_CLASS:
            parent = '(slot-%s obj)' % self.codegen._format_name(window.name)
        else:
            parent = self.tmpl_wparent
        return parent


    def get_code_per_child(self, obj, child):
        """Returns code that will be inserted after the child code; e.g. for adding element to a sizer.
        It's placed before the final code returned from get_code()."""

        if child.WX_CLASS in ("spacer","sizerslot"):  # spacer and slot are adding itself to the sizer
            return []
        obj_name = self.codegen._format_classattr(child)
        sizer_name = self.codegen._format_classattr(obj)

        flag = child.properties["flag"].get_string_value()  # as string, joined with "|"
        flag = self.codegen.cn_f(flag) or '0'


        if obj.WX_CLASS=="wxStdDialogButtonSizer" and child.WX_CLASS=='wxButton':
            # XXX optionally use SetAffirmativeButton, SetCancelButton, SetNegativeButton
            if child.check_prop("stockitem"):
                tmpl_sizeritem = '(wxSizer_AddButton (%s obj) (%s obj))\n'
                return [tmpl_sizeritem % ( sizer_name, obj_name )]

        if child.IS_SIZER:
            tmpl_sizeritem = '(wxSizer_AddSizer (%s obj) (%s obj) %s %s %s nil)\n'
        else:
            tmpl_sizeritem = '(wxSizer_AddWindow (%s obj) (%s obj) %s %s %s nil)\n'
        stmt = tmpl_sizeritem % ( sizer_name, obj_name, child.proportion, flag, child.border )

        return [stmt]


class LispBoxSizerBuilder(BaseLispSizerBuilder):
    tmpl = '(setf (%(sizer_name)s obj) (wxBoxSizer_Create %(orient)s))\n'
    tmpl_wparent = '(slot-top-window obj)'


class LispWrapSizerBuilder(LispBoxSizerBuilder):
    tmpl = '(setf (%(sizer_name)s obj) (wxWrapSizer_Create %(orient)s))\n'


class LispStaticBoxSizerBuilder(BaseLispSizerBuilder):
    tmpl = '(setf (%(sizer_name)s obj) (StaticBoxSizer_Create (wxStaticBox:wxStaticBox_Create %(parent_widget)s %(label)s) %(orient)s))\n'


class LispGridSizerBuilder(BaseLispSizerBuilder):
    tmpl = '(setf (%(sizer_name)s obj) (wxGridSizer_Create %(rows)s %(cols)s %(vgap)s %(hgap)s))\n'


class LispFlexGridSizerBuilder(LispGridSizerBuilder):
    tmpl_AddGrowableRow = '(wxFlexGridSizer_AddGrowableRow (%(sizer_name)s obj) %(row)s)\n'
    tmpl_AddGrowableCol = '(wxFlexGridSizer_AddGrowableCol (%(sizer_name)s obj) %(col)s)\n'


class LispGridBagSizerBuilder(LispGridSizerBuilder):
    tmpl = '(setf (%(sizer_name)s obj) (wxGridSizer_Create %(vgap)s %(hgap)s))\n'


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
        awh = lispgen.register_widget_code_generator
        awh('wxBoxSizer', LispBoxSizerBuilder())
        awh('wxWrapSizer', LispWrapSizerBuilder())
        awh('wxStaticBoxSizer', LispStaticBoxSizerBuilder())
        awh('wxGridSizer', LispGridSizerBuilder())
        awh('wxFlexGridSizer', LispFlexGridSizerBuilder())
        awh('wxGridBagSizer', LispGridBagSizerBuilder())

    common.register('lisp', "sizerslot", SlotGenerator("lisp"))
