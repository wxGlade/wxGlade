"""
Lisp generator functions for the various wxSizerS

@copyright: 2002-2004 D.H. aka crazyinsomniac on sourceforge.net
@copyright: 2013 Carsten Grohmann <mail@carstengrohmann.de>
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


import common
from edit_sizers import BaseSizerBuilder


class BaseLispSizerBuilder(BaseSizerBuilder):
    """\
    Lisp base class for all sizer code generators
    """

    language = 'lisp'

    tmpl_SetSizer = '(wxWindow_SetSizer %(parent_widget)s ' \
                    '(%(sizer_name)s obj))\n'
    tmpl_Fit = '(wxSizer_Fit (%(sizer_name)s obj) %(parent_widget)s)\n'
    tmpl_SetSizeHints = '(wxSizer_SetSizeHints (slot-%(sizer_name)s obj) ' \
                        '%(parent_widget)s)\n'

    tmpl_wparent = '(slot-frame obj)'
    """\
    Only in Lisp the widgets parent statement differs between
    C{slot-frame obj} and C{slot-top-window obj}.

    @todo: Clarify why the widget parent differs between different sizers in
           Lisp.

    @see: L{_get_wparent()}
    @type: String
    """

    def _get_wparent(self, obj):
        if not obj.parent.is_toplevel:
            parent = '(slot-%s obj)' % obj.parent.name
        else:
            parent = self.tmpl_wparent
        return parent

# end of class BaseLispSizerBuilder


class LispBoxSizerBuilder(BaseLispSizerBuilder):
    klass = 'wxBoxSizer'

    init_stmt = [
        '(setf (%(sizer_name)s obj) (wxBoxSizer_Create %(orient)s))\n'
        ]

    tmpl_wparent = '(slot-top-window obj)'

# end of class LispBoxSizerBuilder


class LispStaticBoxSizerBuilder(BaseLispSizerBuilder):
    klass = 'wxStaticBoxSizer'
    init_stmt = [
        '(setf (%(sizer_name)s obj) (StaticBoxSizer_Create '
            '(wxStaticBox:wxStaticBox_Create %(parent_widget)s %(label)s) '
            '%(orient)s))\n',
        ]

# end of class LispStaticBoxSizerBuilder


class LispGridSizerBuilder(BaseLispSizerBuilder):
    klass = 'wxGridSizer'
    init_stmt = [
        '(setf (%(sizer_name)s obj) (wxGridSizer_Create %(rows)s '
            '%(cols)s %(vgap)s %(hgap)s))\n',
        ]

# end of class LispGridSizerBuilder


class LispFlexGridSizerBuilder(LispGridSizerBuilder):
    klass = 'wxFlexGridSizer'

    tmpl_AddGrowableRow = '(wxFlexGridSizer_AddGrowableRow ' \
                          '(%(sizer_name)s obj) %(row)s)\n'
    tmpl_AddGrowableCol = '(wxFlexGridSizer_AddGrowableCol ' \
                          '(%(sizer_name)s obj) %(col)s)\n'

# end of class LispFlexGridSizerBuilder


def initialize():
    cn = common.class_names
    cn['EditBoxSizer'] = 'wxBoxSizer'
    cn['EditStaticBoxSizer'] = 'wxStaticBoxSizer'
    cn['EditGridSizer'] = 'wxGridSizer'
    cn['EditFlexGridSizer'] = 'wxFlexGridSizer'

    lispgen = common.code_writers.get("lisp")
    if lispgen:
        awh = lispgen.add_widget_handler
        awh('wxBoxSizer', LispBoxSizerBuilder())
        awh('wxStaticBoxSizer', LispStaticBoxSizerBuilder())
        awh('wxGridSizer', LispGridSizerBuilder())
        awh('wxFlexGridSizer', LispFlexGridSizerBuilder())
