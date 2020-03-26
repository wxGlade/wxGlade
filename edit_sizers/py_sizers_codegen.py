"""
Python generator functions for the various wxSizers

@copyright: 2002-2007 Alberto Griggio
@copyright: 2013-2016 Carsten Grohmann
@copyright: 2019 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


import common
from .edit_sizers import BaseSizerBuilder, SlotGenerator


class BasePythonSizerBuilder(BaseSizerBuilder):
    "Python base class for all sizer code generators"

    language = 'python'

    tmpl_SetSizer = '%(parent_widget)s.SetSizer(%(sizer_name)s)\n'
    tmpl_Fit = '%(sizer_name)s.Fit(%(parent_widget)s)\n'
    tmpl_Realize = '%(sizer_name)s.Realize()\n'
    tmpl_SetSizeHints = '%(sizer_name)s.SetSizeHints(%(parent_widget)s)\n'

    def _get_wparent(self, obj):
        window = obj.parent_window
        if window.IS_CLASS:
            return 'self'
        return 'self.%s' % window.name


class PythonBoxSizerBuilder(BasePythonSizerBuilder):
    tmpl = '%(sizer_name)s = %(klass)s(%(orient)s)\n'


class PythonStdDialogButtonSizerBuilder(BasePythonSizerBuilder):
    tmpl = '%(sizer_name)s = %(klass)s()\n'


class PythonWrapSizerBuilder(PythonBoxSizerBuilder):
    pass


class PythonStaticBoxSizerBuilder(BasePythonSizerBuilder):
    tmpl = '%(sizer_name)s = %(klass)s(wx.StaticBox(%(parent_widget)s, %(wxIDANY)s, %(label)s), %(orient)s)\n'


class PythonGridSizerBuilder(BasePythonSizerBuilder):
    tmpl = '%(sizer_name)s = %(klass)s(%(rows)s, %(cols)s, %(vgap)s, %(hgap)s)\n'


class PythonFlexGridSizerBuilder(PythonGridSizerBuilder):
    tmpl_AddGrowableRow = '%(sizer_name)s.AddGrowableRow(%(row)s)\n'
    tmpl_AddGrowableCol = '%(sizer_name)s.AddGrowableCol(%(col)s)\n'


class PythonGridBagSizerBuilder(PythonFlexGridSizerBuilder):
    tmpl = '%(sizer_name)s = %(klass)s(%(vgap)s, %(hgap)s)\n'


def initialize():
    cn = common.class_names
    cn['EditBoxSizer'] = 'wxBoxSizer'
    cn['EditStdDialogButtonSizer'] = 'wxStdDialogButtonSizer'
    cn['EditWrapSizer'] = 'wxWrapSizer'
    cn['EditStaticBoxSizer'] = 'wxStaticBoxSizer'
    cn['EditGridSizer'] = 'wxGridSizer'
    cn['EditFlexGridSizer'] = 'wxFlexGridSizer'
    cn['EditGridBagSizer'] = 'wxGridBagSizer'

    pygen = common.code_writers.get("python")
    if pygen:
        awh = pygen.register_widget_code_generator
        awh('wxBoxSizer', PythonBoxSizerBuilder())
        awh('wxStdDialogButtonSizer', PythonStdDialogButtonSizerBuilder())
        awh('wxWrapSizer', PythonWrapSizerBuilder())
        awh('wxStaticBoxSizer', PythonStaticBoxSizerBuilder())
        awh('wxGridSizer', PythonGridSizerBuilder())
        awh('wxFlexGridSizer', PythonFlexGridSizerBuilder())
        awh('wxGridBagSizer', PythonGridBagSizerBuilder())

    common.register('python', "sizerslot", SlotGenerator("python"))
