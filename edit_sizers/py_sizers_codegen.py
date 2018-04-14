"""
Python generator functions for the various wxSizerS

@copyright: 2002-2007 Alberto Griggio
@copyright: 2013-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


import common
from .edit_sizers import BaseSizerBuilder


class BasePythonSizerBuilder(BaseSizerBuilder):
    "Python base class for all sizer code generators"

    language = 'python'

    tmpl_SetSizer = '%(parent_widget)s.SetSizer(%(sizer_name)s)\n'
    tmpl_Fit = '%(sizer_name)s.Fit(%(parent_widget)s)\n'
    tmpl_SetSizeHints = '%(sizer_name)s.SetSizeHints(%(parent_widget)s)\n'

    def _get_wparent(self, obj):
        while obj.is_sizer:
            obj = obj.node.parent.widget
        if not obj.is_toplevel:
            parent = 'self.%s' % obj.name
        else:
            parent = 'self'
        return parent



class PythonBoxSizerBuilder(BasePythonSizerBuilder):
    klass = 'wxBoxSizer'
    tmpl = '%(sizer_name)s = %(klass)s(%(orient)s)\n'


class PythonWrapSizerBuilder(PythonBoxSizerBuilder):
    klass = 'wxWrapSizer'


class PythonStaticBoxSizerBuilder(BasePythonSizerBuilder):
    klass = 'wxStaticBoxSizer'
    tmpl = '%(sizer_name)s = %(klass)s(wx.StaticBox(%(parent_widget)s, %(wxIDANY)s, %(label)s), %(orient)s)\n'



class PythonGridSizerBuilder(BasePythonSizerBuilder):
    klass = 'wxGridSizer'
    tmpl = '%(sizer_name)s = %(klass)s(%(rows)s, %(cols)s, %(vgap)s, %(hgap)s)\n'



class PythonFlexGridSizerBuilder(PythonGridSizerBuilder):
    klass = 'wxFlexGridSizer'

    tmpl_AddGrowableRow = '%(sizer_name)s.AddGrowableRow(%(row)s)\n'
    tmpl_AddGrowableCol = '%(sizer_name)s.AddGrowableCol(%(col)s)\n'


class PythonGridBagSizerBuilder(PythonFlexGridSizerBuilder):
    klass = 'wxGridBagSizer'
    tmpl = '%(sizer_name)s = %(klass)s(%(vgap)s, %(hgap)s)\n'


import wcodegen

class PythonSizerSlotGenerator(wcodegen.PythonWidgetCodeWriter):
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

    pygen = common.code_writers.get("python")
    if pygen:
        awh = pygen.add_widget_handler
        awh('wxBoxSizer', PythonBoxSizerBuilder())
        awh('wxWrapSizer', PythonWrapSizerBuilder())
        awh('wxStaticBoxSizer', PythonStaticBoxSizerBuilder())
        awh('wxGridSizer', PythonGridSizerBuilder())
        awh('wxFlexGridSizer', PythonFlexGridSizerBuilder())
        awh('wxGridBagSizer', PythonGridBagSizerBuilder())

    # handle SizerSlot
    #common.class_names['EditSpacer'] = klass
    common.register('python', "sizerslot", PythonSizerSlotGenerator("sizerslot"))
