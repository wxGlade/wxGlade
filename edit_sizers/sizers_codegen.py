"""
Code generation functions for the various wxSizerS

@copyright: 2002-2007 Alberto Griggio <agriggio@users.sourceforge.net>
@copyright: 2013 Carsten Grohmann <mail@carstengrohmann.de>
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


import common
from edit_sizers import BaseSizerBuilder


class BasePythonSizerBuilder(BaseSizerBuilder):
    """\
    Python base class for all sizer code generators
    """

    language = 'python'

    tmpl_SetSizer = '%(parent_widget)s.SetSizer(%(sizer_name)s)\n'
    tmpl_Fit = '%(sizer_name)s.Fit(%(parent_widget)s)\n'
    tmpl_SetSizeHints = '%(sizer_name)s.SetSizeHints(%(parent_widget)s)\n'
    tmpl_StaticBox = 'self.%s_staticbox'

    def _get_wparent(self, obj):
        if not obj.parent.is_toplevel:
            parent = 'self.%s' % obj.parent.name
        else:
            parent = 'self'
        return parent

# end of class BasePythonSizerBuilder


class BaseCPPSizerBuilder(BaseSizerBuilder):
    """\
    C++ base class for all sizer code generators
    """

    language = 'C++'

    tmpl_SetSizer = '%(parent_ref)sSetSizer(%(sizer_name)s);\n'
    tmpl_Fit = '%(sizer_name)s->Fit(%(parent_widget)s);\n'
    tmpl_SetSizeHints = '%(sizer_name)s->SetSizeHints(%(parent_widget)s);\n'
    tmpl_StaticBox = '%s_staticbox'

    def _get_wparent(self, obj):
        if not obj.parent.is_toplevel:
            parent = '%s' % obj.parent.name
        else:
            parent = 'this'
        return parent

    def _get_parent_ref(self, obj):
        if not obj.parent.is_toplevel:
            parent_ref = '%s->' % obj.parent.name
        else:
            parent_ref = ''
        return parent_ref

    def _get_code(self, obj):
        self.props_get_code['parent_ref'] = self._get_parent_ref(obj)
        result = BaseSizerBuilder._get_code(self, obj)

        # get_code() for C++ has different return values
        result = list(result)
        result.insert(2, [])
        return result

# end of class BaseCPPSizerBuilder


class PythonBoxSizerBuilder(BasePythonSizerBuilder):
    klass = 'wxBoxSizer'
    init_stmt = [
        '%(sizer_name)s = %(klass)s(%(orient)s)\n',
        ]

# end of class PythonBoxSizerBuilder


class PythonStaticBoxSizerBuilder(BasePythonSizerBuilder):
    klass = 'wxStaticBoxSizer'
    init_stmt = [
        '%(staticbox_name)s = %(wxStaticBox)s(%(parent_widget)s, '
            '%(wxIDANY)s, %(label)s)\n',
        '%(sizer_name)s = %(klass)s(%(staticbox_name)s, %(orient)s)\n',
        '%(staticbox_name)s.Lower()\n',
        ]

# end of class PythonStaticBoxSizerBuilder


class PythonGridSizerBuilder(BasePythonSizerBuilder):
    klass = 'wxGridSizer'
    init_stmt = [
        '%(sizer_name)s = %(klass)s(%(rows)s, %(cols)s, '
            '%(vgap)s, %(hgap)s)\n',
        ]

# end of class PythonGridSizerBuilder


class PythonFlexGridSizerBuilder(PythonGridSizerBuilder):
    klass = 'wxFlexGridSizer'

    tmpl_AddGrowableRow = '%(sizer_name)s.AddGrowableRow(%(row)s)\n'
    tmpl_AddGrowableCol = '%(sizer_name)s.AddGrowableCol(%(col)s)\n'

# end of class PythonFlexGridSizerBuilder


class CppBoxSizerBuilder(BaseCPPSizerBuilder):
    klass = 'wxBoxSizer'
    init_stmt = [
        '%(klass)s* %(sizer_name)s = new %(klass)s(%(orient)s);\n',
        ]

# end of class CppBoxSizerBuilder


class CppStaticBoxSizerBuilder(BaseCPPSizerBuilder):
    klass = 'wxStaticBoxSizer'
    init_stmt = [
        '%(staticbox_name)s = new wxStaticBox(%(parent_widget)s, '
            'wxID_ANY, %(label)s);\n',
        '%(klass)s* %(sizer_name)s = new %(klass)s(%(staticbox_name)s, '
            '%(orient)s);\n',
        '%(staticbox_name)s->Lower();\n'
        ]

# end of class CppStaticBoxSizerBuilder


class CppGridSizerBuilder(BaseCPPSizerBuilder):
    klass = 'wxGridSizer'
    init_stmt = [
        '%(klass)s* %(sizer_name)s = new %(klass)s(%(rows)s, %(cols)s, '
            '%(vgap)s, %(hgap)s);\n',
        ]

# end of class CppGridSizerBuilder


class CppFlexGridSizerBuilder(CppGridSizerBuilder):
    klass = 'wxFlexGridSizer'

    tmpl_AddGrowableRow = '%(sizer_name)s->AddGrowableRow(%(row)s);\n'
    tmpl_AddGrowableCol = '%(sizer_name)s->AddGrowableCol(%(col)s);\n'

# end of class CppFlexGridSizerBuilder


def xrc_wxFlexGridSizer_builder(obj):
    xrcgen = common.code_writers['XRC']

    class FlexGridSizerXrcObject(xrcgen.DefaultXrcObject):
        def write_property(self, name, val, outfile, tabs):
            if val and name in ('growable_rows', 'growable_cols'):
                if name == 'growable_rows':
                    name2 = 'growablerows'
                else:
                    name2 = 'growablecols'
                outfile.write('    ' * tabs + '<%s>%s</%s>\n' %
                                  (name2, val, name2))
            else:
                xrcgen.DefaultXrcObject.write_property(self, name, val,
                                                       outfile, tabs)

    # end of class FlexGridSizerXrcObject

    return FlexGridSizerXrcObject(obj)


def initialize():
    cn = common.class_names
    cn['EditBoxSizer'] = 'wxBoxSizer'
    cn['EditStaticBoxSizer'] = 'wxStaticBoxSizer'
    cn['EditGridSizer'] = 'wxGridSizer'
    cn['EditFlexGridSizer'] = 'wxFlexGridSizer'

    pygen = common.code_writers.get("python")
    if pygen:
        awh = pygen.add_widget_handler
        awh('wxBoxSizer', PythonBoxSizerBuilder())
        awh('wxStaticBoxSizer', PythonStaticBoxSizerBuilder())
        awh('wxGridSizer', PythonGridSizerBuilder())
        awh('wxFlexGridSizer', PythonFlexGridSizerBuilder())
    cppgen = common.code_writers.get("C++")
    if cppgen:
        awh = cppgen.add_widget_handler
        awh('wxBoxSizer', CppBoxSizerBuilder())
        awh('wxStaticBoxSizer', CppStaticBoxSizerBuilder())
        awh('wxGridSizer', CppGridSizerBuilder())
        awh('wxFlexGridSizer', CppFlexGridSizerBuilder())
    xrcgen = common.code_writers.get("XRC")
    if xrcgen:
        xrcgen.add_widget_handler('wxFlexGridSizer',
                                  xrc_wxFlexGridSizer_builder)
