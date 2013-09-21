"""
Perl generator functions for the various wxSizerS

@copyright: 2002-2004 D.H. aka crazyinsomniac on sourceforge.net
@copyright: 2013 Carsten Grohmann <mail@carstengrohmann.de>
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


import common
from edit_sizers import BaseSizerBuilder


class BasePerlSizerBuilder(BaseSizerBuilder):
    """\
    Perl base class for all sizer code generators
    """

    language = 'perl'

    tmpl_SetSizer = '%(parent_widget)s->SetSizer(%(sizer_name)s);\n'
    tmpl_Fit = '%(sizer_name)s->Fit(%(parent_widget)s);\n'
    tmpl_SetSizeHints = '%(sizer_name)s->SetSizeHints(' \
                        '%(parent_widget)s);\n'
    tmpl_StaticBox = '$self->{%s_staticbox}'

    def _get_wparent(self, obj):
        if not obj.parent.is_toplevel:
            parent = '$self->{%s}' % obj.parent.name
        else:
            parent = '$self'
        return parent

# end of class BasePerlSizerBuilder


class PerlBoxSizerBuilder(BasePerlSizerBuilder):
    klass = 'wxBoxSizer'
    init_stmt = [
        '%(sizer_name)s = %(klass)s->new(%(orient)s);\n'
        ]

# end of class PerlBoxSizerBuilder


class PerlStaticBoxSizerBuilder(BasePerlSizerBuilder):
    klass = 'wxStaticBoxSizer'
    init_stmt = [
        '%(staticbox_name)s = %(wxStaticBox)s->new('
            '%(parent_widget)s, wxID_ANY, %(label)s );\n',
        '%(sizer_name)s = %(klass)s->new(%(staticbox_name)s, %(orient)s);\n',
        '%(staticbox_name)s->Lower();\n',
        ]

# end of class PerlStaticBoxSizerBuilder


class PerlGridSizerBuilder(BasePerlSizerBuilder):
    klass = 'wxGridSizer'
    init_stmt = [
        '%(sizer_name)s = %(klass)s->new(%(rows)s, %(cols)s, '
            '%(vgap)s, %(hgap)s);\n'
        ]

# end of class PerlGridSizerBuilder


class PerlFlexGridSizerBuilder(PerlGridSizerBuilder):
    klass = 'wxFlexGridSizer'

    tmpl_AddGrowableRow = '%(sizer_name)s->AddGrowableRow(%(row)s);\n'
    tmpl_AddGrowableCol = '%(sizer_name)s->AddGrowableCol(%(col)s);\n'

# end of class PerlFlexGridSizerBuilder


def initialize():
    cn = common.class_names
    cn['EditBoxSizer'] = 'wxBoxSizer'
    cn['EditStaticBoxSizer'] = 'wxStaticBoxSizer'
    cn['EditGridSizer'] = 'wxGridSizer'
    cn['EditFlexGridSizer'] = 'wxFlexGridSizer'

    plgen = common.code_writers.get("perl")
    if plgen:
        awh = plgen.add_widget_handler
        awh('wxBoxSizer', PerlBoxSizerBuilder())
        awh('wxStaticBoxSizer', PerlStaticBoxSizerBuilder())
        awh('wxGridSizer', PerlGridSizerBuilder())
        awh('wxFlexGridSizer', PerlFlexGridSizerBuilder())
