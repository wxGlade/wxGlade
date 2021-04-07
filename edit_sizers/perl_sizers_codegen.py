"""
Perl generator functions for the various wxSizerS

@copyright: 2002-2004 D.H. aka crazyinsomniac on sourceforge.net
@copyright: 2013-2016 Carsten Grohmann
@copyright: 2017-2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
from .edit_sizers import BaseSizerBuilder, SlotGenerator


class BasePerlSizerBuilder(BaseSizerBuilder):
    "Perl base class for all sizer code generators"
    language = 'perl'

    tmpl_SetSizer = '%(parent_widget)s->SetSizer(%(sizer_name)s);\n'
    tmpl_Fit = '%(sizer_name)s->Fit(%(parent_widget)s);\n'
    tmpl_Realize = '%(sizer_name)s->Realize();\n'
    tmpl_SetSizeHints = '%(sizer_name)s->SetSizeHints(%(parent_widget)s);\n'

    def _get_wparent(self, obj):
        window = obj.parent_window
        if not window.IS_CLASS:
            parent = '$self->{%s}' % window.name
        else:
            parent = '$self'
        return parent


class PerlBoxSizerBuilder(BasePerlSizerBuilder):
    tmpl = '%(sizer_name)s = %(klass)s->new(%(orient)s);\n'


class PerlWrapSizerBuilder(PerlBoxSizerBuilder):
    pass


class PerlStaticBoxSizerBuilder(BasePerlSizerBuilder):
    tmpl = '%(sizer_name)s = %(klass)s->new(Wx::StaticBox->new(%(parent_widget)s, wxID_ANY, %(label)s), %(orient)s);\n'


class PerlGridSizerBuilder(BasePerlSizerBuilder):
    tmpl = '%(sizer_name)s = %(klass)s->new(%(rows)s, %(cols)s, %(vgap)s, %(hgap)s);\n'


class PerlFlexGridSizerBuilder(PerlGridSizerBuilder):
    tmpl_AddGrowableRow = '%(sizer_name)s->AddGrowableRow(%(row)s);\n'
    tmpl_AddGrowableCol = '%(sizer_name)s->AddGrowableCol(%(col)s);\n'


class PerlGridBagSizerBuilder(PerlFlexGridSizerBuilder):
    tmpl = '%(sizer_name)s = %(klass)s->new(%(vgap)s, %(hgap)s);\n'


def initialize():
    cn = common.class_names
    cn['EditBoxSizer'] = 'wxBoxSizer'
    cn['EditWrapSizer'] = 'wxWrapSizer'
    cn['EditStaticBoxSizer'] = 'wxStaticBoxSizer'
    cn['EditGridSizer'] = 'wxGridSizer'
    cn['EditFlexGridSizer'] = 'wxFlexGridSizer'
    cn['EditGridBagSizer'] = 'wxGridBagSizer'

    plgen = common.code_writers.get("perl")
    if plgen:
        awh = plgen.register_widget_code_generator
        awh('wxBoxSizer', PerlBoxSizerBuilder())
        awh('wxWrapSizer', PerlWrapSizerBuilder())
        awh('wxStaticBoxSizer', PerlStaticBoxSizerBuilder())
        awh('wxGridSizer', PerlGridSizerBuilder())
        awh('wxFlexGridSizer', PerlFlexGridSizerBuilder())
        awh('wxGridBagSizer', PerlGridBagSizerBuilder())

    common.register('perl', "sizerslot", SlotGenerator("perl"))