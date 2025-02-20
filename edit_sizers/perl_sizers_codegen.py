"""
Perl generator functions for the various wxSizerS

@copyright: 2002-2004 D.H. aka crazyinsomniac on sourceforge.net
@copyright: 2013-2016 Carsten Grohmann
@copyright: 2017-2025 Dietmar Schwertberger
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
        parent = obj.get_parent_window2(self.codegen)
        if parent.IS_SIZER:
            sizer_access = self.codegen.format_generic_access(parent)
            return '%s->GetStaticBox()' % sizer_access
        if parent.IS_CLASS:
            return '$self'
        return '$self->{%s}' % parent.name


class PerlBoxSizerBuilder(BasePerlSizerBuilder):
    tmpl = '%(sizer_name)s = %(klass)s->new(%(orient)s);\n'


class PerlStdDialogButtonSizerBuilder(BasePerlSizerBuilder):
    tmpl = '%(sizer_name)s = %(klass)s->new();\n'


class PerlWrapSizerBuilder(PerlBoxSizerBuilder):
    pass


class PerlStaticBoxSizerBuilder(BasePerlSizerBuilder):
    tmpl = '%(sizer_name)s = %(klass)s->new(Wx::StaticBox->new(%(parent_widget)s, wxID_ANY, %(label)s), %(orient)s);\n'


class PerlGridSizerBuilder(BasePerlSizerBuilder):
    tmpl = '%(sizer_name)s = %(klass)s->new(%(rows)s, %(cols)s, %(vgap)s, %(hgap)s);\n'


class PerlFlexGridSizerBuilder(PerlGridSizerBuilder):
    tmpl_AddGrowableRow = '%(sizer_name)s->AddGrowableRow(%(row)s);\n'
    tmpl_AddGrowableCol = '%(sizer_name)s->AddGrowableCol(%(col)s);\n'
    tmpl_AddGrowableRow_proportion = '%(sizer_name)s->AddGrowableRow(%(row)s, %(proportion)s);\n'
    tmpl_AddGrowableCol_proportion = '%(sizer_name)s->AddGrowableCol(%(col)s, %(proportion)s);\n'


class PerlGridBagSizerBuilder(PerlFlexGridSizerBuilder):
    tmpl = '%(sizer_name)s = %(klass)s->new(%(vgap)s, %(hgap)s);\n'


def initialize():
    cn = common.class_names
    cn['EditBoxSizer'] = 'wxBoxSizer'
    cn['EditStdDialogButtonSizer'] = 'wxStdDialogButtonSizer'
    cn['EditWrapSizer'] = 'wxWrapSizer'
    cn['EditStaticBoxSizer'] = 'wxStaticBoxSizer'
    cn['EditGridSizer'] = 'wxGridSizer'
    cn['EditFlexGridSizer'] = 'wxFlexGridSizer'
    cn['EditGridBagSizer'] = 'wxGridBagSizer'

    plgen = common.code_writers.get("perl")
    if plgen:
        awh = plgen.register_widget_code_generator
        awh('wxBoxSizer', PerlBoxSizerBuilder())
        awh('wxStdDialogButtonSizer', PerlStdDialogButtonSizerBuilder())
        awh('wxWrapSizer', PerlWrapSizerBuilder())
        awh('wxStaticBoxSizer', PerlStaticBoxSizerBuilder())
        awh('wxGridSizer', PerlGridSizerBuilder())
        awh('wxFlexGridSizer', PerlFlexGridSizerBuilder())
        awh('wxGridBagSizer', PerlGridBagSizerBuilder())

    common.register('perl', "sizerslot", SlotGenerator("perl"))
