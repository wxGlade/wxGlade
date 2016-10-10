"""
Perl generator functions for the various wxSizerS

@copyright: 2002-2004 D.H. aka crazyinsomniac on sourceforge.net
@copyright: 2013-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


import common
from .edit_sizers import BaseSizerBuilder


class BasePerlSizerBuilder(BaseSizerBuilder):
    "Perl base class for all sizer code generators"

    language = 'perl'

    tmpl_SetSizer = '%(parent_widget)s->SetSizer(%(sizer_name)s);\n'
    tmpl_Fit = '%(sizer_name)s->Fit(%(parent_widget)s);\n'
    tmpl_SetSizeHints = '%(sizer_name)s->SetSizeHints(' \
                        '%(parent_widget)s);\n'

    def _get_wparent(self, obj):
        if not obj.parent.is_toplevel:
            parent = '$self->{%s}' % obj.parent.name
        else:
            parent = '$self'
        return parent



class PerlBoxSizerBuilder(BasePerlSizerBuilder):
    klass = 'wxBoxSizer'
    tmpl = '%(sizer_name)s = %(klass)s->new(%(orient)s);\n'



class PerlStaticBoxSizerBuilder(BasePerlSizerBuilder):
    klass = 'wxStaticBoxSizer'
    tmpl = '%(sizer_name)s = %(klass)s->new(Wx::StaticBox->new(%(parent_widget)s, wxID_ANY, %(label)s), %(orient)s);\n'



class PerlGridSizerBuilder(BasePerlSizerBuilder):
    klass = 'wxGridSizer'
    tmpl = '%(sizer_name)s = %(klass)s->new(%(rows)s, %(cols)s, %(vgap)s, %(hgap)s);\n'



class PerlFlexGridSizerBuilder(PerlGridSizerBuilder):
    klass = 'wxFlexGridSizer'

    tmpl_AddGrowableRow = '%(sizer_name)s->AddGrowableRow(%(row)s);\n'
    tmpl_AddGrowableCol = '%(sizer_name)s->AddGrowableCol(%(col)s);\n'


import wcodegen

class PerlSizerSlotGenerator(wcodegen.PerlWidgetCodeWriter):
    # spacers and empty sizer slots are generally handled by a hack:
    # The the implementations of add_sizeritem() contains more details.
    # The code generation code is already implemented in base class.
    pass


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

    common.register('perl', "sizerslot", PerlSizerSlotGenerator("sizerslot"))
