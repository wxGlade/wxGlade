"""
C++ generator functions for the various wxSizerS

@copyright: 2014-2016 Carsten Grohmann
@copyright: 2017-2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
from .edit_sizers import BaseSizerBuilder, SlotGenerator


class BaseCPPSizerBuilder(BaseSizerBuilder):
    "C++ base class for all sizer code generators"

    language = 'C++'

    tmpl_SetSizer = '%(parent_ref)sSetSizer(%(sizer_name)s);\n'
    tmpl_Fit = '%(sizer_name)s->Fit(%(parent_widget)s);\n'
    tmpl_Realize = '%(sizer_name)s->Realize();\n'
    tmpl_SetSizeHints = '%(sizer_name)s->SetSizeHints(%(parent_widget)s);\n'

    def _get_wparent(self, obj):
        window = obj.parent_window
        if not window.IS_CLASS:
            parent = '%s' % window.name
        else:
            parent = 'this'
        return parent

    def _get_parent_ref(self, obj):
        if obj.IS_SIZER:
            obj = obj.parent
        if not obj.IS_CLASS:
            parent_ref = '%s->' % obj.parent.name
        else:
            parent_ref = ''
        return parent_ref

    def _get_parent_ref(self, obj):
        if not obj.parent.IS_CLASS:
            parent_ref = '%s->' % obj.parent.name
        else:
            parent_ref = ''
        return parent_ref

    def _get_code(self, obj):
        self.tmpl_dict['parent_ref'] = self._get_parent_ref(obj)
        # get_code() for C++ has different return values
        init, final = BaseSizerBuilder._get_code(self, obj)
        return init, [], final

    def _prepare_tmpl_content(self, obj):
        super(BaseCPPSizerBuilder, self)._prepare_tmpl_content(obj)
        if self.codegen.store_as_attr(obj):
            self.tmpl_dict['assignment'] = '%s' % self.tmpl_dict['sizer_name']
        else:
            self.tmpl_dict['assignment'] = '%s* %s' % (self.tmpl_dict['klass'], self.tmpl_dict['sizer_name'])
        return


class CppBoxSizerBuilder(BaseCPPSizerBuilder):
    klass = 'wxBoxSizer'
    tmpl = '%(assignment)s = new %(klass)s(%(orient)s);\n'


class CppWrapSizerBuilder(CppBoxSizerBuilder):
    import_modules = ['<wx/wrapsizer.h>']
    klass = 'wxWrapSizer'


class CppStaticBoxSizerBuilder(BaseCPPSizerBuilder):
    klass = 'wxStaticBoxSizer'
    tmpl = '%(assignment)s = new %(klass)s(new wxStaticBox(%(parent_widget)s, wxID_ANY, %(label)s), %(orient)s);\n'


class CppGridSizerBuilder(BaseCPPSizerBuilder):
    klass = 'wxGridSizer'
    tmpl = '%(assignment)s = new %(klass)s(%(rows)s, %(cols)s, %(vgap)s, %(hgap)s);\n'


class CppFlexGridSizerBuilder(CppGridSizerBuilder):
    klass = 'wxFlexGridSizer'

    tmpl_AddGrowableRow = '%(sizer_name)s->AddGrowableRow(%(row)s);\n'
    tmpl_AddGrowableCol = '%(sizer_name)s->AddGrowableCol(%(col)s);\n'


class CppGridBagSizerBuilder(CppFlexGridSizerBuilder):
    import_modules = ['<wx/gbsizer.h>']
    klass = 'wxGridBagSizer'
    tmpl = '%(assignment)s = new %(klass)s(%(vgap)s, %(hgap)s);\n'


class CppSizerSlotGenerator(SlotGenerator):
    def get_code(self, obj):
        init, final = SlotGenerator.get_code(self, obj)
        return init, [], final


def initialize():
    cn = common.class_names
    cn['EditBoxSizer'] = 'wxBoxSizer'
    cn['EditWrapSizer'] = 'wxWrapSizer'
    cn['EditStaticBoxSizer'] = 'wxStaticBoxSizer'
    cn['EditGridSizer'] = 'wxGridSizer'
    cn['EditFlexGridSizer'] = 'wxFlexGridSizer'

    cppgen = common.code_writers.get("C++")
    if cppgen:
        awh = cppgen.register_widget_code_generator
        awh('wxBoxSizer', CppBoxSizerBuilder())
        awh('wxWrapSizer', CppWrapSizerBuilder())
        awh('wxStaticBoxSizer', CppStaticBoxSizerBuilder())
        awh('wxGridSizer', CppGridSizerBuilder())
        awh('wxFlexGridSizer', CppFlexGridSizerBuilder())
        awh('wxGridBagSizer', CppGridBagSizerBuilder())

    #common.register('C++', "sizerslot", CppSizerSlotGenerator("sizerslot"))
    common.register('C++', "sizerslot", CppSizerSlotGenerator("C++"))
