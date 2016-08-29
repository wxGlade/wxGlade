"""\
Code generator functions for wxCheckBox objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

from . import checkbox_base
import common
import wcodegen


class PythonCheckBoxGenerator(wcodegen.PythonWidgetCodeWriter, checkbox_base.CheckBoxMixin):
    tmpl = '%(name)s = %(klass)s(%(parent)s, %(id)s, %(label)s%(style)s)\n'
    tmpl_set3statevalue = '%(name)s.Set3StateValue(%(value_3state)s)\n'

    def _prepare_tmpl_content(self, obj):
        super(PythonCheckBoxGenerator, self)._prepare_tmpl_content(obj)
        self._prepare_checkbox_content(obj)

    def get_code(self, obj):
        init_lines, prop_lines, layout_lines = super(PythonCheckBoxGenerator, self).get_code(obj)
        self._get_checkbox_code(prop_lines)
        return init_lines, prop_lines, layout_lines



class CppCheckBoxGenerator(wcodegen.CppWidgetCodeWriter, checkbox_base.CheckBoxMixin):
    tmpl = '%(name)s = new %(klass)s(%(parent)s, %(id)s, %(label)s%(style)s);\n'
    tmpl_set3statevalue = '%(name)s->Set3StateValue(%(value_3state)s);\n'

    def _prepare_tmpl_content(self, obj):
        super(CppCheckBoxGenerator, self)._prepare_tmpl_content(obj)
        self._prepare_checkbox_content(obj)

    def get_code(self, obj):
        init_lines, id_lines, prop_lines, layout_lines = super(CppCheckBoxGenerator, self).get_code(obj)
        self._get_checkbox_code(prop_lines)
        return init_lines, id_lines, prop_lines, layout_lines


def initialize():
    klass = 'wxCheckBox'
    common.class_names['EditCheckBox'] = klass
    common.register('python', klass, PythonCheckBoxGenerator(klass))
    common.register('C++', klass, CppCheckBoxGenerator(klass))
