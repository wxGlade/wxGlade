"""\
Code generator functions for wxCheckBox objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PythonCheckBoxGenerator(wcodegen.PythonWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s(%(parent)s, %(id)s, ' \
           '%(label)s%(style)s)\n'

    def _prepare_tmpl_content(self, obj):
        wcodegen.PythonWidgetCodeWriter._prepare_tmpl_content(self, obj)
        self.has_setvalue1 = obj.properties.get('checked', False)
        return

# end of class PythonCheckBoxGenerator


class CppCheckBoxGenerator(wcodegen.CppWidgetCodeWriter):
    tmpl = '%(name)s = new %(klass)s(%(parent)s, %(id)s, ' \
           '%(label)s%(style)s);\n'

    def _prepare_tmpl_content(self, obj):
        wcodegen.CppWidgetCodeWriter._prepare_tmpl_content(self, obj)
        self.has_setvalue1 = obj.properties.get('checked', False)
        return

# end of class CppCheckBoxGenerator


def initialize():
    klass = 'wxCheckBox'
    common.class_names['EditCheckBox'] = klass
    common.register('python', klass, PythonCheckBoxGenerator(klass))
    common.register('C++', klass, CppCheckBoxGenerator(klass))
