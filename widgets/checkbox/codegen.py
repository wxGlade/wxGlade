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
    common.class_names['EditCheckBox'] = 'wxCheckBox'

    pygen = common.code_writers.get("python")
    if pygen:
        pygen.add_widget_handler('wxCheckBox', PythonCheckBoxGenerator())
    cppgen = common.code_writers.get('C++')
    if cppgen:
        cppgen.add_widget_handler('wxCheckBox', CppCheckBoxGenerator())
