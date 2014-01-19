"""\
Code generator functions for wxTextCtrl objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PythonTextCtrlGenerator(wcodegen.PythonWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s(%(parent)s, %(id)s, %(value)s%(style)s)\n'

# end of class PythonTextCtrlGenerator


class CppCodeGenerator(wcodegen.CppWidgetCodeWriter):
    tmpl = '%(name)s = new %(klass)s(%(parent)s, %(id)s, ' \
           '%(value)s%(style)s);\n'

# end of class CppCodeGenerator


def initialize():
    common.class_names['EditTextCtrl'] = 'wxTextCtrl'

    pygen = common.code_writers.get('python')
    if pygen:
        pygen.add_widget_handler('wxTextCtrl', PythonTextCtrlGenerator())
    cppgen = common.code_writers.get('C++')
    if cppgen:
        cppgen.add_widget_handler('wxTextCtrl', CppCodeGenerator())
