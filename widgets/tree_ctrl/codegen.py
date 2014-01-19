"""\
Code generator functions for wxTreeCtrl objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PythonTreeCtrlGenerator(wcodegen.PythonWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s(%(parent)s, %(id)s%(style)s)\n'
    default_style = 'wxTR_HAS_BUTTONS'

# end of class PythonTreeCtrlGenerator


class CppCodeGenerator(wcodegen.CppWidgetCodeWriter):
    extra_headers = ['<wx/treectrl.h>']
    tmpl = '%(name)s = new %(klass)s(%(parent)s, %(id)s%(style)s);\n'
    default_style = 'wxTR_HAS_BUTTONS'

# end of class CppCodeGenerator


def initialize():
    common.class_names['EditTreeCtrl'] = 'wxTreeCtrl'

    pygen = common.code_writers.get('python')
    if pygen:
        pygen.add_widget_handler('wxTreeCtrl', PythonTreeCtrlGenerator())
    cppgen = common.code_writers.get('C++')
    if cppgen:
        cppgen.add_widget_handler('wxTreeCtrl', CppCodeGenerator())
