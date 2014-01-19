"""\
Code generator functions for wxToggleButton objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PythonToggleButtonGenerator(wcodegen.PythonWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s(%(parent)s, %(id)s, %(label)s)\n'
    has_setvalue = True

# end of class PythonToggleButtonGenerator


class CppToggleButtonGenerator(wcodegen.CppWidgetCodeWriter):
    extra_headers = ['<wx/tglbtn.h>']
    tmpl = '%(name)s = new %(klass)s(%(parent)s, %(id)s, %(label)s);\n'
    has_setvalue = True

# end of class CppToggleButtonGenerator


def initialize():
    common.class_names['EditToggleButton'] = 'wxToggleButton'

    pygen = common.code_writers.get('python')
    if pygen:
        pygen.add_widget_handler(
            'wxToggleButton',
            PythonToggleButtonGenerator()
        )
    cppgen = common.code_writers.get('C++')
    if cppgen:
        cppgen.add_widget_handler('wxToggleButton', CppToggleButtonGenerator())
