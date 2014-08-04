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
    klass = 'wxToggleButton'
    common.class_names['EditToggleButton'] = klass
    common.register('python', klass, PythonToggleButtonGenerator(klass))
    common.register('C++', klass, CppToggleButtonGenerator(klass))
