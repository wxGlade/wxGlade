"""\
Code generator functions for wxToggleButton objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PythonToggleButtonGenerator(wcodegen.PythonWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s(%(parent)s, %(id)s, %(label)s%(style)s)\n'
    has_setvalue = True


class CppToggleButtonGenerator(wcodegen.CppWidgetCodeWriter):
    import_modules = ['<wx/tglbtn.h>']
    tmpl = '%(name)s = new %(klass)s(%(parent)s, %(id)s, %(label)s%(style)s);\n'
    has_setvalue = True


def initialize():
    klass = 'wxToggleButton'
    common.class_names['EditToggleButton'] = klass
    common.register('python', klass, PythonToggleButtonGenerator(klass))
    common.register('C++',    klass, CppToggleButtonGenerator(klass))
