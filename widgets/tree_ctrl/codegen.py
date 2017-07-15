"""\
Code generator functions for wxTreeCtrl objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PythonTreeCtrlGenerator(wcodegen.PythonWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s(%(parent)s, %(id)s%(style)s)\n'


class CppTreeCtrlGenerator(wcodegen.CppWidgetCodeWriter):
    import_modules = ['<wx/treectrl.h>']
    tmpl = '%(name)s = new %(klass)s(%(parent)s, %(id)s%(style)s);\n'


def initialize():
    klass = 'wxTreeCtrl'
    common.class_names['EditTreeCtrl'] = klass
    common.register('python', klass, PythonTreeCtrlGenerator(klass))
    common.register('C++',    klass, CppTreeCtrlGenerator(klass))
