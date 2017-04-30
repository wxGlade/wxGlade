"""\
Code generator functions for wxTextCtrl objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PythonTextCtrlGenerator(wcodegen.PythonWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s(%(parent)s, %(id)s, %(value)s%(style)s)\n'


class CppTextCtrlGenerator(wcodegen.CppWidgetCodeWriter):
    tmpl = '%(name)s = new %(klass)s(%(parent)s, %(id)s, %(value)s%(style)s);\n'


def initialize():
    klass = 'wxTextCtrl'
    common.class_names['EditTextCtrl'] = klass
    common.register('python', klass, PythonTextCtrlGenerator(klass))
    common.register('C++', klass, CppTextCtrlGenerator(klass))
