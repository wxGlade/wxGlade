"""\
Code generator functions for wxBitmapButton objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PythonBitmapButtonGenerator(wcodegen.PythonWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s(%(parent)s, %(id_number)s, %(bitmap)s%(style)s)\n'


class CppBitmapButtonGenerator(wcodegen.CppWidgetCodeWriter):
    tmpl = '%(name)s = new %(klass)s(%(parent)s, %(id_number)s, %(bitmap)s%(style)s);\n'


def initialize():
    klass = 'wxBitmapButton'
    common.class_names['EditBitmapButton'] = klass
    common.register('python', klass, PythonBitmapButtonGenerator(klass))
    common.register('C++', klass, CppBitmapButtonGenerator(klass))
