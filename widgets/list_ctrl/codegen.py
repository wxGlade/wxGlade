"""\
Code generator functions for wxListCtrl objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PythonListCtrlGenerator(wcodegen.PythonWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s(%(parent)s, %(id)s%(style)s)\n'



class CppListCtrlGenerator(wcodegen.CppWidgetCodeWriter):
    import_modules = ['<wx/listctrl.h>']
    tmpl = '%(name)s = new %(klass)s(%(parent)s, %(id)s%(style)s);\n'



def initialize():
    klass = 'wxListCtrl'
    common.class_names['EditListCtrl'] = klass
    common.register('python', klass, PythonListCtrlGenerator(klass))
    common.register('C++',    klass, CppListCtrlGenerator(klass))
