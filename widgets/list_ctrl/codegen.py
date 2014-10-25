"""\
Code generator functions for wxListCtrl objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PythonListCtrlGenerator(wcodegen.PythonWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s(%(parent)s, %(id)s%(style)s)\n'

# end of class PythonListCtrlGenerator


class CppListCtrlGenerator(wcodegen.CppWidgetCodeWriter):
    extra_headers = ['<wx/listctrl.h>']
    tmpl = '%(name)s = new %(klass)s(%(parent)s, %(id)s%(style)s);\n'

    def get_events(self, obj):
        return self.codegen.get_events_with_type(obj, 'wxListEvent')

# end of class CppListCtrlGenerator


def initialize():
    klass = 'wxListCtrl'
    common.class_names['EditListCtrl'] = klass
    common.register('python', klass, PythonListCtrlGenerator(klass))
    common.register('C++', klass, CppListCtrlGenerator(klass))
