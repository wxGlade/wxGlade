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
    default_style = 'wxLC_ICON'

# end of class PythonListCtrlGenerator


class CppListCtrlGenerator(wcodegen.CppWidgetCodeWriter):
    extra_headers = ['<wx/listctrl.h>']
    tmpl = '%(name)s = new %(klass)s(%(parent)s, %(id)s%(style)s);\n'
    default_style = 'wxLC_ICON'

    def get_events(self, obj):
        return self.codegen.get_events_with_type(obj, 'wxListEvent')

# end of class CppListCtrlGenerator


def initialize():
    common.class_names['EditListCtrl'] = 'wxListCtrl'

    pygen = common.code_writers.get('python')
    if pygen:
        pygen.add_widget_handler('wxListCtrl', PythonListCtrlGenerator())
    cppgen = common.code_writers.get('C++')
    if cppgen:
        cppgen.add_widget_handler('wxListCtrl', CppListCtrlGenerator())
