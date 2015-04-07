"""\
Code generator functions for wxTreeCtrl objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2015 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PythonTreeCtrlGenerator(wcodegen.PythonWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s(%(parent)s, %(id)s%(style)s)\n'

# end of class PythonTreeCtrlGenerator


class CppTreeCtrlGenerator(wcodegen.CppWidgetCodeWriter):
    extra_headers = ['<wx/treectrl.h>']
    tmpl = '%(name)s = new %(klass)s(%(parent)s, %(id)s%(style)s);\n'

    def get_events(self, obj):
        """\
        wxTreeCtrl uses wxTreeEvent for event handling
        """
        return self.codegen.get_events_with_type(obj, 'wxTreeEvent')


# end of class CppTreeCtrlGenerator


def initialize():
    klass = 'wxTreeCtrl'
    common.class_names['EditTreeCtrl'] = klass
    common.register('python', klass, PythonTreeCtrlGenerator(klass))
    common.register('C++', klass, CppTreeCtrlGenerator(klass))
