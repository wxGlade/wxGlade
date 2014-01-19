"""
Code generator functions for wxHyperlinkCtrl objects

@copyright: 2012-2014 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PythonHyperlinkCtrlGenerator(wcodegen.PythonWidgetCodeWriter):

    supported_by = ((2, 8), (3, 0),)
    tmpl = '%(name)s = %(klass)s(%(parent)s, %(id)s, %(label)s, ' \
           '%(url)s%(style)s)\n'

    def _prepare_tmpl_content(self, obj):
        wcodegen.PythonWidgetCodeWriter._prepare_tmpl_content(self, obj)
        self.tmpl_dict['url'] = self.codegen.quote_str(obj.properties.get('url', ''))
        self.has_setvalue1 = obj.properties.get('checked', False)
        return

# end of class PythonHyperlinkCtrlGenerator


class CppHyperlinkCtrlGenerator(wcodegen.CppWidgetCodeWriter):

    extra_headers = ['<wx/hyperlink.h>']
    supported_by = ((2, 8), (3, 0),)
    tmpl = '%(name)s = new %(klass)s(%(parent)s, %(id)s, %(label)s, ' \
           '%(url)s%(style)s);\n'

    def _prepare_tmpl_content(self, obj):
        wcodegen.CppWidgetCodeWriter._prepare_tmpl_content(self, obj)
        self.tmpl_dict['url'] = self.codegen.quote_str(obj.properties.get('url', ''))
        self.has_setvalue1 = obj.properties.get('checked', False)
        return

# end of class CppHyperlinkCtrlGenerator


def xrc_code_generator(obj):
    xrcgen = common.code_writers['XRC']

    class XrcCodeGenerator(xrcgen.DefaultXrcObject):
        def write(self, *args, **kwds):
            try:
                del self.properties['attribute']
            except KeyError:
                pass
            xrcgen.DefaultXrcObject.write(self, *args, **kwds)

    return XrcCodeGenerator(obj)


def initialize():
    common.class_names['EditHyperlinkCtrl'] = 'wxHyperlinkCtrl'

    pygen = common.code_writers.get("python")
    if pygen:
        pygen.add_widget_handler(
            'wxHyperlinkCtrl',
            PythonHyperlinkCtrlGenerator()
        )
    cppgen = common.code_writers.get('C++')
    if cppgen:
        cppgen.add_widget_handler(
            'wxHyperlinkCtrl',
            CppHyperlinkCtrlGenerator()
        )
    xrcgen = common.code_writers.get('XRC')
    if xrcgen:
        xrcgen.add_widget_handler('wxHyperlinkCtrl', xrc_code_generator)
