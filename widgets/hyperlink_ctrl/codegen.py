"""
Code generator functions for wxHyperlinkCtrl objects

@copyright: 2012-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PythonHyperlinkCtrlGenerator(wcodegen.PythonWidgetCodeWriter):

    tmpl = '%(name)s = %(klass)s(%(parent)s, %(id)s, %(label)s, ' \
           '%(url)s%(style)s)\n'

    def _prepare_tmpl_content(self, obj):
        wcodegen.PythonWidgetCodeWriter._prepare_tmpl_content(self, obj)
        self.tmpl_dict['url'] = self.codegen.quote_str(obj.properties.get('url', ''))
        self.has_setvalue1 = obj.properties.get('checked', False)
        return

# end of class PythonHyperlinkCtrlGenerator


class CppHyperlinkCtrlGenerator(wcodegen.CppWidgetCodeWriter):

    import_modules = ['<wx/hyperlink.h>']
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
    klass = 'wxHyperlinkCtrl'
    common.class_names['EditHyperlinkCtrl'] = klass
    common.register('python', klass, PythonHyperlinkCtrlGenerator(klass))
    common.register('C++', klass, CppHyperlinkCtrlGenerator(klass))
    common.register('XRC', klass, xrc_code_generator)
