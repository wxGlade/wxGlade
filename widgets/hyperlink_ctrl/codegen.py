"""
Code generator functions for wxHyperlinkCtrl objects

@copyright: 2012-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common, compat
import wcodegen


class PythonHyperlinkCtrlGenerator(wcodegen.PythonWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s(%(parent)s, %(id)s, %(label)s, %(url)s%(style)s)\n'

    if compat.IS_PHOENIX:
        import_modules = ['import wx.adv\n']

        def cn(self, name):
            # don't process already formatted items again
            if name.startswith('wx.'):  return name
            if name.startswith('wx'):
                if name=='wxBORDER_NONE': return 'wx.BORDER_NONE'
                return 'wx.adv.' + name[2:]
            if name.startswith('EVT_'): return 'wx.adv.' + name
            return name

    def _prepare_tmpl_content(self, obj):
        wcodegen.PythonWidgetCodeWriter._prepare_tmpl_content(self, obj)
        self.tmpl_dict['url'] = self.codegen.quote_str(obj.url)
        #self.has_setvalue1 = bool(obj.checked)
        return



class CppHyperlinkCtrlGenerator(wcodegen.CppWidgetCodeWriter):
    import_modules = ['<wx/hyperlink.h>']
    tmpl = '%(name)s = new %(klass)s(%(parent)s, %(id)s, %(label)s, %(url)s%(style)s);\n'

    def _prepare_tmpl_content(self, obj):
        wcodegen.CppWidgetCodeWriter._prepare_tmpl_content(self, obj)
        self.tmpl_dict['url'] = self.codegen.quote_str(obj.url)
        #self.has_setvalue1 = bool(obj.checked)
        return


def xrc_code_generator(obj):
    xrcgen = common.code_writers['XRC']

    class XrcCodeGenerator(xrcgen.DefaultXrcObject):
        def write(self, out_file, ntabs):
            properties = {"attribute":None}
            xrcgen.DefaultXrcObject.write(self, out_file, ntabs, properties)

    return XrcCodeGenerator(obj)


def initialize():
    klass = 'wxHyperlinkCtrl'
    common.class_names['EditHyperlinkCtrl'] = klass
    common.register('python', klass, PythonHyperlinkCtrlGenerator(klass))
    common.register('C++',    klass, CppHyperlinkCtrlGenerator(klass))
    common.register('XRC',    klass, xrc_code_generator)
