"""
Code generator functions for wxPropertyGridManager objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2015 Franco Bugnano
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PythonCodeGenerator(wcodegen.PythonWidgetCodeWriter):
    import_modules = ['import wx.propgrid\n']
    tmpl = '%(name)s = %(klass)s(%(parent)s, %(id)s%(style)s)\n'

    def cn(self, c):
        #self._logger.debug('PythonStaticTextGenerator.cn with arg:', c)
        # TODO remove ugly hack for wxColour
        if c == 'wxColour':
            return wcodegen.PythonWidgetCodeWriter.cn(self, c)
        if c[:2] == 'wx':
            c = c[2:]
        return 'wx.propgrid.' + c

    def _prepare_tmpl_content(self, obj):
        wcodegen.PythonWidgetCodeWriter._prepare_tmpl_content(self, obj)
        return



class CppCodeGenerator(wcodegen.CppWidgetCodeWriter):
    import_modules = ['<wx/propgrid/manager.h>']
    tmpl = '%(name)s = new %(klass)s(%(parent)s, %(id)s%(style)s);\n'

    def _prepare_tmpl_content(self, obj):
        wcodegen.CppWidgetCodeWriter._prepare_tmpl_content(self, obj)
        return



def xrc_code_generator(obj):
    xrcgen = common.code_writers['XRC']

    class PropertyGridManagerXrcObject(xrcgen.DefaultXrcObject):
        def write_property(self, name, val, output, tabs):
            xrcgen.DefaultXrcObject.write_property(self, name, val, output, tabs)

    return PropertyGridManagerXrcObject(obj)


def initialize():
    klass = 'wxPropertyGridManager'
    common.class_names['EditPropertyGridManager'] = klass
    common.register('python', klass, PythonCodeGenerator(klass))
    common.register('C++',    klass, CppCodeGenerator(klass))
    common.register('XRC',    klass, xrc_code_generator)
