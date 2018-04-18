"""\
Code generator functions for wxStaticText objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PythonStaticTextGenerator(wcodegen.PythonWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s(%(parent)s, %(id)s, %(label)s%(style)s)\n'
    def get_more_properties_code(self, obj):
        ret = []
        if obj.wrap>0:
            ret.append( '%s.Wrap(%d)\n'%(self.tmpl_dict['name'], obj.wrap) )
        return ret


class CppStaticTextGenerator(wcodegen.CppWidgetCodeWriter):
    tmpl = '%(name)s = new %(klass)s(%(parent)s, %(id)s, %(label)s%(style)s);\n'
    def get_more_properties_code(self, obj):
        ret = []
        if obj.wrap>0:
            ret.append( '%s.Wrap(%d);\n'%(self.tmpl_dict['name'], obj.wrap) )
        return ret


def xrc_code_generator(obj):
    xrcgen = common.code_writers['XRC']

    class XrcCodeGenerator(xrcgen.DefaultXrcObject):
        def write(self, out_file, ntabs):
            properties = {"attribute":None}
            xrcgen.DefaultXrcObject.write(self, out_file, ntabs, properties)

    return XrcCodeGenerator(obj)


def initialize():
    klass = 'wxStaticText'
    common.class_names['EditStaticText'] = klass
    common.register('python', klass, PythonStaticTextGenerator(klass))
    common.register('C++',    klass, CppStaticTextGenerator(klass))
    common.register('XRC',    klass, xrc_code_generator)
