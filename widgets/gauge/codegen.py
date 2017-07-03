"""\
Code generator functions for wxGauge objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PythonGaugeGenerator(wcodegen.PythonWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s(%(parent)s, %(id)s, %(range)s%(style)s)\n'

    def _prepare_tmpl_content(self, obj):
        wcodegen.PythonWidgetCodeWriter._prepare_tmpl_content(self, obj)
        self.tmpl_dict['range'] = obj.range
        return


class CppGaugeGenerator(wcodegen.CppWidgetCodeWriter):
    tmpl = '%(name)s = new %(klass)s(%(parent)s, %(id)s, %(range)s%(style)s);\n'

    def _prepare_tmpl_content(self, obj):
        wcodegen.CppWidgetCodeWriter._prepare_tmpl_content(self, obj)
        self.tmpl_dict['range'] = obj.range
        return


def xrc_code_generator(obj):
    xrcgen = common.code_writers['XRC']

    class XrcCodeGenerator(xrcgen.DefaultXrcObject):
        def write(self, out_file, ntabs):
            style = self.widget.properties["style"].get_string_value()
            properties = {"style":style}
            xrcgen.DefaultXrcObject.write(self, out_file, ntabs, properties)

    return XrcCodeGenerator(obj)


def initialize():
    klass = 'wxGauge'
    common.class_names['EditGauge'] = klass
    common.register('python', klass, PythonGaugeGenerator(klass))
    common.register('C++', klass, CppGaugeGenerator(klass))
    common.register('XRC',    klass, xrc_code_generator)
