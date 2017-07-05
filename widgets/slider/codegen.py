"""\
Code generator functions for wxSlider objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PythonSliderGenerator(wcodegen.PythonWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s(%(parent)s, %(id)s, %(value)s, %(minValue)s, %(maxValue)s%(style)s)\n'

    def _prepare_tmpl_content(self, obj):
        wcodegen.PythonWidgetCodeWriter._prepare_tmpl_content(self, obj)
        self.tmpl_dict['value'] = obj.value
        try:
            minValue, maxValue = obj.properties["range"].get_tuple()
        except:
            minValue, maxValue = '0', '10'
        self.tmpl_dict['minValue'] = minValue
        self.tmpl_dict['maxValue'] = maxValue
        return



class CppSliderGenerator(wcodegen.CppWidgetCodeWriter):
    tmpl = '%(name)s = new %(klass)s(%(parent)s, %(id)s, %(value)s, %(minValue)s, %(maxValue)s%(style)s);\n'

    def _prepare_tmpl_content(self, obj):
        wcodegen.CppWidgetCodeWriter._prepare_tmpl_content(self, obj)
        self.tmpl_dict['value'] = obj.value
        try:
            minValue, maxValue = obj.properties["range"].get_tuple()
        except:
            minValue, maxValue = '0', '10'
        self.tmpl_dict['minValue'] = minValue
        self.tmpl_dict['maxValue'] = maxValue
        return



def xrc_code_generator(obj):
    xrcgen = common.code_writers['XRC']

    class SliderXrcObject(xrcgen.DefaultXrcObject):
        def write_property(self, name, val, output, tabs):
            if name == 'range':
                try:
                    min, max = val.split(',')
                except ValueError:
                    pass
                else:
                    tab_s = '    '*tabs
                    output.append(tab_s + '<min>%s</min>\n' % min)
                    output.append(tab_s + '<max>%s</max>\n' % max)
            else:
                xrcgen.DefaultXrcObject.write_property(self, name, val, output, tabs)

    return SliderXrcObject(obj)


def initialize():
    klass = 'wxSlider'
    common.class_names['EditSlider'] = klass
    common.register('python', klass, PythonSliderGenerator(klass))
    common.register('C++',    klass, CppSliderGenerator(klass))
    common.register('XRC',    klass, xrc_code_generator)
