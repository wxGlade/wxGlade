"""\
Code generator functions for wxSlider objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PythonSliderGenerator(wcodegen.PythonWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s(%(parent)s, %(id)s, %(value)s, ' \
           '%(minValue)s, %(maxValue)s%(style)s)\n'
    default_style = 'wxSL_HORIZONTAL'

    def _prepare_tmpl_content(self, obj):
        wcodegen.PythonWidgetCodeWriter._prepare_tmpl_content(self, obj)
        prop = obj.properties
        self.tmpl_dict['value'] = prop.get('value', '0')
        try:
            minValue, maxValue = [s.strip() for s in prop['range'].split(',')]
        except:
            minValue, maxValue = '0', '10'
        self.tmpl_dict['minValue'] = minValue
        self.tmpl_dict['maxValue'] = maxValue
        return

# end of class PythonSliderGenerator


class CppSliderGenerator(wcodegen.CppWidgetCodeWriter):
    tmpl = '%(name)s = new %(klass)s(%(parent)s, %(id)s, %(value)s, ' \
           '%(minValue)s, %(maxValue)s%(style)s);\n'
    default_style = 'wxSL_HORIZONTAL'

    def _prepare_tmpl_content(self, obj):
        wcodegen.CppWidgetCodeWriter._prepare_tmpl_content(self, obj)
        prop = obj.properties
        self.tmpl_dict['value'] = prop.get('value', '0')
        try:
            minValue, maxValue = [s.strip() for s in prop['range'].split(',')]
        except:
            minValue, maxValue = '0', '10'
        self.tmpl_dict['minValue'] = minValue
        self.tmpl_dict['maxValue'] = maxValue
        return

    def get_events(self, obj):
        return self.codegen.get_events_with_type(obj, 'wxScrollEvent')

# end of class CppSliderGenerator


def xrc_code_generator(obj):
    xrcgen = common.code_writers['XRC']

    class SliderXrcObject(xrcgen.DefaultXrcObject):
        def write_property(self, name, val, outfile, tabs):
            if name == 'range':
                try:
                    min, max = val.split(',')
                except ValueError:
                    pass
                else:
                    tab_s = '    '*tabs
                    outfile.write(tab_s + '<min>%s</min>\n' % min)
                    outfile.write(tab_s + '<max>%s</max>\n' % max)
            else:
                xrcgen.DefaultXrcObject.write_property(self, name, val,
                                                       outfile, tabs)

    # end of class SliderXrcObject

    return SliderXrcObject(obj)


def initialize():
    common.class_names['EditSlider'] = 'wxSlider'

    pygen = common.code_writers.get("python")
    if pygen:
        pygen.add_widget_handler('wxSlider', PythonSliderGenerator())
    xrcgen = common.code_writers.get("XRC")
    if xrcgen:
        xrcgen.add_widget_handler('wxSlider', xrc_code_generator)
    cppgen = common.code_writers.get('C++')
    if cppgen:
        cppgen.add_widget_handler('wxSlider', CppSliderGenerator())
