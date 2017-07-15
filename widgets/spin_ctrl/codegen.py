"""\
Code generator functions for wxSpinCtrl objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PythonSpinCtrlGenerator(wcodegen.PythonWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s(%(parent)s, %(id)s, "%(value)s", min=%(minValue)s, max=%(maxValue)s%(style)s)\n'

    def _prepare_tmpl_content(self, obj):
        wcodegen.PythonWidgetCodeWriter._prepare_tmpl_content(self, obj)
        self.tmpl_dict['value'] = obj.value
        try:
            minValue, maxValue = obj.properties["range"].get_tuple()
        except:
            minValue, maxValue = '0', '100'
        self.tmpl_dict['minValue'] = minValue
        self.tmpl_dict['maxValue'] = maxValue
        return



class CppSpinCtrlGenerator(wcodegen.CppWidgetCodeWriter):
    import_modules = ['<wx/spinctrl.h>']
    tmpl = '%(name)s = new %(klass)s(%(parent)s, %(id)s, wxT("%(value)s"), ' \
           'wxDefaultPosition, wxDefaultSize, %(style)s, %(minValue)s, %(maxValue)s);\n'
    prefix_style = False
    set_default_style = True

    def _prepare_tmpl_content(self, obj):
        wcodegen.CppWidgetCodeWriter._prepare_tmpl_content(self, obj)
        self.tmpl_dict['value'] = obj.value
        try:
            minValue, maxValue = obj.properties["range"].get_tuple()
        except:
            minValue, maxValue = '0', '100'
        self.tmpl_dict['minValue'] = minValue
        self.tmpl_dict['maxValue'] = maxValue
        return



def xrc_code_generator(obj):
    xrcgen = common.code_writers['XRC']

    class SpinCtrlXrcObject(xrcgen.DefaultXrcObject):
        def write_property(self, name, val, output, tabs):
            if name == 'range':
                try:
                    minValue, maxValue = val.split(',')
                except ValueError:
                    pass
                else:
                    tab_s = '    '*tabs
                    output.append(tab_s + '<min>%s</min>\n' % minValue)
                    output.append(tab_s + '<max>%s</max>\n' % maxValue)
            else:
                xrcgen.DefaultXrcObject.write_property(self, name, val, output, tabs)

    return SpinCtrlXrcObject(obj)


def initialize():
    klass = 'wxSpinCtrl'
    common.class_names['EditSpinCtrl'] = klass
    common.register('python', klass, PythonSpinCtrlGenerator(klass))
    common.register('C++',    klass, CppSpinCtrlGenerator(klass))
    common.register('XRC',    klass, xrc_code_generator)
