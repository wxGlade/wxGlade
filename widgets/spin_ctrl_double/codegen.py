"""\
Code generator functions for wxSpinCtrl objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2018-2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PythonSpinCtrlDoubleGenerator(wcodegen.PythonWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s(%(parent)s, %(id)s, initial=%(value)s, min=%(minValue)s, max=%(maxValue)s%(style)s)\n'

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

    def get_more_properties_code(self, obj):
        ret = []
        name = self.tmpl_dict['name']
        if obj.properties["increment"].is_active():
            ret.append( '%s.SetIncrement(%s)\n'%(name, obj.increment) )
        if obj.properties["digits"].is_active():
            ret.append( '%s.SetDigits(%s)\n'%(name, obj.digits) )
        return ret



class CppSpinCtrlDoubleGenerator(wcodegen.CppWidgetCodeWriter):
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

    def get_more_properties_code(self, obj):
        ret = []
        name = self.tmpl_dict['name']
        if obj.properties["increment"].is_active():
            ret.append( '%s->SetIncrement(%s);\n'%(name, obj.increment) )
        if obj.properties["digits"].is_active():
            ret.append( '%s->SetDigits(%s);\n'%(name, obj.digits) )
        return ret


def xrc_code_generator(obj):
    xrcgen = common.code_writers['XRC']

    class SpinCtrlDoubleXrcObject(xrcgen.DefaultXrcObject):
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

    return SpinCtrlDoubleXrcObject(obj)


def initialize():
    klass = 'wxSpinCtrlDouble'
    common.class_names['EditSpinCtrlDouble'] = klass
    common.register('python', klass, PythonSpinCtrlDoubleGenerator(klass))
    common.register('C++',    klass, CppSpinCtrlDoubleGenerator(klass))
    common.register('XRC',    klass, xrc_code_generator)
