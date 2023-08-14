"""\
Code generator functions for wxTimePickerCtrl objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016-2023 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common, compat
import wcodegen


class PythonTimePickerCtrlGenerator(wcodegen.PythonWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s(%(parent)s, %(id)s%(style)s)\n'

    # XXX the following needs to depend on the code generator when Phoenix is about to be supported fully:
    if compat.IS_PHOENIX:
        import_modules = ['import wx.adv\n']

    if compat.IS_PHOENIX:
        def cn(self, name):
            # don't process already formatted items again
            if name.startswith('wx.'):
                return name
            if name.startswith('wx'):
                return 'wx.adv.' + name[2:]
            elif name.startswith('EVT_'):
                return 'wx.adv.' + name
            return name

    def _prepare_tmpl_content(self, obj):
        wcodegen.PythonWidgetCodeWriter._prepare_tmpl_content(self, obj)
        self.has_setdefault = int(obj.properties.get('default', 0))
        return



class CppTimePickerCtrlGenerator(wcodegen.CppWidgetCodeWriter):
    import_modules = ['<wx/datectrl.h>']
    tmpl = '%(name)s = new %(klass)s(%(parent)s, %(id)s, wxDefaultDateTime, wxDefaultPosition, wxDefaultSize, %(style)s);\n'

    prefix_style = False
    set_default_style = True

    def _prepare_tmpl_content(self, obj):
        wcodegen.CppWidgetCodeWriter._prepare_tmpl_content(self, obj)
        self.has_setdefault = int(obj.properties.get('default', 0))
        return



def xrc_code_generator(obj):
    xrcgen = common.code_writers['XRC']

    class TimePickerCtrlXrcObject(xrcgen.DefaultXrcObject):
        def write_property(self, name, val, output, tabs):
            if name == 'label':
                # translate & into _ as accelerator marker
                val2 = val.replace('&', '_')
                if val.count('&&') > 0:
                    while True:
                        index = val.find('&&')
                        if index < 0:
                            break
                        val = val2[:index] + '&&' + val2[index+2:]
                else:
                    val = val2
            xrcgen.DefaultXrcObject.write_property(self, name, val, output, tabs)

    return TimePickerCtrlXrcObject(obj)


def initialize():
    klass = 'wxTimePickerCtrl'
    common.class_names['EditTimePickerCtrl'] = klass
    common.register('python', klass, PythonTimePickerCtrlGenerator(klass))
    common.register('C++',    klass, CppTimePickerCtrlGenerator(klass))
    common.register('XRC',    klass, xrc_code_generator)
