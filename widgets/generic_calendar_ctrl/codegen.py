"""\
Code generator functions for wxGenericCalendarCtrl objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014 Carsten Grohmann
@copyright: 2015 Franco Bugnano
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common, compat
import wcodegen


class PythonGenericCalendarCtrlGenerator(wcodegen.PythonWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s(%(parent)s, %(id)s%(style)s)\n'

    # XXX the following needs to depend on the code generator when Phoenix is about to be supported fully:
    if compat.IS_CLASSIC:
        import_modules = ['import wx.calendar\n']
    else:
        import_modules = ['import wx.adv\n']

    def cn(self, c):
        # TODO remove ugly hack for wxColour
        if c == 'wxColour':
            return wcodegen.PythonWidgetCodeWriter.cn(self, c)
        if c[:2] == 'wx':
            c = c[2:]
        if compat.IS_CLASSIC:
            return 'wx.calendar.' + c
        else:
            return 'wx.adv.' + c

    def _prepare_tmpl_content(self, obj):
        wcodegen.PythonWidgetCodeWriter._prepare_tmpl_content(self, obj)
        self.has_setdefault = obj.default
        return


class CppGenericCalendarCtrlGenerator(wcodegen.CppWidgetCodeWriter):
    import_modules = ['<wx/generic/calctrlg.h>']
    tmpl = '%(name)s = new %(klass)s(%(parent)s, %(id)s, wxDefaultDateTime%(style)s);\n'

    def _prepare_tmpl_content(self, obj):
        wcodegen.CppWidgetCodeWriter._prepare_tmpl_content(self, obj)
        self.has_setdefault = obj.default
        return

    def get_events(self, obj):
        "wxGenericCalendarCtrl uses wxCalendarEvent for event handling"
        return self.codegen.get_events_with_type(obj, 'wxCalendarEvent')



def xrc_code_generator(obj):
    xrcgen = common.code_writers['XRC']

    class GenericCalendarCtrlXrcObject(xrcgen.DefaultXrcObject):
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
    # end of class GenericCalendarCtrlXrcObject

    return GenericCalendarCtrlXrcObject(obj)


def initialize():
    klass = 'wxGenericCalendarCtrl'
    common.class_names['EditGenericCalendarCtrl'] = klass
    common.register('python', klass, PythonGenericCalendarCtrlGenerator(klass))
    common.register('C++', klass, CppGenericCalendarCtrlGenerator(klass))
    common.register('XRC', klass, xrc_code_generator)
