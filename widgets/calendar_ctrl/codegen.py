"""\
Code generator functions for wxCalendarCtrl objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PythonCalendarCtrlGenerator(wcodegen.PythonWidgetCodeWriter):

    tmpl = '%(name)s = %(klass)s(%(parent)s, %(id)s%(style)s)\n'

    def cn(self, c):
        if self.codegen.use_new_namespace:
            if c[:2] == 'wx':
                c = c[2:]
            return 'wx.calendar.' + c
        else:
            return c

    def _reset_vars(self):
        wcodegen.PythonWidgetCodeWriter._reset_vars(self)
        if self.codegen.use_new_namespace:
            self.import_modules = ['import wx.calendar\n']
        else:
            self.import_modules = ['from wxPython.calendar import *\n']

    def _prepare_tmpl_content(self, obj):
        wcodegen.PythonWidgetCodeWriter._prepare_tmpl_content(self, obj)
        self.has_setdefault = obj.properties.get('default', False)
        return

# end of class PythonCalendarCtrlGenerator


class CppCalendarCtrlGenerator(wcodegen.CppWidgetCodeWriter):
    extra_headers = ['<wx/calctrl.h>']
    tmpl = '%(name)s = new %(klass)s(%(parent)s, %(id)s%(style)s);\n'

    def _prepare_tmpl_content(self, obj):
        wcodegen.CppWidgetCodeWriter._prepare_tmpl_content(self, obj)
        self.has_setdefault = obj.properties.get('default', False)
        return

    def get_events(self, obj):
        """\
        wxCalendarCtrl uses wxCalendarEvent for event handling
        """
        return self.codegen.get_events_with_type(obj, 'wxCalendarEvent')

# end of class CppCalendarCtrlGenerator


def xrc_code_generator(obj):
    xrcgen = common.code_writers['XRC']

    class CalendarCtrlXrcObject(xrcgen.DefaultXrcObject):
        def write_property(self, name, val, outfile, tabs):
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
            xrcgen.DefaultXrcObject.write_property(self, name, val,
                                                   outfile, tabs)
    # end of class CalendarCtrlXrcObject

    return CalendarCtrlXrcObject(obj)


def initialize():
    common.class_names['EditCalendarCtrl'] = 'wxCalendarCtrl'
    pygen = common.code_writers.get('python')
    if pygen:
        pygen.add_widget_handler('wxCalendarCtrl',
                                 PythonCalendarCtrlGenerator())
    xrcgen = common.code_writers.get("XRC")
    if xrcgen:
        xrcgen.add_widget_handler('wxCalendarCtrl',
                                  xrc_code_generator)
    cppgen = common.code_writers.get('C++')
    if cppgen:
        cppgen.add_widget_handler('wxCalendarCtrl',
                                  CppCalendarCtrlGenerator())
