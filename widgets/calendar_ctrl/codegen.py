# codegen.py: code generator functions for wxCalendarCtrl objects
# $Header: /home/alb/tmp/wxglade_cvs_backup/wxGlade/widgets/calendar_ctrl/codegen.py,v 1.3 2006/10/19 16:51:10 guyru Exp $

# Copyright (c) 2002-2005 Alberto Griggio <agriggio@users.sourceforge.net>
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

import common

class PythonCodeGenerator:
    def get_code(self, obj):
        pygen = common.code_writers['python']
        cn = pygen.cn
        prop = obj.properties
        id_name, id = pygen.generate_code_id(obj)
        #label = pygen.quote_str(prop.get('label', ''))
        if not obj.parent.is_toplevel: parent = 'self.%s' % obj.parent.name
        else: parent = 'self'
        style = prop.get("style")
        if style: style = ", style=%s" % pygen.cn_f(style)
        else: style = ''
        init = []
        if id_name: init.append(id_name)
        klass = obj.klass
        if klass == obj.base: klass = cn(klass)
        init.append('self.%s = %s(%s, %s, %s)\n' %
        #            (obj.name, klass, parent, id, label, style))
                     (obj.name, klass,parent, id, style))
        props_buf = pygen.generate_common_properties(obj)
        if prop.get('default', False):
            props_buf.append('self.%s.SetDefault()\n' % obj.name)
        return init, props_buf, []

# end of class PythonCodeGenerator


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
                        if index < 0: break
                        val = val2[:index] + '&&' + val2[index+2:]
                else: val = val2
            xrcgen.DefaultXrcObject.write_property(self, name, val,
                                                   outfile, tabs)
    # end of class CalendarCtrlXrcObject

    return CalendarCtrlXrcObject(obj)


class CppCodeGenerator:
    extra_headers = ['<wx/calctrl.h>']
    
    def get_code(self, obj):
        """\
        fuction that generates python code for wxCalendarCtrl objects.
        """
        cppgen = common.code_writers['C++']
        prop = obj.properties
        id_name, id = cppgen.generate_code_id(obj)
        if id_name: ids = [ id_name ]
        else: ids = []
        if not obj.parent.is_toplevel: parent = '%s' % obj.parent.name
        else: parent = 'this'
        extra = ''
        style = prop.get("style")
        if style: extra = ', wxDefaultPosition, wxDefaultSize, %s' % style
        #label = cppgen.quote_str(prop.get('label', ''))
        init = [ '%s = new %s(%s, %s, %s);\n' % 
        #         (obj.name, obj.klass, parent, id, label, extra) ]
                  (obj.name, obj.klass, parent, id, extra) ]
        props_buf = cppgen.generate_common_properties(obj)
        if prop.get('default', False):
            props_buf.append('%s->SetDefault();\n' % obj.name)
        return init, ids, props_buf, []

# end of class CppCodeGenerator


def initialize():
    common.class_names['EditCalendarCtrl'] = 'wxCalendarCtrl'
    pygen = common.code_writers.get('python')
    if pygen:
        pygen.add_widget_handler('wxCalendarCtrl', PythonCodeGenerator())
    xrcgen = common.code_writers.get("XRC")
    if xrcgen:
        xrcgen.add_widget_handler('wxCalendarCtrl', xrc_code_generator)
    cppgen = common.code_writers.get('C++')
    if cppgen:
        cppgen.add_widget_handler('wxCalendarCtrl', CppCodeGenerator())
