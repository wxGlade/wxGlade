# codegen.py: code generator functions for wxSpinCtrl objects
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: GPL (see license.txt)

import common

def python_code_generator(obj):
    """\
    function that generates python code for wxSpinCtrl objects.
    """
    pygen = common.code_writers['python']
    prop = obj.properties
    id_name, id = pygen.generate_code_id(obj)
    value = prop.get('value', '0')
    try: min_v, max_v = [ s.strip() for s in \
                          prop.get('range', '0, 100').split(',') ]
    except: min_v, max_v = '0', '100'
    
    if not obj.parent.is_toplevel: parent = 'self.%s' % obj.parent.name
    else: parent = 'self'
    if obj.is_toplevel:
        l = ['self.%s = %s(%s, %s, min=%s, max=%s, initial=%s)\n' % \
             (obj.name, obj.klass, parent, id, min_v, max_v, value)]
        if id_name: l.append(id_name) # init lines are written in reverse order
        return l , [], []
    style = prop.get("style")
    if style: style = ", style=%s" % style
    else: style = ''
    init = ['self.%s = wxSpinCtrl(%s, %s, min=%s, max=%s, initial=%s' \
            '%s)\n' % (obj.name, parent, id, min_v, max_v, value, style)]
    if id_name: init.append(id_name)
    props_buf = pygen.generate_common_properties(obj)
    return init, props_buf, []


def xrc_code_generator(obj):
    xrcgen = common.code_writers['XRC']
    class SpinCtrlXrcObject(xrcgen.DefaultXrcObject):
        def write_property(self, name, val, outfile, tabs):
            if name == 'range':
                try: min, max = val.split(',')
                except ValueError: pass
                else:
                    tab_s = '    '*tabs
                    outfile.write(tab_s + '<min>%s</min>\n' % min)
                    outfile.write(tab_s + '<max>%s</max>\n' % max)
            else:
                xrcgen.DefaultXrcObject.write_property(self, name, val,
                                                       outfile, tabs)

    # end of class SpinCtrlXrcObject
    
    return SpinCtrlXrcObject(obj)


def initialize():
    common.class_names['EditSpinCtrl'] = 'wxSpinCtrl'
    
    pygen = common.code_writers.get('python')
    if pygen:
        pygen.add_widget_handler('wxSpinCtrl', python_code_generator)
    xrcgen = common.code_writers.get("XRC")
    if xrcgen:
        xrcgen.add_widget_handler('wxSpinCtrl', xrc_code_generator)
