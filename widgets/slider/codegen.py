# codegen.py: code generator functions for wxSlider objects
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: GPL (see license.txt)

import common

def python_code_generator(obj):
    """\
    generates the python code for wxSlider objects
    """
    pygen = common.code_writers['python']
    prop = obj.properties
    id_name, id = pygen.generate_code_id(obj)
    value = prop.get('value', '0')
    try: min_v, max_v = [ s.strip() for s in prop['range'].split() ]
    except: min_v, max_v = '0', '10'
    if not obj.parent.is_toplevel: parent = 'self.%s' % obj.parent.name
    else: parent = 'self'
    if obj.is_toplevel:
        l = ['self.%s = %s(%s, %s, %s, %s, %s)\n' % \
             (obj.name, obj.klass, parent, id, value, min_v, max_v)]
        if id_name: l.append(id_name)
        return l, [], []
    size = pygen.generate_code_size(obj)
    if size != '(-1, -1)': size = ', size=%s' % size
    else: size = ''
    style = prop.get("style")
    if style and style != 'wxSL_HORIZONTAL': style = ", style=%s" % style
    else: style = ''
    init = ['self.%s = wxSlider(%s, %s, %s, %s, %s%s%s)\n' %
            (obj.name, parent, id, value, min_v, max_v, size, style) ]
    if id_name: init.append(id_name)
    props_buf = []
    if prop.has_key('foreground'):
        props_buf.append(pygen.generate_code_foreground(obj))
    if prop.has_key('background'):
        props_buf.append(pygen.generate_code_background(obj))
    return init, props_buf, []


def xrc_code_generator(obj):
    xrcgen = common.code_writers['XRC']
    class SliderXrcObject(xrcgen.DefaultXrcObject):
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

    # end of class SliderXrcObject

    return SliderXrcObject(obj)


def initialize():
    common.class_names['EditSlider'] = 'wxSlider'

    pygen = common.code_writers.get("python")
    if pygen:
        pygen.add_widget_handler('wxSlider', python_code_generator)
    xrcgen = common.code_writers.get("XRC")
    if xrcgen:
        xrcgen.add_widget_handler('wxSlider', xrc_code_generator)
    
