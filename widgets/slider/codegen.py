# codegen.py: code generator functions for wxSlider objects
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

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
        l = []
        if id_name: l.append(id_name)
        l.append('self.%s = %s(%s, %s, %s, %s, %s)\n' % \
                 (obj.name, obj.klass, parent, id, value, min_v, max_v))
        return l, [], []
    style = prop.get("style")
    if style and style != 'wxSL_HORIZONTAL': style = ", style=%s" % style
    else: style = ''
    init = []
    if id_name: init.append(id_name)
    init.append('self.%s = wxSlider(%s, %s, %s, %s, %s%s)\n' %
                (obj.name, parent, id, value, min_v, max_v, style))
    props_buf = pygen.generate_common_properties(obj)
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


def cpp_code_generator(obj):
    """\
    generates the C++ code for wxSlider objects
    """
    cppgen = common.code_writers['C++']
    prop = obj.properties
    id_name, id = cppgen.generate_code_id(obj)
    if id_name: ids = [ id_name ]
    else: ids = []
    value = prop.get('value', '0')
    try: min_v, max_v = [ s.strip() for s in prop['range'].split() ]
    except: min_v, max_v = '0', '10'
    if not obj.parent.is_toplevel: parent = '%s' % obj.parent.name
    else: parent = 'this'
    if obj.is_toplevel:
        l = ['%s = new %s(%s, %s, %s, %s, %s);\n' % \
             (obj.name, obj.klass, parent, id, value, min_v, max_v)]
        return l, ids, [], []
    extra = ''
    style = prop.get("style")
    if style and style != 'wxSL_HORIZONTAL':
        extra = ', wxDefaultPosition, wxDefaultSize, %s' % style
    init = ['%s = new wxSlider(%s, %s, %s, %s, %s%s);\n' %
            (obj.name, parent,id, value, min_v, max_v, extra)]
    props_buf = cppgen.generate_common_properties(obj)
    return init, ids, props_buf, []


def initialize():
    common.class_names['EditSlider'] = 'wxSlider'

    pygen = common.code_writers.get("python")
    if pygen:
        pygen.add_widget_handler('wxSlider', python_code_generator)
    xrcgen = common.code_writers.get("XRC")
    if xrcgen:
        xrcgen.add_widget_handler('wxSlider', xrc_code_generator)
    cppgen = common.code_writers.get('C++')
    if cppgen:
        constructor = [('wxWindow*', 'parent'), ('int', 'id'),
                       ('int', 'value'), ('int', 'minValue'),
                       ('int', 'maxValue'),
                       ('const wxPoint&', 'pos', 'wxDefaultPosition'),
                       ('const wxSize&', 'size', 'wxDefaultSize'),
                       ('long', 'style', 'wxSL_HORIZONTAL')]
        cppgen.add_widget_handler('wxSlider', cpp_code_generator, constructor)
    
