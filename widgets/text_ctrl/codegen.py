# codegen.py: code generator functions for wxTextCtrl objects
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: GPL (see license.txt)

import common

def python_code_generator(obj):
    """\
    function that generates python code for wxTextCtrl objects.
    """
    pygen = common.code_writers['python']
    prop = obj.properties
    id_name, id = pygen.generate_code_id(obj)
    value = '"' + prop.get('value', '').replace('"', r'\"') + '"'
    if not obj.parent.is_toplevel: parent = 'self.%s' % obj.parent.name
    else: parent = 'self'
    if obj.is_toplevel:
        l = ['self.%s = %s(%s, %s, %s)\n' %
             (obj.name, obj.klass, parent, id, value)]
        if id_name: l.append(id_name) # init lines are written in reverse order
        return l , [], []
    size = pygen.generate_code_size(obj)
    if size != '(-1, -1)': size = ', size=%s' % size
    else: size = ''
    style = prop.get("style")
    if style: style = ", style=%s" % style
    else: style = ''
    init = ['self.%s = wxTextCtrl(%s, %s, %s%s%s)\n' %
            (obj.name, parent, id, value, size, style)]
    if id_name: init.append(id_name)
    props_buf = []
    if prop.has_key('foreground'):
        props_buf.append(pygen.generate_code_foreground(obj))
    if prop.has_key('background'):
        props_buf.append(pygen.generate_code_background(obj))
    if prop.has_key('font'):
        props_buf.append(pygen.generate_code_font(obj))
    return init, props_buf, []


def initialize():
    common.class_names['EditTextCtrl'] = 'wxTextCtrl'
    
    pygen = common.code_writers.get('python')
    if pygen:
        pygen.add_widget_handler('wxTextCtrl', python_code_generator)
    
