# codegen.py: code generator functions for wxToggleButton objects
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: GPL (see license.txt)

import common

def python_code_generator(obj):
    """\
    fuction that generates python code for wxToggleButton objects.
    """
    pygen = common.code_writers['python']
    prop = obj.properties
    id_name, id = pygen.generate_code_id(obj)
    label = '"' + prop.get('label', '').replace('"', r'\"') + '"'
    if not obj.parent.is_toplevel: parent = 'self.%s' % obj.parent.name
    else: parent = 'self'
    if obj.is_toplevel:
        l = ['self.%s = %s(%s, %s, %s)\n' %
             (obj.name, obj.klass, parent, id, label)]
        if id_name: l.append(id_name) # init lines are written in reverse order
        return l, [], []    
    init = [ 'self.%s = wxToggleButton(%s, %s, %s)\n' % 
             (obj.name, parent, id, label) ]
    if id_name: init.append(id_name) # init lines are written in reverse order
    props_buf = pygen.generate_common_properties(obj)
    value = prop.get('value')
    if value: props_buf.append('self.%s.SetValue(%s)\n' % (obj.name, value))
    return init, props_buf, []


def initialize():
    common.class_names['EditToggleButton'] = 'wxToggleButton'

    pygen = common.code_writers.get('python')
    if pygen:
        pygen.add_widget_handler('wxToggleButton', python_code_generator)
