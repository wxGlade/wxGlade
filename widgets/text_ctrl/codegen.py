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
    style = prop.get("style")
    if style: style = ", style=%s" % style
    else: style = ''
    init = ['self.%s = wxTextCtrl(%s, %s, %s%s)\n' %
            (obj.name, parent, id, value, style)]
    if id_name: init.append(id_name)
    props_buf = pygen.generate_common_properties(obj)
    return init, props_buf, []


def initialize():
    common.class_names['EditTextCtrl'] = 'wxTextCtrl'
    
    pygen = common.code_writers.get('python')
    if pygen:
        pygen.add_widget_handler('wxTextCtrl', python_code_generator)
    
