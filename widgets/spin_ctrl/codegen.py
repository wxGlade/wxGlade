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
    
    if obj.is_toplevel:
        l = ['self.%s = %s(self, %s, min=%s, max=%s, initial=%s)\n' % \
             (obj.name, obj.klass, id, min_v, max_v, value)]
        if id_name: l.append(id_name) # init lines are written in reverse order
        return l , [], []
    size = pygen.generate_code_size(obj)
    style = prop.get('style', '0')
    if not style: style = '0'
    if not obj.parent.is_toplevel: parent = 'self.%s' % obj.parent.name
    else: parent = 'self'
    init = ['self.%s = wxSpinCtrl(%s, %s, min=%s, max=%s, initial=%s, ' \
            'size=%s, style=%s)\n' % (obj.name, parent, id, min_v, max_v,
                                      value, size, style)]
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
    common.class_names['EditSpinCtrl'] = 'wxSpinCtrl'
    
    pygen = common.code_writers.get('python')
    if pygen:
        pygen.add_widget_handler('wxSpinCtrl', python_code_generator)
    
