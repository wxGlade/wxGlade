# codegen.py: code generator functions for wxStaticText objects
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: GPL (see license.txt)

import common

def python_code_generator(obj):
    """\
    generates the python code for wxStaticText objects
    """
    pygen = common.code_writers['python']
    prop = obj.properties
    id_name, id = pygen.generate_code_id(obj) 
    label = prop.get('label', '').replace('"', '\"')
    if obj.is_toplevel:
        l = ['self.%s = %s(self, %s, "%s")\n' %
             (obj.name, obj.klass, id, label)]
        if id_name: l.append(id_name)
        return l, [], []
    size = pygen.generate_code_size(obj)
    if not obj.parent.is_toplevel: parent = 'self.%s' % obj.parent.name
    else: parent = 'self'
    style = prop.get("style", "0")
    if not style: style = "0"
    init = ['self.%s = wxStaticText(%s, %s, "%s", size=%s, style=%s)\n' %
            (obj.name, parent, id, label, size, style) ]
    if id_name: init.append(id_name)
    props_buf = []
    if prop.has_key('foreground'):
        props_buf.append(pygen.generate_code_foreground(obj))
    if prop.has_key('background'):
        props_buf.append(pygen.generate_code_background(obj))
    if prop.has_key('font'): props_buf.append(pygen.generate_code_font(obj))
    return init, props_buf, []


def initialize():
    common.class_names['EditStaticText'] = 'wxStaticText'

    # python code generation functions
    pygen = common.code_writers.get("python")
    if pygen:
        pygen.add_widget_handler('wxStaticText', python_code_generator)
