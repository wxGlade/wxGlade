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
    label = prop.get('label', '').replace('"', r'\"')
    if not obj.parent.is_toplevel: parent = 'self.%s' % obj.parent.name
    else: parent = 'self'
    if obj.is_toplevel:
        l = ['self.%s = %s(%s, %s, "%s")\n' %
             (obj.name, obj.klass, parent, id, label)]
        if id_name: l.append(id_name)
        return l, [], []
    style = prop.get("style")
    if style: style = ", style=%s" % style
    else: style = ''
    init = ['self.%s = wxStaticText(%s, %s, "%s"%s)\n' %
            (obj.name, parent, id, label, style) ]
    if id_name: init.append(id_name)
    props_buf = pygen.generate_common_properties(obj)
    return init, props_buf, []


def initialize():
    common.class_names['EditStaticText'] = 'wxStaticText'

    # python code generation functions
    pygen = common.code_writers.get("python")
    if pygen:
        pygen.add_widget_handler('wxStaticText', python_code_generator)
