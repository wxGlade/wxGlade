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
        l = []
        if id_name: l.append(id_name)
        l.append('self.%s = %s(%s, %s, %s)\n' %
                 (obj.name, obj.klass, parent, id, label))
        return l, [], []    
    init = []
    if id_name: init.append(id_name)
    init.append('self.%s = wxToggleButton(%s, %s, %s)\n' % 
                (obj.name, parent, id, label))
    props_buf = pygen.generate_common_properties(obj)
    value = prop.get('value')
    if value: props_buf.append('self.%s.SetValue(%s)\n' % (obj.name, value))
    return init, props_buf, []


def cpp_code_generator(obj):
    """\
    fuction that generates C++ code for wxToggleButton objects.
    """
    cppgen = common.code_writers['C++']
    prop = obj.properties
    id_name, id = cppgen.generate_code_id(obj)
    if id_name: ids = [ '%s = %s' % (id_name, id) ]
    else: ids = []
    label = '"' + prop.get('label', '').replace('"', r'\"') + '"'
    if not obj.parent.is_toplevel: parent = '%s' % obj.parent.name
    else: parent = 'this'
    if obj.is_toplevel:
        l = ['%s = new %s(%s, %s, %s);\n' %
             (obj.name, obj.klass, parent, id, label)]
        return l, ids, [], []    
    init = [ '%s = new wxToggleButton(%s, %s, %s);\n' % 
             (obj.name, parent, id, label) ]
    props_buf = cppgen.generate_common_properties(obj)
    value = prop.get('value')
    if value: props_buf.append('%s->SetValue(%s);\n' % (obj.name, value))
    return init, ids, props_buf, []


def initialize():
    common.class_names['EditToggleButton'] = 'wxToggleButton'

    pygen = common.code_writers.get('python')
    if pygen:
        pygen.add_widget_handler('wxToggleButton', python_code_generator)
    cppgen = common.code_writers.get('C++')
    if cppgen:
        constructor = [('wxWindow*', 'parent'), ('int', 'id'),
                       ('const wxString&', 'label'),
                       ('const wxPoint&', 'pos', 'wxDefaultPosition'),
                       ('const wxSize&', 'size', 'wxDefaultSize'),
                       ('long', 'style', '0')]
        cppgen.add_widget_handler('wxToggleButton', cpp_code_generator,
                                  constructor,
                                  extra_headers=['<wx/tglbtn.h>'])
