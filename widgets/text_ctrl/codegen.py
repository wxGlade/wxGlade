# codegen.py: code generator functions for wxTextCtrl objects
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: Python 2.2 license (see license.txt)

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
        l = []
        if id_name: l.append(id_name)
        l.append('self.%s = %s(%s, %s, %s)\n' %
                 (obj.name, obj.klass, parent, id, value))
        return l , [], []
    style = prop.get("style")
    if style: style = ", style=%s" % style
    else: style = ''
    init = []
    if id_name: init.append(id_name)
    init.append('self.%s = wxTextCtrl(%s, %s, %s%s)\n' %
                (obj.name, parent, id, value, style))
    props_buf = pygen.generate_common_properties(obj)
    return init, props_buf, []


def cpp_code_generator(obj):
    """\
    function that generates C++ code for wxTextCtrl objects.
    """
    cppgen = common.code_writers['C++']
    prop = obj.properties
    id_name, id = cppgen.generate_code_id(obj)
    if id_name: ids = [ id_name ]
    else: ids = []
    value = '"' + prop.get('value', '').replace('"', r'\"') + '"'
    if not obj.parent.is_toplevel: parent = '%s' % obj.parent.name
    else: parent = 'this'
    if obj.is_toplevel:
        l = ['%s = new %s(%s, %s, %s);\n' %
             (obj.name, obj.klass, parent, id, value)]
        return l, ids, [], []
    extra = ''
    style = prop.get('style')
    if style: extra = ', wxDefaultPosition, wxDefaultSize, %s' % style
    init = ['%s = new wxTextCtrl(%s, %s, %s%s);\n' %
            (obj.name, parent, id, value, extra)]
    props_buf = cppgen.generate_common_properties(obj)
    return init, ids, props_buf, []


def initialize():
    common.class_names['EditTextCtrl'] = 'wxTextCtrl'
    
    pygen = common.code_writers.get('python')
    if pygen:
        pygen.add_widget_handler('wxTextCtrl', python_code_generator)
    cppgen = common.code_writers.get('C++')
    if cppgen:
        constructor = [('wxWindow*', 'parent'), ('int', 'id'),
                       ('const wxString&', 'value', '""'),
                       ('const wxPoint&', 'pos', 'wxDefaultPosition'),
                       ('const wxSize&', 'size', 'wxDefaultSize'),
                       ('long', 'style', '0')]
        cppgen.add_widget_handler('wxTextCtrl', cpp_code_generator,
                                  constructor)
    
