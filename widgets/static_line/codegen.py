# codegen.py: code generator functions for wxStaticLine objects
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

import common

def python_code_generator(obj):
    """\
    generates the python code for wxStaticLine objects
    """
    pygen = common.code_writers['python']
    prop = obj.properties
    id_name, id = pygen.generate_code_id(obj)
    if not obj.parent.is_toplevel: parent = 'self.%s' % obj.parent.name
    else: parent = 'self'
    if obj.is_toplevel:
        l = []
        if id_name: l.append(id_name)
        l.append('self.%s = %s(%s, %s)\n' % (obj.name, obj.klass, parent, id))
        return l, [], []
    style = prop.get("style")
    if style and style != 'wxLI_HORIZONTAL': style = ", style=%s" % style
    else: style = ''
    init = []
    if id_name: init.append(id_name)
    init.append('self.%s = wxStaticLine(%s, %s%s)\n' %
                (obj.name, parent, id, style))
    props_buf = pygen.generate_common_properties(obj)
    return init, props_buf, []
    

def cpp_code_generator(obj):
    """\
    generates the C++ code for wxStaticLine objects
    """
    cppgen = common.code_writers['C++']
    prop = obj.properties
    id_name, id = cppgen.generate_code_id(obj)
    if id_name: ids = [ id_name ]
    else: ids = []
    if not obj.parent.is_toplevel: parent = '%s' % obj.parent.name
    else: parent = 'this'
    if obj.is_toplevel:
        l = ['%s = new %s(%s, %s);\n' % (obj.name, obj.klass, parent, id)]
        if id_name: l.append(id_name)
        return l, ids, [], []
    extra = ''
    style = prop.get("style")
    if style and style != 'wxLI_HORIZONTAL':
        extra = ', wxDefaultPosition, wxDefaultSize, %s' % style
    init = ['%s = new wxStaticLine(%s, %s%s);\n' %
            (obj.name, parent, id, extra) ]
    if id_name: init.append(id_name)
    props_buf = cppgen.generate_common_properties(obj)
    return init, ids, props_buf, []


def initialize():
    common.class_names['EditStaticLine'] = 'wxStaticLine'

    pygen = common.code_writers.get("python")
    if pygen:
        pygen.add_widget_handler('wxStaticLine', python_code_generator)
    cppgen = common.code_writers.get("C++")
    if cppgen:
        constructor = [('wxWindow*', 'parent'), ('int', 'id'),
                       ('const wxPoint&', 'pos', 'wxDefaultPosition'),
                       ('const wxSize&', 'size', 'wxDefaultSize'),
                       ('long', 'style', 'wxLI_HORIZONTAL')]
        cppgen.add_widget_handler('wxStaticLine', cpp_code_generator,
                                  constructor,
                                  extra_headers=['<wx/statline.h>'])
    
