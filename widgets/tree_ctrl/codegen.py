# codegen.py: code generator functions for wxTreeCtrl objects
#
# Copyright (c) 2002-2003 Alberto Griggio <albgrig@tiscalinet.it>
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

import common

def python_code_generator(obj):
    """\
    function that generates python code for wxTreeCtrl objects.
    """
    pygen = common.code_writers['python']
    prop = obj.properties
    id_name, id = pygen.generate_code_id(obj)
    if not obj.parent.is_toplevel: parent = 'self.%s' % obj.parent.name
    else: parent = 'self'
    style = prop.get("style")
    if style and style != 'wxTR_HAS_BUTTONS': # default style
        style = ", style=%s" % style
    else: style = ''
    init = []
    if id_name: init.append(id_name)
    init.append('self.%s = %s(%s, %s%s)\n' %
                (obj.name, obj.klass, parent, id, style))
    props_buf = pygen.generate_common_properties(obj)
    return init, props_buf, []


def cpp_code_generator(obj):
    """\
    function that generates C++ code for wxTreeCtrl objects.
    """
    cppgen = common.code_writers['C++']
    prop = obj.properties
    id_name, id = cppgen.generate_code_id(obj)
    if id_name: ids = [ id_name ]
    else: ids = []
    if not obj.parent.is_toplevel: parent = '%s' % obj.parent.name
    else: parent = 'this'
    extra = ''
    style = prop.get('style')
    if style and style != 'wxTR_HAS_BUTTONS':
        extra = ', wxDefaultPosition, wxDefaultSize, %s' % style
    init = ['%s = new %s(%s, %s%s);\n' %
            (obj.name, obj.klass, parent, id, extra)]
    props_buf = cppgen.generate_common_properties(obj)
    return init, ids, props_buf, []


def initialize():
    common.class_names['EditTreeCtrl'] = 'wxTreeCtrl'
    
    pygen = common.code_writers.get('python')
    if pygen:
        pygen.add_widget_handler('wxTreeCtrl', python_code_generator)
    cppgen = common.code_writers.get('C++')
    if cppgen:
        cppgen.add_widget_handler('wxTreeCtrl', cpp_code_generator,
                                  extra_headers=['<wx/treectrl.h>'])

