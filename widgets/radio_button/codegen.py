# codegen.py: code generator functions for wxRadioButton objects
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

import common

def python_code_generator(obj):
    """\
    generates the python code for wxRadioButton objects
    """
    pygen = common.code_writers['python']
    prop = obj.properties
    id_name, id = pygen.generate_code_id(obj)
    label = prop.get('label', '').replace('"', r'\"')
    if not obj.parent.is_toplevel: parent = 'self.%s' % obj.parent.name
    else: parent = 'self'
##     if obj.is_toplevel:
##         l = []
##         if id_name: l.append(id_name)
##         l.append('self.%s = %s(%s, %s, "%s")\n' %
##                  (obj.name, obj.klass, parent, id, label))
##         return l, [], []
    style = prop.get("style")
    if style: style = ", style=%s" % style
    else: style = ''
    init = []
    if id_name: init.append(id_name)
    init.append('self.%s = %s(%s, %s, "%s"%s)\n' %
                (obj.name, obj.klass, parent, id, label, style))
    props_buf = pygen.generate_common_properties(obj)
    clicked = prop.get('clicked')
    if clicked: props_buf.append('self.%s.SetValue(1)\n' % obj.name)
    return init, props_buf, []


def cpp_code_generator(obj):
    """\
    generates the C++ code for wxRadioButton objects
    """
    cppgen = common.code_writers['C++']
    prop = obj.properties
    id_name, id = cppgen.generate_code_id(obj)
    if id_name: ids = [ id_name ]
    else: ids = []
    label = prop.get('label', '').replace('"', r'\"')
    if not obj.parent.is_toplevel: parent = '%s' % obj.parent.name
    else: parent = 'this'
##     if obj.is_toplevel:
##         l = ['%s = new %s(%s, %s, "%s");\n' %
##              (obj.name, obj.klass, parent, id, label)]
##         return l, ids, [], []
    extra = ''
    style = prop.get("style")
    if style: extra = ', wxDefaultPosition, wxDefaultSize, %s' % style
    init = ['%s = new %s(%s, %s, "%s"%s);\n' %
            (obj.name, obj.klass, parent, id, label, extra) ]
    props_buf = cppgen.generate_common_properties(obj)
    clicked = prop.get('clicked')
    if clicked: props_buf.append('%s->SetValue(1);\n' % obj.name)
    return init, ids, props_buf, []


def initialize():
    common.class_names['EditRadioButton'] = 'wxRadioButton'

    # python code generation functions
    pygen = common.code_writers.get("python")
    if pygen:
        pygen.add_widget_handler('wxRadioButton', python_code_generator)
    cppgen = common.code_writers.get('C++')
    if cppgen:
##         constructor = [('wxWindow*', 'parent'), ('int', 'id'),
##                        ('const wxString&', 'label'),
##                        ('const wxPoint&', 'pos'),
##                        ('const wxSize&', 'size'),
##                        ('long', 'style', '0')]
        cppgen.add_widget_handler('wxRadioButton', cpp_code_generator)#,
                                  #constructor)
