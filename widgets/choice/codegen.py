# codegen.py: code generator functions for wxChoice objects
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

import common
from ChoicesCodeHandler import *

def python_code_generator(obj):
    """\
    generates the python code for wxChoice objects
    """
    pygen = common.code_writers['python']
    prop = obj.properties
    id_name, id = pygen.generate_code_id(obj)
    choices = prop.get('choices', [])
    if not obj.parent.is_toplevel: parent = 'self.%s' % obj.parent.name
    else: parent = 'self'
##     if obj.is_toplevel:
##         l = []
##         if id_name: l.append(id_name)
##         l.append('self.%s = %s(%s, %s, choices=%s)\n' % \
##                  (obj.name, obj.klass, parent, id, repr(choices)))
##         return l, [], []
    style = prop.get("style")
    if style: style = ", style=%s" % style
    else: style = ''
    init = []
    if id_name: init.append(id_name)
    choices = ', '.join([pygen.quote_str(c) for c in choices])
    init.append('self.%s = %s(%s, %s, choices=[%s]%s)\n' %
                (obj.name, obj.klass, parent, id, choices, style))
    props_buf = pygen.generate_common_properties(obj)
    selection = prop.get('selection')
    if selection is not None:
        props_buf.append('self.%s.SetSelection(%s)\n' % (obj.name, selection))
    return init, props_buf, []   


def xrc_code_generator(obj):
    xrcgen = common.code_writers['XRC']
    class ChoiceXrcObject(xrcgen.DefaultXrcObject):
        def write_property(self, name, val, outfile, tabs):
            if name == 'choices':
                xrc_write_choices_property(self, outfile, tabs)
            else:
                xrcgen.DefaultXrcObject.write_property(self, name, val,
                                                       outfile, tabs)

    # end of class ChoiceXrcObject

    return ChoiceXrcObject(obj)


def cpp_code_generator(obj):
    """\
    generates the C++ code for wxChoice objects
    """
    cppgen = common.code_writers['C++']
    prop = obj.properties
    id_name, id = cppgen.generate_code_id(obj)
    if id_name: ids = [ id_name ]
    else: ids = []
    choices = prop.get('choices', [])
    if not obj.parent.is_toplevel: parent = '%s' % obj.parent.name
    else: parent = 'this'
    number = len(choices)
    ch_arr = '{\n        %s\n    };\n' % \
             ',\n        '.join([cppgen.quote_str(c) for c in choices])
##     if obj.is_toplevel:
##         l = []
##         l.append('const wxString %s_choices[] = %s' % (obj.name, ch_arr))
##         l.append('%s = new %s(%s, %s, wxDefaultPosition, wxDefaultSize, %s, '
##                  '%s_choices);\n' % \
##                  (obj.name, obj.klass, parent, id, number, obj.name))
##         return l, ids, [], []
    style = prop.get("style", "0")
    init = []
    init.append('const wxString %s_choices[] = %s' % (obj.name, ch_arr))
    init.append('%s = new %s(%s, %s, wxDefaultPosition, wxDefaultSize, '
                '%s, %s_choices, %s);\n' % \
                (obj.name, obj.klass, parent, id, number, obj.name, style))
    props_buf = cppgen.generate_common_properties(obj)
    selection = prop.get('selection')
    if selection is not None:
        props_buf.append('%s->SetSelection(%s);\n' % (obj.name, selection))
    return init, ids, props_buf, []   


def initialize():
    common.class_names['EditChoice'] = 'wxChoice'

    pygen = common.code_writers.get("python")
    if pygen:
        pygen.add_widget_handler('wxChoice', python_code_generator)
        pygen.add_property_handler('choices', ChoicesCodeHandler)
    xrcgen = common.code_writers.get("XRC")
    if xrcgen:
        xrcgen.add_widget_handler('wxChoice', xrc_code_generator)
        xrcgen.add_property_handler('choices', ChoicesCodeHandler)
    cppgen = common.code_writers.get('C++')
    if cppgen:
##         constructor = [('wxWindow*', 'parent'), ('int', 'id'),
##                        ('const wxPoint&', 'pos'),
##                        ('const wxSize&', 'size'),
##                        ('int', 'n'), ('const wxString*', 'choices'),
##                        ('long', 'style', '0')]
        cppgen.add_widget_handler('wxChoice', cpp_code_generator)#, constructor)
        cppgen.add_property_handler('choices', ChoicesCodeHandler)
