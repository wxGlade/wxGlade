# codegen.py: code generator functions for wxButton objects
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

import common

def python_code_generator(obj):
    """\
    fuction that generates python code for wxButton objects.
    """
    pygen = common.code_writers['python']
    prop = obj.properties
    id_name, id = pygen.generate_code_id(obj)
    #label = '"' + prop.get('label', '').replace('"', r'\"') + '"'
    label = pygen.quote_str(prop.get('label', ''))
    if not obj.parent.is_toplevel: parent = 'self.%s' % obj.parent.name
    else: parent = 'self'
##     if obj.is_toplevel:
##         l = []
##         if id_name: l.append(id_name)
##         l.append('self.%s = %s(%s, %s, %s)\n' % (obj.name, obj.klass, parent,
##                                                  id, label))
##         return l, [], []    
    init = []
    if id_name: init.append(id_name)
    init.append('self.%s = %s(%s, %s, %s)\n' %
                (obj.name, obj.klass, parent, id, label))
    props_buf = pygen.generate_common_properties(obj)
    if prop.get('default', False):
        props_buf.append('self.%s.SetDefault()\n' % obj.name)
    return init, props_buf, []

## def python_generate_properties(obj):
##     pygen = common.code_writers['python']
##     out = []
##     if obj.properties.get('default', False):
##         out.append('self.SetDefault()\n')
##     out.extend(pygen.generate_common_properties(obj))
##     return out


def xrc_code_generator(obj):
    xrcgen = common.code_writers['XRC']
    class ButtonXrcObject(xrcgen.DefaultXrcObject):
        def write_property(self, name, val, outfile, tabs):
            if name == 'label':
                # translate & into _ as accelerator marker
                val2 = val.replace('&', '_')
                if val.count('&&') > 0:
                    while True:
                        index = val.find('&&')
                        if index < 0: break
                        val = val2[:index] + '&&' + val2[index+2:]
                else: val = val2
            xrcgen.DefaultXrcObject.write_property(self, name, val,
                                                   outfile, tabs)
    # end of class ButtonXrcObject

    return ButtonXrcObject(obj)


def cpp_code_generator(obj):
    """\
    fuction that generates python code for wxButton objects.
    """
    cppgen = common.code_writers['C++']
    prop = obj.properties
    id_name, id = cppgen.generate_code_id(obj)
    if id_name: ids = [ id_name ]
    else: ids = []
    if not obj.parent.is_toplevel: parent = '%s' % obj.parent.name
    else: parent = 'this'
    #label = '"' + prop.get('label', '').replace('"', r'\"') + '"'
    label = cppgen.quote_str(prop.get('label', ''))
##     if obj.is_toplevel:
##         l = ['%s = new %s(%s, %s, %s);\n' % (obj.name, obj.klass, parent,
##                                              id, label)]
##         return l , ids, [], []    
    init = [ '%s = new %s(%s, %s, %s);\n' % 
             (obj.name, obj.klass, parent, id, label) ]
    props_buf = cppgen.generate_common_properties(obj)
    if prop.get('default', False):
        props_buf.append('%s->SetDefault();\n' % obj.name)
    return init, ids, props_buf, []

## def cpp_generate_properties(obj):
##     cppgen = common.code_writers['C++']
##     out = []
##     if obj.properties.get('default', False):
##         out.append('SetDefault();\n')
##     out.extend(cppgen.generate_common_properties(obj))
##     return out


def initialize():
    common.class_names['EditButton'] = 'wxButton'
    pygen = common.code_writers.get('python')
    if pygen:
        pygen.add_widget_handler('wxButton', python_code_generator) #,
                                 #python_generate_properties)
    xrcgen = common.code_writers.get("XRC")
    if xrcgen:
        xrcgen.add_widget_handler('wxButton', xrc_code_generator)
    cppgen = common.code_writers.get('C++')
    if cppgen:
##         constructor = [('wxWindow*', 'parent'), ('int', 'id'),
##                        ('const wxString&', 'label'),
##                        ('const wxPoint&', 'pos', 'wxDefaultPosition'),
##                        ('const wxSize&', 'size', 'wxDefaultSize'),
##                        ('long', 'style', '0')]
        cppgen.add_widget_handler('wxButton', cpp_code_generator)#, constructor,
                                  #cpp_generate_properties)
