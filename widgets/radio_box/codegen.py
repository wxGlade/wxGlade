# codegen.py: code generator functions for wxRadioBox objects
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: GPL (see license.txt)

import common
from ChoicesCodeHandler import *

def python_code_generator(obj):
    """\
    generates the python code for wxRadioBox objects
    """
    pygen = common.code_writers['python']
    prop = obj.properties
    id_name, id = pygen.generate_code_id(obj) 
    label = prop.get('label', '').replace('"', r'\"')
    choices = prop.get('choices', [])
    major_dim = prop.get('dimension', '0')
    if obj.is_toplevel:
        l = ['self.%s = %s(self, %s, "%s", choices=%s, majorDimension=%s)\n'  %
             (obj.name, obj.klass, id, label, repr(choices), major_dim)]
        if id_name: l.append(id_name)
        return l, [], []
    size = pygen.generate_code_size(obj)
    if not obj.parent.is_toplevel: parent = 'self.%s' % obj.parent.name
    else: parent = 'self'
    style = prop.get("style", "wxRA_SPECIFY_COLS")
    if not style: style = "wxRA_SPECIFY_COLS"
    init = ['self.%s = wxRadioBox(%s, %s, "%s", choices=%s, majorDimension=%s,'
            ' size=%s, style=%s)\n' % (obj.name, parent, id, label,
                                      repr(choices), major_dim, size, style) ]
    if id_name: init.append(id_name)
    props_buf = []
    selection = prop.get('selection')
    if selection is not None:
        props_buf.append('self.%s.SetSelection(%s)\n' % (obj.name, selection))
    if prop.has_key('foreground'):
        props_buf.append(pygen.generate_code_foreground(obj))
    if prop.has_key('background'):
        props_buf.append(pygen.generate_code_background(obj))
    if prop.has_key('font'): props_buf.append(pygen.generate_code_font(obj))
    return init, props_buf, []


def xrc_code_generator(obj):
    xrcgen = common.code_writers['XRC']
    class RadioBoxXrcObject(xrcgen.DefaultXrcObject):
        def write_property(self, name, val, outfile, tabs):
            if name == 'choices':
                xrc_write_choices_property(self, outfile, tabs)
            elif name == 'major_dim' and val:
                outfile.write('    '*tabs + '<dimension>%s</dimension>\n' % \
                              val)
            else:
                xrcgen.DefaultXrcObject.write_property(self, name, val,
                                                       outfile, tabs)

    # end of class RadioBoxXrcObject

    return RadioBoxXrcObject(obj)


def initialize():
    common.class_names['EditRadioBox'] = 'wxRadioBox'

    pygen = common.code_writers.get("python")
    if pygen:
        pygen.add_widget_handler('wxRadioBox', python_code_generator)
        pygen.add_property_handler('choices', ChoicesCodeHandler)
    xrcgen = common.code_writers.get("XRC")
    if xrcgen:
        xrcgen.add_widget_handler('wxRadioBox', xrc_code_generator)
        xrcgen.add_property_handler('choices', ChoicesCodeHandler)
