# codegen.py: code generator functions for wxComboBox objects
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: GPL (see license.txt)

import common
from ChoicesCodeHandler import *

def python_code_generator(obj):
    """\
    generates the python code for wxComboBox objects
    """
    pygen = common.code_writers['python']
    prop = obj.properties
    id_name, id = pygen.generate_code_id(obj)
    choices = prop.get('choices', [])
    if not obj.parent.is_toplevel: parent = 'self.%s' % obj.parent.name
    else: parent = 'self'
    if obj.is_toplevel:
        l = ['self.%s = %s(%s, %s, choices=%s)\n' % \
             (obj.name, obj.klass, parent, id, repr(choices))]
        if id_name: l.append(id_name)
        return l, [], []
    style = prop.get("style", None)
    if not style: style = 'wxCB_DROPDOWN'
    else: style = 'wxCB_DROPDOWN|' + style
    init = ['self.%s = wxComboBox(%s, %s, choices=%s, style=%s)\n' %
            (obj.name, parent, id, repr(choices), style) ]
    if id_name: init.append(id_name)
    props_buf = pygen.generate_common_properties(obj)
    selection = prop.get('selection')
    if selection is not None:
        props_buf.append('self.%s.SetSelection(%s)\n' % (obj.name, selection))
    return init, props_buf, []


def xrc_code_generator(obj):
    xrcgen = common.code_writers['XRC']
    class ComboBoxXrcObject(xrcgen.DefaultXrcObject):
        def write_property(self, name, val, outfile, tabs):
            if name == 'choices':
                xrc_write_choices_property(self, outfile, tabs)
            else:
                xrcgen.DefaultXrcObject.write_property(self, name, val,
                                                       outfile, tabs)

    # end of class ComboBoxXrcObject

    return ComboBoxXrcObject(obj)


def initialize():
    common.class_names['EditComboBox'] = 'wxComboBox'

    pygen = common.code_writers.get("python")
    if pygen:
        pygen.add_widget_handler('wxComboBox', python_code_generator)
        pygen.add_property_handler('choices', ChoicesCodeHandler)
    xrcgen = common.code_writers.get("XRC")
    if xrcgen:
        xrcgen.add_widget_handler('wxComboBox', xrc_code_generator)
        xrcgen.add_property_handler('choices', ChoicesCodeHandler)

