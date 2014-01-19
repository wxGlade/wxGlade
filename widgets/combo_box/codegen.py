"""\
Code generator functions for wxComboBox objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


import common
import wcodegen
from ChoicesCodeHandler import *


class PythonComboBoxGenerator(wcodegen.PythonWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s(%(parent)s, %(id)s, ' \
           'choices=[%(choices)s]%(style)s)\n'
    default_style = 'wxCB_DROPDOWN'
    set_default_style = True
    has_choice = True

# end of class PythonComboBoxGenerator


class CppComboBoxGenerator(wcodegen.CppWidgetCodeWriter):
    tmpl = '%(name)s = new %(klass)s(%(parent)s, %(id)s, wxT(""), ' \
           'wxDefaultPosition, wxDefaultSize, %(choices_len)s, ' \
           '%(name)s_choices, %(style)s);\n'

    default_style = 'wxCB_DROPDOWN'
    has_choice = True
    prefix_style = False
    set_default_style = True

# end of class CppComboBoxGenerator


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
        pygen.add_widget_handler('wxComboBox', PythonComboBoxGenerator())
        pygen.add_property_handler('choices', ChoicesCodeHandler)
    xrcgen = common.code_writers.get("XRC")
    if xrcgen:
        xrcgen.add_widget_handler('wxComboBox', xrc_code_generator)
        xrcgen.add_property_handler('choices', ChoicesCodeHandler)
    cppgen = common.code_writers.get('C++')
    if cppgen:
        cppgen.add_widget_handler('wxComboBox', CppComboBoxGenerator())
        cppgen.add_property_handler('choices', ChoicesCodeHandler)
