"""\
Code generator functions for wxListBox objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen
from ChoicesCodeHandler import *


class PythonListBoxGenerator(wcodegen.PythonWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s(%(parent)s, %(id)s, ' \
           'choices=[%(choices)s]%(style)s)\n'
    has_choice = True

# end of class PythonListBoxGenerator


class CppListBoxGenerator(wcodegen.CppWidgetCodeWriter):
    tmpl = '%(name)s = new %(klass)s(%(parent)s, %(id)s, ' \
           'wxDefaultPosition, wxDefaultSize, %(choices_len)s, ' \
           '%(name)s_choices%(style)s);\n'
    has_choice = True

# end of class CppListBoxGenerator


def xrc_code_generator(obj):
    xrcgen = common.code_writers['XRC']

    class ListBoxXrcObject(xrcgen.DefaultXrcObject):
        def write_property(self, name, val, outfile, tabs):
            if name == 'choices':
                xrc_write_choices_property(self, outfile, tabs)
            elif name == 'selection':
                choices = self.properties['choices']
                if choices:
                    xrcgen.DefaultXrcObject.write_property(self, name, val,
                                                           outfile, tabs)
            else:
                xrcgen.DefaultXrcObject.write_property(self, name, val,
                                                       outfile, tabs)

    # end of class ListBoxXrcObject

    return ListBoxXrcObject(obj)


def initialize():
    common.class_names['EditListBox'] = 'wxListBox'

    pygen = common.code_writers.get("python")
    if pygen:
        pygen.add_widget_handler('wxListBox', PythonListBoxGenerator())
        pygen.add_property_handler('choices', ChoicesCodeHandler)
    xrcgen = common.code_writers.get("XRC")
    if xrcgen:
        xrcgen.add_widget_handler('wxListBox', xrc_code_generator)
        xrcgen.add_property_handler('choices', ChoicesCodeHandler)
    cppgen = common.code_writers.get('C++')
    if cppgen:
        cppgen.add_widget_handler('wxListBox', CppListBoxGenerator())
        cppgen.add_property_handler('choices', ChoicesCodeHandler)
