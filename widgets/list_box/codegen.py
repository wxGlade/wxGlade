"""\
Code generator functions for wxListBox objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2017 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen
from ChoicesCodeHandler import *


class PythonListBoxGenerator(wcodegen.PythonWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s(%(parent)s, %(id)s, choices=[%(choices)s]%(style)s)\n'


class CppListBoxGenerator(wcodegen.CppWidgetCodeWriter):
    tmpl = '%(name)s = new %(klass)s(%(parent)s, %(id)s, ' \
           'wxDefaultPosition, wxDefaultSize, %(choices_len)s, %(name)s_choices, %(style)s);\n'
    prefix_style = False
    set_default_style = True


def xrc_code_generator(obj):
    xrcgen = common.code_writers['XRC']

    class ListBoxXrcObject(xrcgen.DefaultXrcObject):
        def write(self, out_file, ntabs):
            style = self.widget.properties["style"].get_string_value()
            properties = {"style":style}
            xrcgen.DefaultXrcObject.write(self, out_file, ntabs, properties)

        def write_property(self, name, val, output, tabs):
            if name == 'choices':
                xrc_write_choices_property(self, output, tabs)
            elif name == 'selection':
                choices = self.widget.properties['choices']
                if choices.is_active() and val!='-1':
                    xrcgen.DefaultXrcObject.write_property(self, name, val, output, tabs)
            else:
                xrcgen.DefaultXrcObject.write_property(self, name, val, output, tabs)

    return ListBoxXrcObject(obj)


def initialize():
    klass = 'wxListBox'
    common.class_names['EditListBox'] = klass
    common.register('python', klass, PythonListBoxGenerator(klass) )
    common.register('C++',    klass, CppListBoxGenerator(klass) )
    common.register('XRC',    klass, xrc_code_generator )
