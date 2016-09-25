"""\
Code generator functions for wxListBox objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen
from ChoicesCodeHandler import *


class PythonListBoxGenerator(wcodegen.PythonWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s(%(parent)s, %(id)s, choices=[%(choices)s]%(style)s)\n'



class CppListBoxGenerator(wcodegen.CppWidgetCodeWriter):
    tmpl = '%(name)s = new %(klass)s(%(parent)s, %(id)s, ' \
           'wxDefaultPosition, wxDefaultSize, %(choices_len)s, ' \
           '%(name)s_choices, %(style)s);\n'

    prefix_style = False
    set_default_style = True



def xrc_code_generator(obj):
    xrcgen = common.code_writers['XRC']

    class ListBoxXrcObject(xrcgen.DefaultXrcObject):
        def write_property(self, name, val, outfile, tabs):
            if name == 'choices':
                xrc_write_choices_property(self, outfile, tabs)
            elif name == 'selection':
                choices = self.properties['choices']
                if choices:
                    xrcgen.DefaultXrcObject.write_property(self, name, val, outfile, tabs)
            else:
                xrcgen.DefaultXrcObject.write_property(self, name, val, outfile, tabs)

    return ListBoxXrcObject(obj)


def initialize():
    klass = 'wxListBox'
    common.class_names['EditListBox'] = klass
    common.register('python', klass, PythonListBoxGenerator(klass), 'choices', ChoicesCodeHandler)
    common.register('C++',    klass, CppListBoxGenerator(klass),    'choices', ChoicesCodeHandler)
    common.register('XRC',    klass, xrc_code_generator,            'choices', ChoicesCodeHandler)
