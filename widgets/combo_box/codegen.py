"""\
Code generator functions for wxComboBox objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2017 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


import common
import wcodegen
from ChoicesCodeHandler import *


class PythonComboBoxGenerator(wcodegen.PythonWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s(%(parent)s, %(id)s, choices=[%(choices)s]%(style)s)\n'
    set_default_style = True


class CppComboBoxGenerator(wcodegen.CppWidgetCodeWriter):
    tmpl = '%(name)s = new %(klass)s(%(parent)s, %(id)s, wxT(""), ' \
           'wxDefaultPosition, wxDefaultSize, %(choices_len)s, %(name)s_choices, %(style)s);\n'
    prefix_style = False
    set_default_style = True


def xrc_code_generator(obj):
    xrcgen = common.code_writers['XRC']

    class ComboBoxXrcObject(xrcgen.DefaultXrcObject):
        def write_property(self, name, val, output, tabs):
            if name == 'choices':
                xrc_write_choices_property(self, output, tabs)
            else:
                xrcgen.DefaultXrcObject.write_property(self, name, val, output, tabs)

    return ComboBoxXrcObject(obj)


def initialize():
    klass = 'wxComboBox'
    common.class_names['EditComboBox'] = klass
    common.register('python', klass, PythonComboBoxGenerator(klass) )
    common.register('C++',    klass, CppComboBoxGenerator(klass) )
    common.register('XRC',    klass, xrc_code_generator )
