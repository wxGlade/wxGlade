"""\
Code generator functions for wxCheckListBox objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014 Carsten Grohmann
@copyright: 2015 Franco Bugnano
@copyright: 2017 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen
from ChoicesCodeHandler import *

class PythonCheckListBoxGenerator(wcodegen.PythonWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s(%(parent)s, %(id)s, choices=[%(choices)s]%(style)s)\n'


class CppCheckListBoxGenerator(wcodegen.CppWidgetCodeWriter):
    tmpl = '%(name)s = new %(klass)s(%(parent)s, %(id)s, ' \
           'wxDefaultPosition, wxDefaultSize, %(choices_len)s, %(name)s_choices%(style)s);\n'
    prefix_style = False
    tmpl_flags = ', %s'


def xrc_code_generator(obj):
    xrcgen = common.code_writers['XRC']

    class CheckListBoxXrcObject(xrcgen.DefaultXrcObject):
        def write_property(self, name, val, output, tabs):
            if name == 'choices':
                xrc_write_choices_property(self, output, tabs)
            elif name == 'selection':
                choices = self.widget.properties['choices']
                if choices.is_active():
                    xrcgen.DefaultXrcObject.write_property(self, name, val, output, tabs)
            else:
                xrcgen.DefaultXrcObject.write_property(self, name, val, output, tabs)

    return CheckListBoxXrcObject(obj)


def initialize():
    klass = 'wxCheckListBox'
    common.class_names['EditCheckListBox'] = klass
    common.register('python', klass, PythonCheckListBoxGenerator(klass) )
    common.register('C++',    klass, CppCheckListBoxGenerator(klass) )
    common.register('XRC',    klass, xrc_code_generator )
