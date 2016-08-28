"""\
Code generator functions for wxChoice objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen
from ChoicesCodeHandler import *


class PythonChoiceGenerator(wcodegen.PythonWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s(%(parent)s, %(id)s, choices=[%(choices)s]%(style)s)\n'



class CppChoiceGenerator(wcodegen.CppWidgetCodeWriter):
    tmpl = '%(name)s = new %(klass)s(%(parent)s, %(id)s, ' \
           'wxDefaultPosition, wxDefaultSize, %(choices_len)s, ' \
           '%(name)s_choices%(style)s);\n'
    prefix_style = False




def xrc_code_generator(obj):
    xrcgen = common.code_writers['XRC']

    class ChoiceXrcObject(xrcgen.DefaultXrcObject):
        def write_property(self, name, val, outfile, tabs):
            if name == 'choices':
                xrc_write_choices_property(self, outfile, tabs)
            else:
                xrcgen.DefaultXrcObject.write_property(self, name, val, outfile, tabs)

    return ChoiceXrcObject(obj)


def initialize():
    klass = 'wxChoice'
    common.class_names['EditChoice'] = klass
    common.register('python', klass, PythonChoiceGenerator(klass), 'choices', ChoicesCodeHandler)
    common.register('C++',    klass, CppChoiceGenerator(klass),    'choices', ChoicesCodeHandler)
    common.register('XRC',    klass, xrc_code_generator,           'choices', ChoicesCodeHandler)
