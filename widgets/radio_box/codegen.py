"""
Code generator functions for wxRadioBox objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2017 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen
from . import radio_box_base
from ChoicesCodeHandler import *


class PythonRadioBoxGenerator(radio_box_base.RadioBoxMixin, wcodegen.PythonWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s(%(parent)s, %(id)s, %(label)s, ' \
           'choices=[%(choices)s], majorDimension=%(majorDimension)s%(style)s)\n'

    def _prepare_choice(self, obj):
        # avoid empty choices, which causes a crash in classic wxPython; the box will still be empty with just [""]
        if obj.choices or self.language!="python":
            return wcodegen.PythonWidgetCodeWriter._prepare_choice(self, obj)

        self.tmpl_dict['choices'] = '""'
        self.tmpl_dict['choices_len'] = 1


class CppRadioBoxGenerator(radio_box_base.RadioBoxMixin, wcodegen.CppWidgetCodeWriter):
    tmpl = '%(name)s = new %(klass)s(%(parent)s, %(id)s, %(label)s, ' \
           'wxDefaultPosition, wxDefaultSize, %(choices_len)s, %(name)s_choices, %(majorDimension)s, %(style)s);\n'
    prefix_style = False


def xrc_code_generator(obj):
    xrcgen = common.code_writers['XRC']

    class RadioBoxXrcObject(xrcgen.DefaultXrcObject):
        def write_property(self, name, val, output, tabs):
            if name == 'choices':
                xrc_write_choices_property(self, output, tabs)
            else:
                xrcgen.DefaultXrcObject.write_property(self, name, val, output, tabs)

    return RadioBoxXrcObject(obj)


def initialize():
    klass = 'wxRadioBox'
    common.class_names['EditRadioBox'] = klass
    common.register('python', klass, PythonRadioBoxGenerator(klass) )
    common.register('C++',    klass, CppRadioBoxGenerator(klass) )
    common.register('XRC',    klass, xrc_code_generator )
