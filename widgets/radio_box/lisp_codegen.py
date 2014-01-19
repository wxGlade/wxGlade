"""
Lisp generator functions for wxRadioBox objects

@copyright: 2002-2004 D.H. aka crazyinsomniac on sourceforge.net
@copyright: 2014 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen
from ChoicesCodeHandler import *


class LispRadioBoxGenerator(wcodegen.LispWidgetCodeWriter):
    tmpl = '(setf %(name)s (%(klass)s_Create %(parent)s %(id)s %(label)s ' \
           '-1 -1 -1 -1 %(choices_len)s (vector %(choices)s) ' \
           '%(majorDimension)s %(style)s))\n'
    default_style = 'wxRA_SPECIFY_COLS'
    has_choice = True
    set_default_style = True

    def _prepare_tmpl_content(self, obj):
        wcodegen.LispWidgetCodeWriter._prepare_tmpl_content(self, obj)
        self.tmpl_dict['majorDimension'] = obj.properties.get('dimension', '1')
        return

# end of class LispRadioBoxGenerator


def initialize():
    common.class_names['EditRadioBox'] = 'wxRadioBox'

    codegen = common.code_writers.get('lisp')
    if codegen:
        codegen.add_widget_handler('wxRadioBox', LispRadioBoxGenerator())
        codegen.add_property_handler('choices', ChoicesCodeHandler)
