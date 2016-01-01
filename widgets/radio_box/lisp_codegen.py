"""
Lisp generator functions for wxRadioBox objects

@copyright: 2002-2004 D.H. aka crazyinsomniac on sourceforge.net
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen
import radio_box_base
from ChoicesCodeHandler import *


class LispRadioBoxGenerator(radio_box_base.RadioBoxMixin,
                            wcodegen.LispWidgetCodeWriter):
    tmpl = '(setf %(name)s (%(klass)s_Create %(parent)s %(id)s %(label)s ' \
           '-1 -1 -1 -1 %(choices_len)s (vector %(choices)s) ' \
           '%(majorDimension)s %(style)s))\n'

# end of class LispRadioBoxGenerator


def initialize():
    klass = 'wxRadioBox'
    common.class_names['EditRadioBox'] = klass
    common.register('lisp', klass, LispRadioBoxGenerator(klass),
                    'choices', ChoicesCodeHandler)
