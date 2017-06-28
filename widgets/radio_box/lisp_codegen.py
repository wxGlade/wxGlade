"""
Lisp generator functions for wxRadioBox objects

@copyright: 2002-2004 D.H. aka crazyinsomniac on sourceforge.net
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2017 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen
from . import radio_box_base


class LispRadioBoxGenerator(radio_box_base.RadioBoxMixin, wcodegen.LispWidgetCodeWriter):
    tmpl = '(setf %(name)s (%(klass)s_Create %(parent)s %(id)s %(label)s ' \
           '-1 -1 -1 -1 %(choices_len)s (vector %(choices)s) %(majorDimension)s %(style)s))\n'


def initialize():
    klass = 'wxRadioBox'
    common.class_names['EditRadioBox'] = klass
    common.register('lisp', klass, LispRadioBoxGenerator(klass) )
