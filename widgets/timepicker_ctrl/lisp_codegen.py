"""\
Code generator functions for wxTimePickerCtrl objects

@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class LispTimePickerCtrlGenerator(wcodegen.LispWidgetCodeWriter):
    tmpl = '(setf %(name)s (%(klass)s_Create %(parent)s %(id)s -1 -1 -1 -1 %(style)s))\n'


def initialize():
    klass = 'wxTimePickerCtrl'
    common.class_names['EditTimePickerCtrl'] = klass
    common.register('lisp', klass, LispTimePickerCtrlGenerator(klass))
