"""\
Code generator functions for wxDatePickerCtrl objects

@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class LispDatePickerCtrlGenerator(wcodegen.LispWidgetCodeWriter):

    tmpl = '(setf %(name)s (%(klass)s_Create %(parent)s %(id)s ' \
           '-1 -1 -1 -1 %(style)s))\n'

# end of class LispDatePickerCtrlGenerator


def initialize():
    klass = 'wxDatePickerCtrl'
    common.class_names['EditDatePickerCtrl'] = klass
    common.register('lisp', klass, LispDatePickerCtrlGenerator(klass))
