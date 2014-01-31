"""\
Code generator functions for wxDatePickerCtrl objects

@copyright: 2014 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class LispDatePickerCtrlGenerator(wcodegen.LispWidgetCodeWriter):

    tmpl = '(setf %(name)s (%(klass)s_Create %(parent)s %(id)s ' \
           '-1 -1 -1 -1 %(style)s))\n'

# end of class LispDatePickerCtrlGenerator


def initialize():
    common.class_names['EditDatePickerCtrl'] = 'wxDatePickerCtrl'
    lispgen = common.code_writers.get('lisp')

    if lispgen:
        lispgen.add_widget_handler('wxDatePickerCtrl',
                                   LispDatePickerCtrlGenerator())
