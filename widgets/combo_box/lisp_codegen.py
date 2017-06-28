"""\
Lisp generator functions for wxComboBox objects

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2017 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


import common
import wcodegen


class LispComboBoxGenerator(wcodegen.LispWidgetCodeWriter):
    tmpl = '(setf %(name)s (%(klass)s_Create %(parent)s %(id)s "" -1 -1 -1 ' \
           '-1 %(choices_len)s (vector %(choices)s) %(style)s))\n'
    set_default_style = True


def initialize():
    klass = 'wxComboBox'
    common.class_names['EditComboBox'] = klass
    common.register('lisp', klass, LispComboBoxGenerator(klass) )
