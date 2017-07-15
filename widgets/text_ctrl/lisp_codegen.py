"""\
Lisp generator functions for wxTextCtrl objects

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class LispTextCtrlGenerator(wcodegen.LispWidgetCodeWriter):
    tmpl = '(setf %(name)s (%(klass)s_Create %(parent)s %(id)s %(value)s -1 -1 -1 -1 %(style)s))\n'


def initialize():
    klass = 'wxTextCtrl'
    common.class_names['EditTextCtrl'] = klass
    common.register('lisp', klass, LispTextCtrlGenerator(klass))
