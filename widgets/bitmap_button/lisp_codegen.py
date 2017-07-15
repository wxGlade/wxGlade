"""\
Lisp generator functions for wxBitmapButton objects

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class LispBitmapButtonGenerator(wcodegen.LispWidgetCodeWriter):
    tmpl = '(setf %(name)s (%(klass)s_Create %(parent)s %(id)s %(bitmap)s -1 -1 -1 -1 %(style)s))\n'
    tmpl_setdefault = '(wxButton_SetDefault %(name)s)\n'


def initialize():
    klass = 'wxBitmapButton'
    common.class_names['EditBitmapButton'] = klass
    common.register('lisp', klass, LispBitmapButtonGenerator(klass))
