"""\
Lisp code generator functions for wxStaticBitmap objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class LispStaticBitmapGenerator(wcodegen.LispWidgetCodeWriter):
    tmpl = '(setf %(name)s (%(klass)s_Create %(parent)s %(id)s  %(bitmap)s -1 -1 -1 -1 %(style)s))\n'
    tmpl_SetBestSize = ''


def initialize():
    klass = 'wxStaticBitmap'
    common.class_names['EditStaticBitmap'] = klass
    common.register('lisp', klass, LispStaticBitmapGenerator(klass))
