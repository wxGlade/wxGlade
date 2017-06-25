"""\
Lisp generator functions for wxStaticLine objects

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


import common
import wcodegen


class LispStaticLineGenerator(wcodegen.LispWidgetCodeWriter):
    tmpl = '(setf %(name)s (%(klass)s_Create %(parent)s %(id)s -1 -1 -1 -1 %(style)s))\n'


def initialize():
    klass = 'wxStaticLine'
    common.class_names['EditStaticLine'] = klass
    common.register('lisp', klass, LispStaticLineGenerator(klass))
