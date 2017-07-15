"""\
Lisp generator functions for wxChoice objects

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2017 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


import common
import wcodegen


class LispChoiceGenerator(wcodegen.LispWidgetCodeWriter):
    #tmpl = '(setf %(name)s (%(klass)s_Create %(parent)s %(id)s -1 -1 -1 -1 %(choices_len)s (vector %(choices)s)  %(style)s))\n'
    tmpl = '(setf %(name)s (%(klass)s_Create %(parent)s %(id)s -1 -1 -1 -1 %(choices_len)s (vector %(choices)s)  0))\n'


def initialize():
    klass = 'wxChoice'
    common.class_names['EditChoice'] = klass
    common.register('lisp', klass, LispChoiceGenerator(klass) )
