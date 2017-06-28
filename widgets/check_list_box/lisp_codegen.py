"""\
Lisp generator functions for wxCheckListBox objects

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014 Carsten Grohmann
@copyright: 2015 Franco Bugnano
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


import common
import wcodegen


class LispCheckListBoxGenerator(wcodegen.LispWidgetCodeWriter):
    tmpl = '(setf %(name)s (%(klass)s_Create %(parent)s %(id)s -1 -1 -1 ' \
           '-1 %(choices_len)s (vector %(choices)s) %(style)s))\n'


def initialize():
    klass = 'wxCheckListBox'
    common.class_names['EditCheckListBox'] = klass
    common.register('lisp', klass, LispCheckListBoxGenerator(klass) )
