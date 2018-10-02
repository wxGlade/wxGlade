"""\
Perl generator functions for wxCheckListBox objects

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014 Carsten Grohmann
@copyright: 2015 Franco Bugnano
@copyright: 2017 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


import common
import wcodegen


class PerlCheckListBoxGenerator(wcodegen.PerlWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s->new(%(parent)s, %(id)s, ' \
           'wxDefaultPosition, wxDefaultSize, [%(choices)s], %(style)s);\n'
    prefix_style = False


def initialize():
    klass = 'wxCheckListBox'
    common.class_names['EditCheckListBox'] = klass
    common.register('perl', klass, PerlCheckListBoxGenerator(klass))
