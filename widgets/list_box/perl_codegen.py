"""\
Perl generator functions for wxListBox objects

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2017 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


import common
import wcodegen


class PerlListBoxGenerator(wcodegen.PerlWidgetCodeWriter):
    tmpl ='%(name)s = %(klass)s->new(%(parent)s, %(id)s, wxDefaultPosition, wxDefaultSize, [%(choices)s], %(style)s);\n'
    prefix_style = False
    set_default_style = True


def initialize():
    klass = 'wxListBox'
    common.class_names['EditListBox'] = klass
    common.register('perl', klass, PerlListBoxGenerator(klass))
