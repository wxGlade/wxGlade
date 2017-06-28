"""\
Perl generator functions for wxChoice objects

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2017 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


import common
import wcodegen


class PerlChoiceGenerator(wcodegen.PerlWidgetCodeWriter):
    #tmpl ='%(name)s = %(klass)s->new(%(parent)s, %(id)s, wxDefaultPosition, wxDefaultSize, [%(choices)s], %(style)s);\n'
    tmpl ='%(name)s = %(klass)s->new(%(parent)s, %(id)s, wxDefaultPosition, wxDefaultSize, [%(choices)s], );\n'



def initialize():
    klass = 'wxChoice'
    common.class_names['EditChoice'] = klass
    common.register('perl', klass, PerlChoiceGenerator(klass) )
