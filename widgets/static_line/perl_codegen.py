"""\
Perl generator functions for wxStaticLine objects

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


import common
import wcodegen


class PerlStaticLineGenerator(wcodegen.PerlWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s->new(%(parent)s, %(id)s%(style)s);\n'


def initialize():
    klass = 'wxStaticLine'
    common.class_names['EditStaticLine'] = klass
    common.register('perl', klass, PerlStaticLineGenerator(klass))
