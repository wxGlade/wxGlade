"""\
Perl generator functions for wxListCtrl objects
#
@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


import common
import wcodegen


class PerlListCtrlGenerator(wcodegen.PerlWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s->new(%(parent)s, %(id)s%(style)s);\n'
    default_style = 'wxLC_ICON'

# end of class PerlListCtrlGenerator


def initialize():
    klass = 'wxListCtrl'
    common.class_names['EditListCtrl'] = klass
    common.register('perl', klass, PerlListCtrlGenerator(klass))
