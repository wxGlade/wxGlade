"""\
Perl generator functions for wxTreeCtrl objects

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PerlTreeCtrlGenerator(wcodegen.PerlWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s->new(%(parent)s, %(id)s%(style)s);\n'
    default_style = 'wxTR_HAS_BUTTONS'

# end of class PerlTreeCtrlGenerator


def initialize():
    common.class_names['EditTreeCtrl'] = 'wxTreeCtrl'
    plgen = common.code_writers.get('perl')

    if plgen:
        plgen.add_widget_handler('wxTreeCtrl', PerlTreeCtrlGenerator())
