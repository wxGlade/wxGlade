"""\
Perl generator functions for wxToggleButton objects

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PerlToggleButtonGenerator(wcodegen.PerlWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s->new(%(parent)s, %(id)s, %(label)s);\n'
    has_setvalue = True

# end of class PerlToggleButtonGenerator


def initialize():
    common.class_names['EditToggleButton'] = 'wxToggleButton'
    plgen = common.code_writers.get('perl')

    if plgen:
        plgen.add_widget_handler('wxToggleButton', PerlToggleButtonGenerator())
