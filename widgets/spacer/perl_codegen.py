"""\
Perl generator functions for spacers

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PerlSpacerGenerator(wcodegen.PerlWidgetCodeWriter):

    # spacers are generally handled by a hack:
    # The the implementations of add_sizeritem() contains more details.
    # The code generation code is already implemented in base class.
    pass

# end of class PerlSpacerGenerator


def initialize():
    common.class_names['EditSpacer'] = 'spacer'

    plgen = common.code_writers.get('perl')
    if plgen:
        plgen.add_widget_handler('spacer', PerlSpacerGenerator())
