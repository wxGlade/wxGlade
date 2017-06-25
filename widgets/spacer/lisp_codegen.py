"""\
Lisp generator functions for spacers

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class LispSpacerGenerator(wcodegen.LispWidgetCodeWriter):
    # spacers are generally handled by a hack:
    # The the implementations of add_sizeritem() contains more details.
    # The code generation code is already implemented in base class.
    pass


def initialize():
    klass = 'spacer'
    common.class_names['EditSpacer'] = klass
    common.register('lisp', klass, LispSpacerGenerator(klass))
