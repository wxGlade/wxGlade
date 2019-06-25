"""\
Lisp generator functions for spacers

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen
from . import spacer_base


class LispSpacerGenerator(spacer_base.SpacerMixin, wcodegen.LispWidgetCodeWriter):
    pass


def initialize():
    klass = 'spacer'
    common.class_names['EditSpacer'] = klass
    common.register('lisp', klass, LispSpacerGenerator(klass))
