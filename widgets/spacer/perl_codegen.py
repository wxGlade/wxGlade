"""\
Perl generator functions for spacers

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2019 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen
from . import spacer_base


class PerlSpacerGenerator(spacer_base.SpacerMixin, wcodegen.PerlWidgetCodeWriter):
    pass


def initialize():
    klass = 'spacer'
    common.class_names['EditSpacer'] = klass
    common.register('perl', klass, PerlSpacerGenerator(klass))
