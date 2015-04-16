"""
Perl generator functions for wxRadioBox objects

@copyright: 2002-2004 D.H. aka crazyinsomniac on sourceforge.net
@copyright: 2014-2015 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen
from ChoicesCodeHandler import *


class PerlRadioBoxGenerator(wcodegen.PerlWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s->new(%(parent)s, %(id)s, %(label)s, ' \
           'wxDefaultPosition, wxDefaultSize, [%(choices)s], ' \
           '%(majorDimension)s, %(style)s);\n'

    default_style = 'wxRA_SPECIFY_COLS'
    prefix_style = False
    set_default_style = True

    def _prepare_tmpl_content(self, obj):
        wcodegen.PerlWidgetCodeWriter._prepare_tmpl_content(self, obj)
        self.tmpl_dict['majorDimension'] = obj.properties.get('dimension', '1')
        return

# end of class PerlRadioBoxGenerator


def initialize():
    klass = 'wxRadioBox'
    common.class_names['EditRadioBox'] = klass
    common.register('perl', klass, PerlRadioBoxGenerator(klass),
                    'choices', ChoicesCodeHandler)
