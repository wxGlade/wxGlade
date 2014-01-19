"""
Perl generator functions for wxRadioBox objects

@copyright: 2002-2004 D.H. aka crazyinsomniac on sourceforge.net
@copyright: 2014 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen
from ChoicesCodeHandler import *


class PerlCodeGenerator(wcodegen.PerlWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s->new(%(parent)s, %(id)s, %(label)s, ' \
           'wxDefaultPosition, wxDefaultSize, [%(choices)s], ' \
           '%(majorDimension)s, %(style)s);\n'
    default_style = 'wxRA_SPECIFY_COLS'
    has_choice = True
    prefix_style = False
    set_default_style = True

    def _prepare_tmpl_content(self, obj):
        wcodegen.PerlWidgetCodeWriter._prepare_tmpl_content(self, obj)
        self.tmpl_dict['majorDimension'] = obj.properties.get('dimension', '1')
        return

# end of class PerlCodeGenerator


def initialize():
    common.class_names['EditRadioBox'] = 'wxRadioBox'

    plgen = common.code_writers.get('perl')
    if plgen:
        plgen.add_widget_handler('wxRadioBox', PerlCodeGenerator())
        plgen.add_property_handler('choices', ChoicesCodeHandler)
