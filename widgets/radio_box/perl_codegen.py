"""
Perl generator functions for wxRadioBox objects

@copyright: 2002-2004 D.H. aka crazyinsomniac on sourceforge.net
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2017 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen
from . import radio_box_base


class PerlRadioBoxGenerator(radio_box_base.RadioBoxMixin, wcodegen.PerlWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s->new(%(parent)s, %(id)s, %(label)s, ' \
           'wxDefaultPosition, wxDefaultSize, [%(choices)s], %(majorDimension)s, %(style)s);\n'
    prefix_style = False


def initialize():
    klass = 'wxRadioBox'
    common.class_names['EditRadioBox'] = klass
    common.register('perl', klass, PerlRadioBoxGenerator(klass) )
