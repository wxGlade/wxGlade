"""\
Perl generator functions for wxSlider objects

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PerlSliderGenerator(wcodegen.PerlWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s->new(%(parent)s, %(id)s, %(value)s, %(minValue)s, %(maxValue)s%(style)s);\n'

    def _prepare_tmpl_content(self, obj):
        wcodegen.PerlWidgetCodeWriter._prepare_tmpl_content(self, obj)
        self.tmpl_dict['value'] = obj.value
        try:
            minValue, maxValue = obj.properties["range"].get_tuple()
        except:
            minValue, maxValue = '0', '10'
        self.tmpl_dict['minValue'] = minValue
        self.tmpl_dict['maxValue'] = maxValue
        return



def initialize():
    klass = 'wxSlider'
    common.class_names['EditSlider'] = klass
    common.register('perl', klass, PerlSliderGenerator(klass))
