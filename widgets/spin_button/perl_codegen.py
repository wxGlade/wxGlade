"""\
Perl generator functions for wxSpinButton objects

@copyright: 2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2026 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PerlSpinButtonGenerator(wcodegen.PerlWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s->new(%(parent)s, %(id)s,%(style)s);\n'

    def get_more_properties_code(self, obj):
        ret = []
        name = self.tmpl_dict['name']
        if obj.properties["range"].is_active():
            mi,ma = obj.properties["range"].get_tuple()
            ret.append( '%s->SetRange(%s, %s)\n'%(name, mi, ma) )
        if obj.properties["value"].is_active():
            ret.append( '%s->SetValue(%s)\n'%(name, obj.value) )
        return ret


def initialize():
    klass = 'wxSpinButton'
    common.class_names['EditSpinButton'] = klass
    common.register('perl', klass, PerlSpinButtonGenerator(klass))
