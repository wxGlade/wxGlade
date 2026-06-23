"""\
Lisp generator functions for wxSpinButton objects

@copyright: 2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2026 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class LispSpinButtonGenerator(wcodegen.LispWidgetCodeWriter):
    tmpl = '(setf %(name)s (%(klass)s_Create %(parent)s %(id)s -1 -1 -1 -1 %(style)s))\n'

    def get_more_properties_code(self, obj):
        ret = []
        klass = self.tmpl_dict['klass']
        name = self.tmpl_dict['name']
        if obj.properties["range"].is_active():
            mi,ma = obj.properties["range"].get_tuple()
            ret.append( '(%s_SetRange %s %s %s)\n'%(klass, name, mi, ma) )
        if obj.properties["value"].is_active():
            ret.append( '(%s_SetValue %s %s)\n'%(klass, name, obj.value) )
        return ret


def initialize():
    klass = 'wxSpinButton'
    common.class_names['EditSpinButton'] = klass
    common.register('lisp', klass, LispSpinButtonGenerator(klass))
