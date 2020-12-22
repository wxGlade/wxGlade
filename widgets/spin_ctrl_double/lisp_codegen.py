"""\
Lisp generator functions for wxSpinCtrl objects

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class LispSpinCtrlDoubleGenerator(wcodegen.LispWidgetCodeWriter):
    tmpl = '(setf %(name)s (%(klass)s_Create %(parent)s %(id)s %(value)s ' \
           '-1 -1 -1 -1 %(style)s %(minValue)s %(maxValue)s %(value)s))\n'
    set_default_style = True

    def _prepare_tmpl_content(self, obj):
        wcodegen.LispWidgetCodeWriter._prepare_tmpl_content(self, obj)
        self.tmpl_dict['value'] = obj.value
        try:
            minValue, maxValue = obj.properties["range"].get_tuple()
        except:
            minValue, maxValue = '0', '100'
        self.tmpl_dict['minValue'] = minValue
        self.tmpl_dict['maxValue'] = maxValue
        return

    def get_more_properties_code(self, obj):
        ret = []
        klass = self.tmpl_dict['klass']
        name = self.tmpl_dict['name']
        if obj.properties["increment"].is_active():
            ret.append( '(%s_SetIncrement %s %s)\n'%(klass, name, obj.increment) )
        if obj.properties["digits"].is_active():
            ret.append( '(%s_SetDigits %s %s)\n'%(klass, name, obj.digits) )
        return ret


def initialize():
    klass = 'wxSpinCtrlDouble'
    common.class_names['EditSpinCtrlDouble'] = klass
    common.register('lisp', klass, LispSpinCtrlDoubleGenerator(klass))
