"""\
Perl generator functions for wxSpinCtrl objects

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PerlSpinCtrlDoubleGenerator(wcodegen.PerlWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s->new(%(parent)s, %(id)s, "%(value)s", ' \
           'wxDefaultPosition, wxDefaultSize, %(style)s, %(minValue)s, ' '%(maxValue)s, %(value)s);\n'
    prefix_style = False
    set_default_style = True

    def _prepare_tmpl_content(self, obj):
        wcodegen.PerlWidgetCodeWriter._prepare_tmpl_content(self, obj)
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
        name = self.tmpl_dict['name']
        if obj.properties["increment"].is_active():
            ret.append( '%s->SetIncrement(%s);\n'%(name, obj.increment) )
        if obj.properties["digits"].is_active():
            ret.append( '%s->SetDigits(%s);\n'%(name, obj.digits) )
        return ret


def initialize():
    klass = 'wxSpinCtrlDouble'
    common.class_names['EditSpinCtrlDouble'] = klass
    common.register('perl', klass, PerlSpinCtrlDoubleGenerator(klass))
