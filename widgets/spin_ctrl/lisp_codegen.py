"""\
Lisp generator functions for wxSpinCtrl objects

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class LispSpinCtrlGenerator(wcodegen.LispWidgetCodeWriter):
    tmpl = '(setf %(name)s (%(klass)s_Create %(parent)s %(id)s %(value)s ' \
           '-1 -1 -1 -1 %(style)s %(minValue)s %(maxValue)s %(value)s))\n'
    default_style = 'wxSP_ARROW_KEYS'
    set_default_style = True

    def _prepare_tmpl_content(self, obj):
        wcodegen.LispWidgetCodeWriter._prepare_tmpl_content(self, obj)
        prop = obj.properties
        self.tmpl_dict['value'] = prop.get('value', '')
        try:
            minValue, maxValue = [s.strip() for s in
                                  prop.get('range', '0, 100').split(',')]
        except:
            minValue, maxValue = '0', '100'
        self.tmpl_dict['minValue'] = minValue
        self.tmpl_dict['maxValue'] = maxValue
        return

# end of class LispSpinCtrlGenerator


def initialize():
    common.class_names['EditSpinCtrl'] = 'wxSpinCtrl'

    codegen = common.code_writers.get('lisp')
    if codegen:
        codegen.add_widget_handler('wxSpinCtrl', LispSpinCtrlGenerator())
