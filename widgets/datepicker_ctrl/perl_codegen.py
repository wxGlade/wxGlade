"""\
Code generator functions for wxDatePickerCtrl objects

@copyright: 2014-2015 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PerlDatePickerCtrlGenerator(wcodegen.PerlWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s->new(%(parent)s, %(id)s%(style)s);\n'

    def _prepare_tmpl_content(self, obj):
        wcodegen.PerlWidgetCodeWriter._prepare_tmpl_content(self, obj)
        self.has_setdefault = obj.properties.get('default', False)
        return

# end of class PerlDatePickerCtrlGenerator


def initialize():
    klass = 'wxDatePickerCtrl'
    common.class_names['EditDatePickerCtrl'] = klass
    common.register('perl', klass, PerlDatePickerCtrlGenerator(klass))
