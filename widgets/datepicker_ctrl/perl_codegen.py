"""\
Code generator functions for wxDatePickerCtrl objects

@copyright: 2014 Carsten Grohmann
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
    common.class_names['EditDatePickerCtrl'] = 'wxDatePickerCtrl'
    plgen = common.code_writers.get('perl')
    if plgen:
        plgen.add_widget_handler('wxDatePickerCtrl',
                                 PerlDatePickerCtrlGenerator())
