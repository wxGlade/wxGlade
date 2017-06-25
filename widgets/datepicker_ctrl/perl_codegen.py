"""\
Code generator functions for wxDatePickerCtrl objects

@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PerlDatePickerCtrlGenerator(wcodegen.PerlWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s->new(%(parent)s, %(id)s, ' \
           'Wx::DateTime->new(), wxDefaultPosition, wxDefaultSize, %(style)s);\n'
    prefix_style = False
    set_default_style = True

    def _prepare_tmpl_content(self, obj):
        wcodegen.PerlWidgetCodeWriter._prepare_tmpl_content(self, obj)
        self.has_setdefault = int(obj.properties.get('default', 0))
        return


def initialize():
    klass = 'wxDatePickerCtrl'
    common.class_names['EditDatePickerCtrl'] = klass
    common.register('perl', klass, PerlDatePickerCtrlGenerator(klass))
