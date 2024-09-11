"""\
Code generator functions for wxFilePickerCtrl objects
"""

import common
import wcodegen


class PerlFilePickerCtrlGenerator(wcodegen.PerlWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s->new(%(parent)s, %(id)s, ' \
           '"", "Select file", "Text files (*.txt)|*.txt|All files|*.*", ' \
           'wxDefaultPosition, wxDefaultSize, %(style)s);\n'
    prefix_style = False
    set_default_style = True

    def _prepare_tmpl_content(self, obj):
        wcodegen.PerlWidgetCodeWriter._prepare_tmpl_content(self, obj)
        self.has_setdefault = int(obj.properties.get('default', 0))
        return


def initialize():
    klass = 'wxFilePickerCtrl'
    common.class_names['EditFilePickerCtrl'] = klass
    common.register('perl', klass, PerlFilePickerCtrlGenerator(klass))
