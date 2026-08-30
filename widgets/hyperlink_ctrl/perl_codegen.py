"""
Perl code generator functions for wxHyperlinkCtrl objects

@copyright: 2012-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PerlHyperlinkCtrlGenerator(wcodegen.PerlWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s->new(%(parent)s, %(id)s, %(label)s, %(url)s%(style)s);\n'

    def _prepare_tmpl_content(self, obj):
        wcodegen.PerlWidgetCodeWriter._prepare_tmpl_content(self, obj)
        translate = self.codegen._use_gettext and not obj.check_prop_truth("no_gettext")
        self.tmpl_dict['url'] = self.codegen.quote_str(obj.url, translate)
        #self.has_setvalue1 = bool(obj.checked)
        return


def initialize():
    klass = 'wxHyperlinkCtrl'
    common.class_names['EditHyperlinkCtrl'] = klass
    common.register('perl', klass, PerlHyperlinkCtrlGenerator(klass))
