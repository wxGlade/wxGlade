"""
Lisp code generator functions for wxHyperlinkCtrl objects

@copyright: 2012-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class LispHyperlinkCtrlGenerator(wcodegen.LispWidgetCodeWriter):
    tmpl = '(setf %(name)s (%(klass)s_Create %(parent)s %(id)s %(label)s %(url)s -1 -1 -1 -1 %(style)s))\n'

    def _prepare_tmpl_content(self, obj):
        wcodegen.LispWidgetCodeWriter._prepare_tmpl_content(self, obj)
        self.tmpl_dict['url'] = self.codegen.quote_str(obj.url)
        #self.has_setvalue1 = bool(obj.checked)
        return


def initialize():
    klass = 'wxHyperlinkCtrl'
    common.class_names['EditHyperlinkCtrl'] = klass
    common.register('lisp', klass, LispHyperlinkCtrlGenerator(klass))
