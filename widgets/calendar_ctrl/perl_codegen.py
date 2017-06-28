"""
Perl generator functions for wxCalendarCtrl objects

@copyright: 2012 Eric McKeeth
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PerlCalendarCtrlGenerator(wcodegen.PerlWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s->new(%(parent)s, %(id)s, Wx::DateTime->new%(style)s);\n'
    prefix_style = True

    def _prepare_tmpl_content(self, obj):
        wcodegen.PerlWidgetCodeWriter._prepare_tmpl_content(self, obj)
        self.has_setdefault = obj.default
        return


def initialize():
    klass = 'wxCalendarCtrl'
    common.class_names['EditCalendarCtrl'] = klass
    common.register('perl', klass, PerlCalendarCtrlGenerator(klass))
