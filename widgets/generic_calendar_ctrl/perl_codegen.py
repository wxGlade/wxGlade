"""
Perl generator functions for wxGenericCalendarCtrl objects

@copyright: 2012 Eric McKeeth
@copyright: 2014 Carsten Grohmann
@copyright: 2015 Franco Bugnano
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PerlGenericCalendarCtrlGenerator(wcodegen.PerlWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s->new(%(parent)s, %(id)s, Wx::DateTime->new%(style)s);\n'
    prefix_style = True

    def _prepare_tmpl_content(self, obj):
        wcodegen.PerlWidgetCodeWriter._prepare_tmpl_content(self, obj)
        self.has_setdefault = obj.default
        return


def initialize():
    klass = 'wxGenericCalendarCtrl'
    common.class_names['EditGenericCalendarCtrl'] = klass
    common.register('perl', klass, PerlGenericCalendarCtrlGenerator(klass))
