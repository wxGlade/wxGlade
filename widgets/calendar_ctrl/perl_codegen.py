"""
Perl generator functions for wxCalendarCtrl objects

@copyright: 2012 Eric McKeeth
@copyright: 2014 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PerlCalendarCtrlGenerator(wcodegen.PerlWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s->new(%(parent)s, %(id)s, ' \
           'Wx::DateTime->new%(style)s);\n'
    prefix_style = True

    def _prepare_tmpl_content(self, obj):
        wcodegen.PerlWidgetCodeWriter._prepare_tmpl_content(self, obj)
        self.has_setdefault = obj.properties.get('default', False)
        return

#end of code generator


def initialize():
    common.class_names['EditCalendarCtrl'] = 'wxCalendarCtrl'

    plgen = common.code_writers.get('perl')
    if plgen:
        plgen.add_widget_handler('wxCalendarCtrl', PerlCalendarCtrlGenerator())
