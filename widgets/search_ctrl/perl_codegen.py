"""\
Perl generator functions for wxSearchCtrl objects

@copyright: 2018 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PerlSearchCtrlGenerator(wcodegen.PerlWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s->new(%(parent)s, %(id)s, %(value)s%(style)s);\n'

    def get_more_properties_code(self, obj):
        ret = []
        if obj.cancel_button:
            ret.append( '%(name)s.ShowCancelButton(true)\n'%self.tmpl_dict )
        return ret


def initialize():
    klass = 'wxSearchCtrl'
    common.class_names['EditSearchCtrl'] = klass
    common.register('perl', klass, PerlSearchCtrlGenerator(klass))
