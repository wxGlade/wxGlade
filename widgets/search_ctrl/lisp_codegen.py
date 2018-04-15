"""\
Lisp generator functions for wxSearchCtrl objects

@copyright: 2018 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class LispSearchCtrlGenerator(wcodegen.LispWidgetCodeWriter):
    tmpl = '(setf %(name)s (%(klass)s_Create %(parent)s %(id)s %(value)s -1 -1 -1 -1 %(style)s))\n'

    def get_more_properties_code(self, obj):
        ret = []
        if obj.cancel_button:
            ret.append( '(%(klass)s_ShowCancelButton %(name)s t)\n'%self.tmpl_dict )
        return ret


def initialize():
    klass = 'wxSearchCtrl'
    common.class_names['EditSearchCtrl'] = klass
    common.register('lisp', klass, LispSearchCtrlGenerator(klass))
