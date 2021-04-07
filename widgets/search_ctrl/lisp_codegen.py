"""\
Lisp generator functions for wxSearchCtrl objects

@copyright: 2018-2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class LispSearchCtrlGenerator(wcodegen.LispWidgetCodeWriter):
    tmpl = '(setf %(name)s (%(klass)s_Create %(parent)s %(id)s %(value)s -1 -1 -1 -1 %(style)s))\n'

    def get_more_properties_code(self, obj):
        ret = []
        klass = self.tmpl_dict['klass']
        name = self.tmpl_dict['name']
        if not obj.search_button:
            ret.append( '(%s_ShowSearchButton %s nil)\n'%(klass, name) )
        if obj.cancel_button:
            ret.append( '(%s_ShowCancelButton %s t)\n'%(klass, name) )
        if obj.descriptive_text:
            text = self.codegen.quote_str(obj.descriptive_text)
            ret.append( '(%s_SetDescriptiveText %s %s)\n'%(klass, name, text) )
        if obj.properties["max_length"].is_active():
            ret.append( '(%s_SetMaxLength %s %d)\n'%(klass, name, obj.max_length) )
        return ret


def initialize():
    klass = 'wxSearchCtrl'
    common.class_names['EditSearchCtrl'] = klass
    common.register('lisp', klass, LispSearchCtrlGenerator(klass))
