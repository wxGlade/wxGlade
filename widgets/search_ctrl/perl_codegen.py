"""\
Perl generator functions for wxSearchCtrl objects

@copyright: 2018-2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PerlSearchCtrlGenerator(wcodegen.PerlWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s->new(%(parent)s, %(id)s, %(value)s%(style)s);\n'

    def get_more_properties_code(self, obj):
        ret = []
        name = self.tmpl_dict['name']
        if not obj.search_button:
            ret.append( '%s->ShowSearchButton(false);\n'%name )
        if obj.cancel_button:
            ret.append( '%s->ShowCancelButton(true);\n'%name )
        if obj.properties["descriptive_text"].is_active():
            text =  self.codegen.quote_str(obj.descriptive_text)
            ret.append( '%s->SetDescriptiveText(%s);\n'%(name, text) )
        if obj.properties["max_length"].is_active():
            ret.append( '%s->SetMaxLength(%d);\n'%(name, obj.max_length) )
        return ret


def initialize():
    klass = 'wxSearchCtrl'
    common.class_names['EditSearchCtrl'] = klass
    common.register('perl', klass, PerlSearchCtrlGenerator(klass))
