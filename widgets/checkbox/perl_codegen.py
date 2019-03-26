"""\
Perl generator functions for wxCheckBox objects

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

from . import checkbox_base
import common
import wcodegen


class PerlCheckBoxGenerator(wcodegen.PerlWidgetCodeWriter, checkbox_base.CheckBoxMixin):
    tmpl = '%(name)s = %(klass)s->new(%(parent)s, %(id)s, %(label)s%(style)s);\n'
    tmpl_set3statevalue = '%(name)s->Set3StateValue(%(value_3state)s);\n'

    def _prepare_tmpl_content(self, obj):
        super(PerlCheckBoxGenerator, self)._prepare_tmpl_content(obj)
        self._prepare_checkbox_content(obj)

    def get_code(self, obj):
        init_lines, final_lines = super(PerlCheckBoxGenerator, self).get_code(obj)
        init_lines.extend( self._get_checkbox_code() )
        return init_lines, final_lines


def initialize():
    klass = 'wxCheckBox'
    common.class_names['EditCheckBox'] = klass
    common.register('perl', klass, PerlCheckBoxGenerator(klass))
