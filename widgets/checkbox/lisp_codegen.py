"""\
Lisp generator functions for wxCheckBox objects

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

from . import checkbox_base
import common
import wcodegen


class LispCheckBoxGenerator(wcodegen.LispWidgetCodeWriter,
                            checkbox_base.CheckBoxMixin):
    tmpl = '(setf %(name)s (%(klass)s_Create %(parent)s %(id)s %(label)s -1 -1 -1 -1 %(style)s))\n'
    tmpl_set3statevalue = '(%(klass)s_Set3StateValue %(name)s %(value_3state)s)\n'

    def _prepare_tmpl_content(self, obj):
        super(LispCheckBoxGenerator, self)._prepare_tmpl_content(obj)
        self._prepare_checkbox_content(obj)

    def get_code(self, obj):
        init_lines, final_lines =super(LispCheckBoxGenerator, self).get_code(obj)
        init_lines.extend( self._get_checkbox_code() )
        return init_lines, final_lines


def initialize():
    klass = 'wxCheckBox'
    common.class_names['EditCheckBox'] = klass
    common.register('lisp', klass, LispCheckBoxGenerator(klass))
