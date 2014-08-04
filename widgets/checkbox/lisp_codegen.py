"""\
Lisp generator functions for wxCheckBox objects

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


import common
import wcodegen


class LispCheckBoxGenerator(wcodegen.LispWidgetCodeWriter):
    tmpl = '(setf %(name)s (%(klass)s_Create %(parent)s %(id)s ' \
           '%(label)s -1 -1 -1 -1 %(style)s))\n'

    def _prepare_tmpl_content(self, obj):
        wcodegen.LispWidgetCodeWriter._prepare_tmpl_content(self, obj)
        self.has_setvalue1 = obj.properties.get('checked', False)
        return

# end of class LispCheckBoxGenerator


def initialize():
    klass = 'wxCheckBox'
    common.class_names['EditCheckBox'] = klass
    common.register('lisp', klass, LispCheckBoxGenerator(klass))
