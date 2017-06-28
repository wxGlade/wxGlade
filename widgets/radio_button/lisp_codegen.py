"""\
Lisp generator functions for wxRadioButton objects

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


import common
import wcodegen


class LispRadioButtonGenerator(wcodegen.LispWidgetCodeWriter):
    tmpl = '(setf %(name)s (%(klass)s_Create %(parent)s %(id)s %(label)s -1 -1 -1 -1 %(style)s))\n'

    def _prepare_tmpl_content(self, obj):
        wcodegen.LispWidgetCodeWriter._prepare_tmpl_content(self, obj)
        self.has_setvalue1 = bool(obj.clicked)
        return


def initialize():
    klass = 'wxRadioButton'
    common.class_names['EditRadioButton'] = klass
    common.register('lisp', klass, LispRadioButtonGenerator(klass))
