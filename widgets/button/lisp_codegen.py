"""\
Lisp generator functions for wxButton objects

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


import common
import wcodegen


class LispButtonGenerator(wcodegen.LispWidgetCodeWriter):
    tmpl = '(setf %(name)s (%(klass)s_Create %(parent)s %(id)s %(label)s -1 -1 -1 -1 %(style)s))\n'

    def _prepare_tmpl_content(self, obj):
        wcodegen.LispWidgetCodeWriter._prepare_tmpl_content(self, obj)

        if obj.check_prop_truth("stockitem"):
            self.tmpl_dict['label'] = self.codegen.quote_str('')
            self.tmpl_dict['id_number'] = self.codegen.cn("wxID_" + obj.stockitem)
            self.tmpl_dict['id'] = self.tmpl_dict['id_number']

        self.has_setdefault = obj.default

        return


def initialize():
    klass = 'wxButton'
    common.class_names['EditButton'] = klass
    common.register('lisp', klass, LispButtonGenerator(klass))
