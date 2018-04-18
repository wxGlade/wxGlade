"""\
Lisp generator functions for wxStaticText objects

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class LispStaticTextGenerator(wcodegen.LispWidgetCodeWriter):
    tmpl = '(setf %(name)s (%(klass)s_Create %(parent)s %(id)s %(label)s -1 -1 -1 -1 %(style)s))\n'
    def get_more_properties_code(self, obj):
        ret = []
        if obj.wrap>0:
            ret.append( '(%s_Wrap %s %d)\n'%(self.tmpl_dict['klass'], self.tmpl_dict['name'], obj.wrap) )
        return ret


def initialize():
    klass = 'wxStaticText'
    common.class_names['EditStaticText'] = klass
    common.register('lisp', klass, LispStaticTextGenerator(klass))
