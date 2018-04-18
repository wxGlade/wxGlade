"""\
Perl generator functions for wxStaticText objects

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PerlStaticTextGenerator(wcodegen.PerlWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s->new(%(parent)s, %(id)s, %(label)s%(style)s);\n'
    def get_more_properties_code(self, obj):
        ret = []
        if obj.wrap>0:
            ret.append( '%s->Wrap(%d);\n'%(self.tmpl_dict['name'], obj.wrap) )
        return ret


def initialize():
    klass = 'wxStaticText'
    common.class_names['EditStaticText'] = klass
    common.register('perl', klass, PerlStaticTextGenerator(klass))
