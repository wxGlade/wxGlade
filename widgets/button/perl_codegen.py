"""\
Perl generator functions for wxButton objects

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


import common
import wcodegen


class PerlButtonGenerator(wcodegen.PerlWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s->new(%(parent)s, %(id)s, ' \
           '%(label)s%(style)s);\n'
    prefix_style = True

    def _prepare_tmpl_content(self, obj):
        wcodegen.PerlWidgetCodeWriter._prepare_tmpl_content(self, obj)

        prop = obj.properties
        stockitem = prop.get('stockitem', None)
        if stockitem:
            self.tmpl_dict['label'] = self.codegen.quote_str('')
            self.tmpl_dict['id_number'] = self.codegen.cn("wxID_" + stockitem)
            self.tmpl_dict['id'] = self.tmpl_dict['id_number']

        self.has_setdefault = prop.get('default', False)

        return

# end of class PerlButtonGenerator


def initialize():
    common.class_names['EditButton'] = 'wxButton'

    plgen = common.code_writers.get('perl')
    if plgen:
        plgen.add_widget_handler('wxButton', PerlButtonGenerator())
