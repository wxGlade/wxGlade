# perl_codegen.py : perl generator functions for wxStaticText objects
# $Id: perl_codegen.py,v 1.3 2004/09/17 13:09:49 agriggio Exp $
#
# Copyright (c) 2002-2004 D. H. aka crazyinsomniac on sourceforge
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

import common

class PerlCodeGenerator:
    def get_code(self, obj):
        init = []
        plgen = common.code_writers['perl']
        prop = obj.properties

        attribute = plgen.test_attribute(obj)

        id_name, id = plgen.generate_code_id(obj) 
        label = plgen.quote_str(prop.get('label', ''))
        if not obj.parent.is_toplevel:
            parent = '$self->{%s}' % obj.parent.name
        else:
            parent = '$self'

        style = prop.get("style")
        if not style: style = ''

        if id_name: init.append(id_name)
        if attribute:
            prefix = '$self->{%s}' % obj.name
        else:
            prefix = 'my $%s' % obj.name
            obj.name = '$' + obj.name

        init.append('%s = %s->new(%s, %s, %s, wxDefaultPosition, \
wxDefaultSize, %s);\n' % (prefix, obj.klass.replace('wx','Wx::',1),
            parent, id, label, style))

        props_buf = plgen.generate_common_properties(obj)
        if not attribute:
            # the object doesn't have to be stored as an attribute of the
            # custom class, but it is just considered part of the layout
            return [], [], init + props_buf
        return init, props_buf, []

# end of class PerlCodeGenerator

def initialize():

    common.class_names['EditStaticText'] = 'wxStaticText'
    plgen = common.code_writers.get('perl')

    if plgen:
        plgen.add_widget_handler('wxStaticText', PerlCodeGenerator())
