# perl_codegen.py : perl generator functions for wxRadioButton objects
# $Id: perl_codegen.py,v 1.3 2004/09/17 13:09:51 agriggio Exp $
#
# Copyright (c) 2002-2004 D.H. aka crazyinsomniac on sourceforge.net
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY


import common


class PerlCodeGenerator:
    def get_code(self, obj):
        init = []
        plgen = common.code_writers['perl']
        prop = obj.properties
        id_name, id = plgen.generate_code_id(obj)
        label = plgen.quote_str(prop.get('label', ''))

        if not obj.parent.is_toplevel:
            parent = '$self->{%s}' % obj.parent.name
        else:
            parent = '$self'

        style = prop.get("style")
        if not style: style = ''

        if id_name: init.append(id_name)

        init.append('$self->{%s} = %s->new(%s, %s, %s, wxDefaultPosition, \
wxDefaultSize, %s);\n' % (obj.name, obj.klass.replace('wx','Wx::',1), parent,
            id, label, style))
        props_buf = plgen.generate_common_properties(obj)

        clicked = prop.get('clicked')
        if clicked: props_buf.append('$self->{%s}->SetValue(1);\n' % obj.name)

        return init, props_buf, []

# end of class PerlCodeGenerator


def initialize():
    common.class_names['EditRadioButton'] = 'wxRadioButton'

    plgen = common.code_writers.get('perl')
    if plgen:
        plgen.add_widget_handler('wxRadioButton', PerlCodeGenerator())

