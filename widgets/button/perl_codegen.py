# perl_codegen.py : perl generator functions for wxButton objects
# $Id: perl_codegen.py,v 1.5 2004/09/17 13:09:54 agriggio Exp $
#
# Copyright (c) 2002-2004 D.H. aka crazyinsomniac on sourceforge.net
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY


import common

class PerlCodeGenerator:
    def get_code(self, obj):
        """\
        fuction that generates perl code for wxButton objects.
        """
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
        if not style:
            extra = ''
        else:
            extra = ', wxDefaultPosition, wxDefaultSize, %s' % style

        if id_name: init.append(id_name)

        init.append('$self->{%s} = %s->new(%s, %s, %s%s);\n' %
                    (obj.name, obj.klass.replace('wx', 'Wx::', 1),
                     parent, id, label, extra))
        props_buf = plgen.generate_common_properties(obj)

        if prop.get('default', False):
            props_buf.append('$self->{%s}->SetDefault();\n' % obj.name)

        return init, props_buf, []

# end of class PerlCodeGenerator

def initialize():
    common.class_names['EditButton'] = 'wxButton'

    plgen = common.code_writers.get('perl')
    if plgen:
        plgen.add_widget_handler('wxButton', PerlCodeGenerator())
