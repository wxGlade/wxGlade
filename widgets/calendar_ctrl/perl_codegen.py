"""
Perl generator functions for wxCalendarCtrl objects

@copyright: 2012 Eric McKeeth

@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common


class PerlCodeGenerator:
    def get_code(self, obj):
        plgen = common.code_writers['perl']
        prop = obj.properties
        id_name, id = plgen.generate_code_id(obj)

        if not obj.parent.is_toplevel:
            parent = '$self->{%s}' % obj.parent.name
        else:
            parent = '$self'

        style = prop.get("style")
        if not(style):
            style = ''

        init = []
        if id_name:
            init.append(id_name)

        klass = obj.base
        if klass != obj.klass:
            klass = obj.klass
        else:
            klass = klass.replace('wx', 'Wx::', 1)

        init.append('$self->{%s} = %s->new(%s, %s, Wx::DateTime->new, \
wxDefaultPosition, wxDefaultSize, %s);\n' % (obj.name, klass, parent, id, style))
        props_buf = plgen.generate_common_properties(obj)
        return init, props_buf, []

#end of code generator


def initialize():
    common.class_names['EditCalendarCtrl'] = 'wxCalendarCtrl'

    plgen = common.code_writers.get('perl')
    if plgen:
        plgen.add_widget_handler('wxCalendarCtrl', PerlCodeGenerator())
