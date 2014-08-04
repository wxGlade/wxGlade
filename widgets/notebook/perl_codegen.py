"""\
Perl generator functions for wxNotebook objects

@copyright: 2002-2004 D.H. aka crazyinsomniac on sourceforge.net
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
from codegen import TabsCodeHandler


class PerlNotebookGenerator:
    new_signature = [
        '$parent', '$id', '$pos', '$size', '$style', '$name'
    ]

    def get_code(self, window):
        plgen = common.code_writers['perl']
        prop = window.properties
        id_name, id = plgen.generate_code_id(window)

        layout_props = []
        tabs = prop.get('tabs', [])
        for label, tab_win in tabs:
            layout_props.append('$self->{%s}->AddPage($self->{%s}, %s);\n' %
                                (window.name, tab_win, plgen.quote_str(label)))

        if not window.parent.is_toplevel:
            parent = '$self->{%s}' % window.parent.name
        else:
            parent = '$self'

        if window.is_toplevel:
            klass = window.base
            if klass != window.klass:
                klass = window.klass
            else:
                klass = plgen.cn(klass)

            l = []
            if id_name:
                l.append(id_name)
            l.append(
                '$self->{%s} = %s->new(%s, %s);\n' % (
                    window.name,
                    klass,
                    parent,
                    id,
                    )
                )
            return l, [], []
        style = prop.get("style")
        if style:
            style = "%s" % style
        else:
            style = ''
        init = []
        if id_name:
            init.append(id_name)
        init.append('$self->{%s} = %s->new(%s, %s, '
                    'wxDefaultPosition, wxDefaultSize, %s);\n' %
                    (window.name, plgen.cn(window.klass), parent, id, style))

        props_buf = plgen.generate_common_properties(window)
        return init, props_buf, layout_props

    def get_properties_code(self, obj):
        prop = obj.properties
        plgen = common.code_writers['perl']
        props_buf = []
        tabs = prop.get('tabs', [])
        for label, window in tabs:
            props_buf.append('$self->AddPage($self->{%s}, %s);\n' %
                             (window, plgen.quote_str(label)))
        props_buf.extend(plgen.generate_common_properties(obj))
        return props_buf

# end of class PerlNotebookGenerator


def initialize():
    klass = 'wxNotebook'
    common.class_names['EditNotebook'] = klass
    common.class_names['NotebookPane'] = 'wxPanel'
    common.toplevels['EditNotebook'] = 1
    common.toplevels['NotebookPane'] = 1
    common.register('perl', klass, PerlNotebookGenerator(),
                    'tabs', TabsCodeHandler, klass)
