"""\
Perl generator functions for wxNotebook objects

@copyright: 2002-2004 D.H. aka crazyinsomniac on sourceforge.net
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2017 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PerlNotebookGenerator(wcodegen.PerlWidgetCodeWriter):
    new_signature = ['$parent', '$id', '$pos', '$size', '$style', '$name']

    def get_code(self, window):
        self._reset_vars()
        wcodegen.PerlWidgetCodeWriter._prepare_tmpl_content(self, window)

        prop = window.properties
        id_name, id = self.codegen.generate_code_id(window)

        layout_props = []
        for label, tab_win in zip(window.tabs, window.pages):
            layout_props.append('$self->{%s}->AddPage($self->{%s}, %s);\n' %
                                (window.name, tab_win.name, self.codegen.quote_str(label[0])) )

        parent = self.format_widget_access(window.parent)

        if window.is_toplevel:
            klass = window.base
            if klass != window.klass:
                klass = window.klass
            else:
                klass = self.cn(klass)

            l = []
            if id_name:
                l.append(id_name)
            l.append( '$self->{%s} = %s->new(%s, %s);\n' % ( window.name, klass, parent, id) )
            return l, [], []
        init = []
        if id_name:
            init.append(id_name)
        init.append( '$self->{%s} = %s->new(%s, %s%s);\n' % (
                     window.name, self.cn(window.klass), parent, id, self.tmpl_dict['style']) )

        props_buf = self.codegen.generate_common_properties(window)
        return init, props_buf, layout_props

    def get_properties_code(self, obj):
        prop = obj.properties
        props_buf = []
        for label, tab_win in zip(obj.tabs, obj.pages):
            label = label[0]
            props_buf.append( '$self->AddPage($self->{%s}, %s);\n' % (tab_win.name, self.codegen.quote_str(label)) )
        props_buf.extend(self.codegen.generate_common_properties(obj))
        return props_buf


def initialize():
    klass = 'wxNotebook'
    common.class_names['EditNotebook'] = klass
    common.class_names['NotebookPane'] = 'wxPanel'
    common.toplevels['EditNotebook'] = 1
    common.toplevels['NotebookPane'] = 1
    common.register( 'perl', klass, PerlNotebookGenerator(klass) )
