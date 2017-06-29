"""\
Lisp generator functions for wxNotebook objects

@copyright: 2002-2004 D.H. aka crazyinsomniac on sourceforge.net
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2017 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class LispNotebookGenerator(wcodegen.LispWidgetCodeWriter):

    def get_code(self, window):
        self._reset_vars()
        wcodegen.LispWidgetCodeWriter._prepare_tmpl_content(self, window)

        prop = window.properties
        id_name, id = self.codegen.generate_code_id(window)
        window_name = self.codegen._format_name(window.name)

        layout_props = []
        for label, tab_win in zip(window.tabs, window.pages):
            tab_win = tab_win.name.replace('_', '-')
            fmt = '(wxNotebook_AddPage (slot-%s obj) (slot-%s obj) %s 1 -1)\n'
            layout_props.append( fmt % (window_name, tab_win, self.codegen.quote_str(label[0]) ) )

        parent = self.format_widget_access(window.parent)

        if window.is_toplevel:
            l = []
            if id_name:
                l.append(id_name)
            fmt = '(setf (slot-%s obj) (wxNotebook_Create %s %s -1 -1 -1 -1 wxNB_TOP))\n'
            l.append( fmt % (window_name, parent, id) )
            return l, [], []

        init = []
        if id_name:
            init.append(id_name)
        fmt = '(setf (slot-%s obj) (wxNotebook_Create %s %s -1 -1 -1 -1 %s))\n'
        init.append( fmt % (window_name, parent, id, self.tmpl_dict['style']) )

        props_buf = self.codegen.generate_common_properties(window)
        return init, props_buf, layout_props

    def get_properties_code(self, obj):
        props_buf = []
        for label, tab_win in zip(obj.tabs, obj.pages):
            fmt = '(wxNotebook_AddPage (slot-%s obj) page %s 1 -1);\n'
            props_buf.append( fmt % (tab_win.name, self.codegen.quote_str(label[0]) ) )
        props_buf.extend(self.codegen.generate_common_properties(obj))
        return props_buf


def initialize():
    klass = 'wxNotebook'
    common.class_names['EditNotebook'] = klass
    common.class_names['NotebookPane'] = 'wxPanel'
    common.toplevels['EditNotebook'] = 1
    common.toplevels['NotebookPane'] = 1

    common.register('lisp', klass, LispNotebookGenerator(klass) )
