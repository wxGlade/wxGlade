"""\
Lisp generator functions for wxNotebook objects

@copyright: 2002-2004 D.H. aka crazyinsomniac on sourceforge.net
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen
from codegen import TabsCodeHandler


class LispNotebookGenerator(wcodegen.LispWidgetCodeWriter):

    def get_code(self, window):
        self._reset_vars()
        wcodegen.LispWidgetCodeWriter._prepare_tmpl_content(self, window)

        prop = window.properties
        id_name, id = self.codegen.generate_code_id(window)

        layout_props = []
        tabs = prop.get('tabs', [])
        for label, tab_win in tabs:
            tab_win = tab_win.replace('_', '-')
            layout_props.append(
                '(wxNotebook_AddPage (slot-%s obj) (slot-%s obj) %s '
                '1 -1)\n' % (
                    window.name,
                    tab_win,
                    self.codegen.quote_str(label)
                    )
                )

        parent = self.format_widget_access(window.parent)

        if window.is_toplevel:
            l = []
            if id_name:
                l.append(id_name)
            l.append(
                '(setf (slot-%s obj) (wxNotebook_Create %s %s -1 -1 -1 -1 '
                'wxNB_TOP))\n' % (
                    window.name,
                    parent,
                    id
                    )
                )
            return l, [], []

        init = []
        if id_name:
            init.append(id_name)
        init.append(
            '(setf (slot-%s obj) (wxNotebook_Create %s %s '
            '-1 -1 -1 -1 %s))\n' % (
                window.name,
                parent,
                id,
                self.tmpl_dict['style']
                )
            )

        props_buf = self.codegen.generate_common_properties(window)
        return init, props_buf, layout_props

    def get_properties_code(self, obj):
        prop = obj.properties
        props_buf = []
        tabs = prop.get('tabs', [])
        for label, window in tabs:
            props_buf.append(
                '(wxNotebook_AddPage (slot-%s obj) page %s 1 -1);\n' % (
                    window,
                    self.codegen.quote_str(label),
                    )
                )
        props_buf.extend(self.codegen.generate_common_properties(obj))
        return props_buf

# end of class LispNotebookGenerator


def initialize():
    klass = 'wxNotebook'
    common.class_names['EditNotebook'] = klass
    common.class_names['NotebookPane'] = 'wxPanel'
    common.toplevels['EditNotebook'] = 1
    common.toplevels['NotebookPane'] = 1

    common.register('lisp', klass, LispNotebookGenerator(klass),
                    'tabs', TabsCodeHandler, klass)
