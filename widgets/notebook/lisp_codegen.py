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

        id_name, id = self.codegen.generate_code_id(window)
        window_name = self.codegen._format_name(window.name)

        parent = self.format_widget_access(window.parent_window)

        if window.IS_CLASS:
            l = []
            if id_name:
                l.append(id_name)
            fmt = '(setf (slot-%s obj) (wxNotebook_Create %s %s -1 -1 -1 -1 wxNB_TOP))\n'
            l.append( fmt % (window_name, parent, id) )
            return l, []

        init = []
        if id_name:
            init.append(id_name)
        fmt = '(setf (slot-%s obj) (wxNotebook_Create %s %s -1 -1 -1 -1 %s))\n'
        init.append( fmt % (window_name, parent, id, self.tmpl_dict['style']) )

        init += self.codegen.generate_code_common_properties(window)
        return init, []

    def get_layout_code(self, obj):
        # called for a toplevel class
        return self.codegen.generate_code_common_properties(obj)

    def get_code_per_child(self, obj, child):
        i = obj.children.index(child)
        label = self.codegen.quote_str( obj.tabs[i][0] )

        if obj.IS_CLASS:
            return ['(wxNotebook_AddPage (slot-%s obj) page %s 1 -1);\n'% (child.name, label)]

        tab_win = child.name.replace('_', '-')
        notebook = self.codegen._format_name(obj.name)
        return ['(wxNotebook_AddPage (slot-%s obj) (slot-%s obj) %s 1 -1)\n' % (notebook, tab_win, label)]


def initialize():
    klass = 'wxNotebook'
    common.class_names['EditNotebook'] = klass
    common.class_names['NotebookPane'] = 'wxPanel'

    common.register('lisp', klass, LispNotebookGenerator(klass) )
