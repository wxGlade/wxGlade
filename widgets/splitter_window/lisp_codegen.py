"""\
Lisp generator functions for wxSplitterWindow objects

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2018 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class LispSplitterWindowGenerator(wcodegen.LispWidgetCodeWriter):

    def get_code(self, window):
        self._reset_vars()
        wcodegen.LispWidgetCodeWriter._prepare_tmpl_content(self, window)

        init = []
        layout_buf = []
        props_buf = self.codegen.generate_common_properties(window)

        id_name, id = self.codegen.generate_code_id(window)
        window_name = self.codegen._format_name(window.name)
        parent = self.format_widget_access(window.parent)

        if window.is_toplevel:
            l = []
            if id_name:
                l.append(id_name)

            l.append( '(setf (slot-%s obj) (wxSplitterWindow_Create %s %s))\n' % (window_name, parent, id) )
            return l, [], []

        if id_name:
            init.append(id_name)

        init.append('(setf (slot-%s obj) (wxSplitterWindow_Create %s %s -1 -1 -1 -1 %s))\n'
                    % (window_name, parent, id, self.tmpl_dict['style']))

        win_1 = window.window_1
        win_2 = window.window_2
        orientation = window.properties['orientation'].get_string_value()

        if win_1 and win_2:
            sash_pos = window.sash_pos
            if sash_pos!="": sash_pos = ', %s' % sash_pos

            if orientation == 'wxSPLIT_VERTICAL':
                f_name = 'SplitVertically'
            else:
                f_name = 'SplitHorizontally'

            layout_buf.append( '(%s %s %s %s %s)\n' % (f_name, window_name, win_1, win_2, sash_pos) )
        else:
            def add_sub(win):
                layout_buf.append( '(wxSplitterWindow_SetSplitMode (slot-%s obj) %s)\n' % (window_name, orientation) )
                layout_buf.append( '(wxSplitterWindow_Initialize (slot-%s obj) %s)\n' % (window_name, win) )
            if win_1:
                add_sub(win_1)
            elif win_2:
                add_sub(win_2)

        if window.min_pane_size:
            props_buf.append( 'wxSplitterWindow_SetMinimumPaneSize (slot-%s obj) %s)\n' % (window_name, window.min_pane_size) )
        if window.properties["sash_gravity"].is_active():
            props_buf.append( 'wxSplitterWindow_SetSashGravity (slot-%s obj) %s)\n' % (window_name, window.sash_gravity) )

        return init, props_buf, layout_buf

    def get_layout_code(self, obj):
        props_buf = []

        win_1 = window.window_1
        win_2 = window.window_2
        orientation = window.properties['orientation'].get_string_value()

        if win_1 and win_2:
            sash_pos = window.sash_pos
            if sash_pos!="": sash_pos = ', %s' % sash_pos

            if orientation == 'wxSPLIT_VERTICAL':
                f_name = 'SplitVertically'
            else:
                f_name = 'SplitHorizontally'

            props_buf.append('$self->%s($self->{%s}, $self->{%s}, %s);\n' %
                             (f_name, win_1, win_2, sash_pos))
        else:
            obj_name = self.codegen._format_name(obj.name)
            def add_sub(win):
                props_buf.append( '(wxSplitterWindow_SetSplitMode (slot-%s obj) %s)\n' % (obj_name,orientation) )
                props_buf.append( '(wxSplitterWindow_Initialize (slot-%s obj) %s)\n' % (obj_name,win) )
            if win_1:
                add_sub(win_1)
            elif win_2:
                add_sub(win_2)

        return props_buf


def initialize():
    klass = 'wxSplitterWindow'
    common.class_names['EditSplitterWindow'] = klass
    common.class_names['SplitterPane'] = 'wxPanel'
    common.toplevels['EditSplitterWindow'] = 1
    common.toplevels['SplitterPane'] = 1
    common.register('lisp', klass, LispSplitterWindowGenerator(klass))
