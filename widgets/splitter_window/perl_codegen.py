"""\
Perl generator functions for wxSplitterWindow objects

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2018 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PerlSplitterWindowGenerator(wcodegen.PerlWidgetCodeWriter):
    new_signature = ['$parent', '$id', '$pos', '$size', '$style', '$name']

    def get_code(self, window):
        self._reset_vars()
        wcodegen.PerlWidgetCodeWriter._prepare_tmpl_content(self, window)

        init = []
        layout_buf = []
        props_buf = self.codegen.generate_common_properties(window)

        id_name, id = self.codegen.generate_code_id(window)
        parent = self.format_widget_access(window.parent)

        if window.is_toplevel:
            l = []
            if id_name:
                l.append(id_name)

            klass = window.base
            if klass != window.klass:
                klass = window.klass
            else:
                klass = self.cn(klass)

            l.append( '$self->{%s} = %s->new(%s, %s);\n' % (window.name, self.cn(klass), parent, id) )
            return l, [], []

        if id_name:
            init.append(id_name)

        init.append( '$self->{%s} = %s->new(%s, %s%s);\n' % (
                     window.name, self.cn(window.klass), parent, id, self.tmpl_dict['style']) )

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

            layout_buf.append( '$self->{%s}->%s($self->{%s}, $self->{%s}, %s);\n' % (
                                window.name, f_name, win_1, win_2, sash_pos) )
        else:
            def add_sub(win):
                layout_buf.append( '$self->{%s}->SetSplitMode(%s);\n' % (window.name, orientation) )
                layout_buf.append( '$self->{%s}->Initialize($self->{%s});\n' % (window.name, win) )
            if win_1:
                add_sub(win_1)
            elif win_2:
                add_sub(win_2)

        if window.min_pane_size:
            props_buf.append( '$self->{%s}->SetMinimumPaneSize(%s);\n' % (window.name, window.min_pane_size) )
        if window.properties["sash_gravity"].is_active():
            props_buf.append( '$self->{%s}->SetSashGravity(%s);\n' % (window.name, window.sash_gravity) )

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
            props_buf.append( '$self->%s($self->{%s}, $self->{%s}, %s);\n' % (f_name, win_1, win_2, sash_pos) )
        else:
            def add_sub(win):
                props_buf.append( '$self->SetSplitMode(%s);\n' % orientation )
                props_buf.append( '$self->Initialize($self->{%s});\n' % win )

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
    common.register('perl', klass, PerlSplitterWindowGenerator(klass))
