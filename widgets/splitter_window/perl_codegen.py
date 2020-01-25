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

    def get_code(self, obj):
        self._reset_vars()
        wcodegen.PerlWidgetCodeWriter._prepare_tmpl_content(self, obj)

        init = []
        layout_buf = []
        props_buf = self.codegen.generate_common_properties(obj)

        id_name, id = self.codegen.generate_code_id(obj)
        parent = self.format_widget_access(obj.parent)

        if obj.is_toplevel:
            l = []
            if id_name:
                l.append(id_name)

            klass = obj.base
            if klass != obj.klass:
                klass = obj.klass
            else:
                klass = self.cn(klass)

            l.append( '$self->{%s} = %s->new(%s, %s);\n' % (obj.name, self.cn(klass), parent, id) )
            return l, [], []

        if id_name:
            init.append(id_name)

        init.append( '$self->{%s} = %s->new(%s, %s%s);\n' % (
                     obj.name, self.cn(obj.klass), parent, id, self.tmpl_dict['style']) )

        win_1 = obj.window_1
        win_2 = obj.window_2
        orientation = obj.properties['orientation'].get_string_value()
        if win_1 and win_2:
            sash_pos = obj.sash_pos
            if sash_pos!="": sash_pos = ', %s' % sash_pos

            if orientation == 'wxSPLIT_VERTICAL':
                f_name = 'SplitVertically'
            else:
                f_name = 'SplitHorizontally'

            layout_buf.append( '$self->{%s}->%s($self->{%s}, $self->{%s}, %s);\n' % (
                                obj.name, f_name, win_1, win_2, sash_pos) )
        else:
            def add_sub(win):
                layout_buf.append( '$self->{%s}->SetSplitMode(%s);\n' % (obj.name, orientation) )
                layout_buf.append( '$self->{%s}->Initialize($self->{%s});\n' % (obj.name, win) )
            if win_1:
                add_sub(win_1)
            elif win_2:
                add_sub(win_2)

        if obj.min_pane_size:
            props_buf.append( '$self->{%s}->SetMinimumPaneSize(%s);\n' % (obj.name, obj.min_pane_size) )
        if obj.properties["sash_gravity"].is_active():
            props_buf.append( '$self->{%s}->SetSashGravity(%s);\n' % (obj.name, obj.sash_gravity) )

        return init, props_buf, layout_buf

    def get_layout_code(self, obj):
        props_buf = []
        win_1 = obj.window_1
        win_2 = obj.window_2
        orientation = obj.properties['orientation'].get_string_value()

        if win_1 and win_2:
            sash_pos = obj.sash_pos
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
