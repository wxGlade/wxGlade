# perl_codegen.py : perl generator functions for wxSplitterWindow objects
# $Id: perl_codegen.py,v 1.3 2003/07/06 06:08:18 crazyinsomniac Exp $
#
# Copyright (c) 2002-2003 D.H. aka crazyinsomniac on sourceforge.net
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

import common

class PerlCodeGenerator:
    def get_code(self, window):
        plgen = common.code_writers['perl']
        prop = window.properties
        id_name, id = plgen.generate_code_id(window)

        if not window.parent.is_toplevel:
            parent = '$self->{%s}' % window.parent.name
        else:
            parent = '$self'

        if window.is_toplevel:
            l = []
            if id_name: l.append(id_name)
            l.append('$self->{%s} = %s->new(%s, %s);\n' %
                (window.name, window.klass.replace('wx','Wx::',1), parent,id))
            return l, [], []

        style = prop.get("style")
        if not( style and style != 'wxSP_3D' ): # default style
            style = ''

        init = []
        if id_name: init.append(id_name)

        init.append('$self->{%s} = Wx::SplitterWindow->new(%s, %s, %s);\n' %
                    (window.name, parent, id, style))

        props_buf = plgen.generate_common_properties(window)
        win_1 = prop.get('window_1')
        win_2 = prop.get('window_2')
        orientation = prop.get('orientation', 'wxSPLIT_VERTICAL')

        if win_1 and win_2:
            sash_pos = prop.get('sash_pos', '')

            if orientation == 'wxSPLIT_VERTICAL':
                f_name = 'SplitVertically'
            else:
                f_name = 'SplitHorizontally'

            props_buf.append('$self->{%s}->%s($self->{%s}, $self->{%s}, %s);\n'
                % (window.name, f_name, win_1, win_2, sash_pos))
        else:
            def add_sub(win):
                props_buf.append('$self->{%s}->SetSplitMode(%s);\n'
                    % (window.name, orientation))
                props_buf.append('$self->{%s}->Initialize($self->{%s});\n'
                    % (window.name, win))
            if win_1:
                add_sub(win_1)
            elif win_2:
                add_sub(win_2)

        return init, props_buf, []


    def get_properties_code(self, obj):
        plgen = common.code_writers['perl']
        props_buf = []
        prop = obj.properties
        orientation = prop.get('orientation', 'wxSPLIT_VERTICAL')

        win_1 = prop.get('window_1')
        win_2 = prop.get('window_2')

        if win_1 and win_2:
            sash_pos = prop.get('sash_pos', '')

            if orientation == 'wxSPLIT_VERTICAL':
                f_name = 'SplitVertically'
            else:
                f_name = 'SplitHorizontally'

            props_buf.append('$self->%s($self->{%s}, $self->{%s}, %s);\n' %
                             (f_name, win_1, win_2, sash_pos))
        else:
            def add_sub(win):
                props_buf.append('$self->SetSplitMode(%s);\n' % orientation)
                props_buf.append('$self->Initialize($self->{%s});\n' % win)

            if win_1:
                add_sub(win_1)
            elif win_2:
                add_sub(win_2)

        props_buf.extend(plgen.generate_common_properties(obj))

        return props_buf    

# end of class PerlCodeGenerator


def initialize():
    common.class_names['EditSplitterWindow'] = 'wxSplitterWindow'
    common.class_names['SplitterPane'] = 'wxPanel'
    common.toplevels['EditSplitterWindow'] = 1
    common.toplevels['SplitterPane'] = 1

    plgen = common.code_writers.get('perl')
    if plgen:
        plgen.add_widget_handler('wxSplitterWindow', PerlCodeGenerator())
