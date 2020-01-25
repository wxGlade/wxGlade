"""\
Code generator functions for wxSplitterWindow objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2018 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PythonSplitterWindowGenerator(wcodegen.PythonWidgetCodeWriter):
    def get_code(self, obj):
        self._reset_vars()
        wcodegen.PythonWidgetCodeWriter._prepare_tmpl_content(self, obj)

        init = []
        layout_buf = []
        props_buf = self.codegen.generate_common_properties(obj)

        id_name, id = self.codegen.generate_code_id(obj)
        parent = self.format_widget_access(obj.parent)
        if obj.is_toplevel:
            l = []
            if id_name:
                l.append(id_name)
            l.append( 'self.%s = %s(%s, %s)\n' % (obj.name, self.codegen.get_class(obj.klass), parent, id) )
            return l, [], []
        if id_name:
            init.append(id_name)
        klass = obj.klass
        if self.codegen.preview: klass = 'wxSplitterWindow'
        init.append( 'self.%s = %s(%s, %s%s)\n' % (obj.name, self.cn(klass), parent, id, self.tmpl_dict['style']) )

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
            layout_buf.append( 'self.%s.%s(self.%s, self.%s%s)\n' % (obj.name, f_name, win_1, win_2, sash_pos) )
        else:
            def add_sub(win):
                layout_buf.append( 'self.%s.SetSplitMode(%s)\n' % (obj.name, self.cn(orientation)) )
                layout_buf.append( 'self.%s.Initialize(self.%s)\n' % (obj.name, win) )
            if win_1:
                add_sub(win_1)
            elif win_2:
                add_sub(win_2)

        if obj.min_pane_size:
            props_buf.append( 'self.%s.SetMinimumPaneSize(%s)\n' % (obj.name, obj.min_pane_size) )
        if obj.properties["sash_gravity"].is_active():
            props_buf.append( 'self.%s.SetSashGravity(%s)\n' % (obj.name, obj.sash_gravity) )

        return init, props_buf, layout_buf

    def get_layout_code(self, obj):
        win_1 = obj.window_1
        win_2 = obj.window_2
        orientation = obj.properties['orientation'].get_string_value()
        props_buf = []
        if win_1 and win_2:
            sash_pos = obj.sash_pos
            if sash_pos!="": sash_pos = ', %s' % sash_pos
            if orientation == 'wxSPLIT_VERTICAL':
                f_name = 'SplitVertically'
            else:
                f_name = 'SplitHorizontally'
            props_buf.append('self.%s(self.%s, self.%s%s)\n' % (f_name, win_1, win_2, sash_pos))
        else:
            def add_sub(win):
                props_buf.append('self.SetSplitMode(%s)\n' % self.cn(orientation))
                props_buf.append('self.Initialize(self.%s)\n' % win)
            if win_1:
                add_sub(win_1)
            elif win_2:
                add_sub(win_2)
        return props_buf



class CppSplitterWindowGenerator(wcodegen.CppWidgetCodeWriter):
    constructor = [('wxWindow*', 'parent'), ('wxWindowID', 'id'),
                   ('const wxPoint&', 'pos', 'wxDefaultPosition'),
                   ('const wxSize&', 'size', 'wxDefaultSize'),
                   ('long', 'style', 'wxSP_3D')]

    import_modules = ['<wx/splitter.h>']

    def get_code(self, obj):
        "generates the C++ code for wxSplitterWindow"
        self._reset_vars()
        wcodegen.CppWidgetCodeWriter._prepare_tmpl_content(self, obj)

        init = []
        layout_buf = []
        props_buf = self.codegen.generate_common_properties(obj)

        id_name, id = self.codegen.generate_code_id(obj)

        if id_name:
            ids = [id_name]
        else:
            ids = []
        if not obj.parent.is_toplevel:
            parent = '%s' % obj.parent.name
        else:
            parent = 'this'
        if obj.is_toplevel:
            l = ['%s = new %s(%s, %s);\n' % (obj.name, obj.klass, parent, id)]
            return l, ids, [], []

        init.append( '%s = new %s(%s, %s%s);\n' % (obj.name, obj.klass, parent, id, self.tmpl_dict['style']) )

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
            layout_buf.append('%s->%s(%s, %s%s);\n' % (obj.name, f_name, win_1, win_2, sash_pos) )
        else:
            def add_sub(win):
                layout_buf.append( '%s->SetSplitMode(%s);\n' % (obj.name, orientation) )
                layout_buf.append( '%s->Initialize(%s);\n' % (obj.name, win) )
            if win_1:
                add_sub(win_1)
            elif win_2:
                add_sub(win_2)

        if obj.min_pane_size:
            props_buf.append( '%s->SetMinimumPaneSize(%s);\n' % (obj.name, obj.min_pane_size) )
        if obj.properties["sash_gravity"].is_active():
            props_buf.append( '%s->SetSashGravity(%s);\n' % (obj.name, obj.sash_gravity) )

        return init, ids, props_buf, layout_buf

    def get_layout_code(self, obj):
        win_1 = obj.window_1
        win_2 = obj.window_2
        orientation = obj.properties['orientation'].get_string_value()
        props_buf = []
        if win_1 and win_2:
            sash_pos = obj.sash_pos
            if sash_pos!="": sash_pos = ', %s' % sash_pos
            if orientation == 'wxSPLIT_VERTICAL':
                f_name = 'SplitVertically'
            else:
                f_name = 'SplitHorizontally'
            props_buf.append( '%s(%s, %s%s);\n' % (f_name, win_1, win_2, sash_pos) )
        else:
            def add_sub(win):
                props_buf.append('SetSplitMode(%s);\n' % orientation)
                props_buf.append('Initialize(%s);\n' % win)
            if win_1:
                add_sub(win_1)
            elif win_2:
                add_sub(win_2)
        return props_buf



def xrc_code_generator(obj):
    xrcgen = common.code_writers['XRC']

    class XrcCodeGenerator(xrcgen.DefaultXrcObject):
        props_map  = { 'sash_pos': 'sashpos', 'window_1': '', 'window_2': '' }
        orient_map = { 'wxSPLIT_VERTICAL': 'vertical', 'wxSPLIT_HORIZONTAL': 'horizontal' }
        def write_property(self, name, val, outfile, ntabs):
            try:
                prop = self.props_map.get(name, name)
                if not prop:
                    return
                if prop == 'orientation':
                    val = self.orient_map[val]
                xrcgen.DefaultXrcObject.write_property( self, prop, val, outfile, ntabs )
            except KeyError:
                return
        def write(self, *args, **kwds):
            if 'no_custom_class' in self.properties:
                del self.properties['no_custom_class']
            xrcgen.DefaultXrcObject.write(self, *args, **kwds)

    return XrcCodeGenerator(obj)


def initialize():
    klass = 'wxSplitterWindow'
    common.class_names['EditSplitterWindow'] = klass
    common.class_names['SplitterPane'] = 'wxPanel'
    common.toplevels['EditSplitterWindow'] = 1
    common.toplevels['SplitterPane'] = 1
    common.register('python', klass, PythonSplitterWindowGenerator(klass))
    common.register('C++',    klass, CppSplitterWindowGenerator(klass))
    common.register('XRC',    klass, xrc_code_generator)
