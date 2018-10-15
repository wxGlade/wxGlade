"""\
Code generator functions for wxNotebook objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PythonNotebookGenerator(wcodegen.PythonWidgetCodeWriter):
    def get_code(self, window):
        self._reset_vars()
        wcodegen.PythonWidgetCodeWriter._prepare_tmpl_content(self, window)

        prop = window.properties
        id_name, id = self.codegen.generate_code_id(window)

        layout_props = []
        for label, tab_win in zip(window.tabs, window.pages):
            label = label[0]
            if tab_win.klass == "sizerslot":
                tab_win = "wx.Panel(self.%s)"%window.name
            else:
                tab_win = 'self.%s'%tab_win.name
            layout_props.append('self.%s.AddPage(%s, %s)\n'%(window.name, tab_win, self.codegen.quote_str(label)))
            
        parent = self.format_widget_access(window.parent)
        if window.is_toplevel:
            l = []
            if id_name:
                l.append(id_name)
            l.append('self.%s = %s(%s, %s)\n' % (window.name, self.codegen.get_class(window.klass), parent, id))
            return l, [], []
        klass = window.klass
        if self.codegen.preview:
            klass = 'wxNotebook'
        init = []
        if id_name:
            init.append(id_name)
        init.append(('self.%s = ' + self.cn(klass) + '(%s, %s%s)\n')%(window.name, parent, id, self.tmpl_dict['style']))

        props_buf = self.codegen.generate_common_properties(window)
        return init, props_buf, layout_props

    def get_properties_code(self, obj):
        prop = obj.properties
        props_buf = []
        for label, tab_win in zip(obj.tabs, obj.pages):
            label = label[0]
            props_buf.append( 'self.AddPage(self.%s, %s)\n' % (tab_win.name, self.codegen.quote_str(label)) )
        props_buf.extend(self.codegen.generate_common_properties(obj))
        return props_buf



def xrc_code_generator(obj):
    xrcgen = common.code_writers['XRC']
    from xml.sax.saxutils import escape

    class NotebookXrcObject(xrcgen.DefaultXrcObject):

        def write(self, output, ntabs, properties=None):
            if properties is None: properties = {}
            # the "tabs" property contains the pages of a notebook
            # be careful: tabs in context of code generation are white spaces used for indenting lines!!
            properties["tabs"] = None # don't write

            # always use a wxNotebookSizer
            properties['usenotebooksizer'] = '1'
            properties['no_custom_class'] = None
            xrcgen.DefaultXrcObject.write(self, output, ntabs, properties)

        def write_child_prologue(self, child, output, ntabs):
            if self.widget.pages:
                label = self.widget.tabs[child.widget.pos-1][0]  # pos is 1-based
                tab_s = '    ' * ntabs
                output.append( tab_s + '<object class="notebookpage">\n' )
                output.append( tab_s + '<label>%s</label>\n' % escape(label) )

        def write_child_epilogue(self, child, output, ntabs):
            if self.widget.pages:
                output.append( '    '*ntabs + '</object>\n' )

    return NotebookXrcObject(obj)


class CppNotebookGenerator(wcodegen.CppWidgetCodeWriter):
    constructor = [('wxWindow*', 'parent'), ('wxWindowID', 'id'),
                   ('const wxPoint&', 'pos', 'wxDefaultPosition'),
                   ('const wxSize&', 'size', 'wxDefaultSize'),
                   ('long', 'style', '0')]

    import_modules = ['<wx/notebook.h>']

    def get_code(self, window):
        "generates the C++ code for wxNotebook"
        self._reset_vars()
        wcodegen.CppWidgetCodeWriter._prepare_tmpl_content(self, window)

        prop = window.properties
        id_name, id = self.codegen.generate_code_id(window)
        if id_name:
            ids = [id_name]
        else:
            ids = []

        layout_props = []
        for label, tab_win in zip(window.tabs, window.pages):
            label = label[0]
            layout_props.append('%s->AddPage(%s, %s);\n' % (window.name, tab_win.name, self.codegen.quote_str(label)))

        if not window.parent.is_toplevel:
            parent = '%s' % window.parent.name
        else:
            parent = 'this'
        if window.is_toplevel:
            l = ['%s = new %s(%s, %s);\n' % (window.name, window.klass, parent, id)]
            return l, ids, [], []
        init = ['%s = new %s(%s, %s%s);\n' % (window.name, window.klass, parent, id, self.tmpl_dict['style'])]

        props_buf = self.codegen.generate_common_properties(window)

        return init, ids, props_buf, layout_props

    def get_properties_code(self, obj):
        prop = obj.properties
        props_buf = []
        for label, tab_win in zip(obj.tabs, obj.pages):
            label = label[0]
            props_buf.append( 'AddPage(%s, %s);\n' % (tab_win.name, self.codegen.quote_str(label)) )
        props_buf.extend(self.codegen.generate_common_properties(obj))
        return props_buf



def initialize():
    klass = 'wxNotebook'
    common.class_names['EditNotebook'] = klass
    common.class_names['NotebookPane'] = 'wxPanel'
    common.toplevels['EditNotebook'] = 1
    common.toplevels['NotebookPane'] = 1
    common.register('python', klass, PythonNotebookGenerator(klass) )
    common.register('C++',    klass, CppNotebookGenerator(klass) )
    common.register('XRC',    klass, xrc_code_generator )
