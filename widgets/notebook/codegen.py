"""\
Code generator functions for wxNotebook objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2019 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PythonNotebookGenerator(wcodegen.PythonWidgetCodeWriter):
    def get_code(self, window):
        self._reset_vars()
        wcodegen.PythonWidgetCodeWriter._prepare_tmpl_content(self, window)

        id_name, id = self.codegen.generate_code_id(window)
        parent = self.format_widget_access(window.parent_window)

        klass = window.get_instantiation_class(self.cn, self.cn_class, self.codegen.preview)

        if window.IS_CLASS:
            l = []
            if id_name: l.append(id_name)
            l.append('self.%s = %s(%s, %s)\n' % (window.name, klass, parent, id))
            return l, []

        init = []
        if id_name: init.append(id_name)
        init.append(('self.%s = ' + klass + '(%s, %s%s)\n')%(window.name, parent, id, self.tmpl_dict['style']))

        init += self.codegen.generate_code_common_properties(window)
        return init, []

    def get_layout_code(self, obj):
        # called for a toplevel class
        return self.codegen.generate_code_common_properties(obj)

    def get_code_per_child(self, obj, child):
        i = obj.children.index(child)
        label = obj.tabs[i][0]

        if child.IS_SLOT:
            tab_win = "wx.Panel(self.%s)"%obj.name
        else:
            tab_win = 'self.%s'%child.name
        notebook = self.format_widget_access(obj)  # 'self' or 'self.%s'%obj.name
        return ['%s.AddPage(%s, %s)\n'%(notebook, tab_win, self.codegen.quote_str(label))]


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
            if self.widget.children:
                label = self.widget.tabs[child.widget.index][0]
                tab_s = '    ' * ntabs
                output.append( tab_s + '<object class="notebookpage">\n' )
                output.append( tab_s + '<label>%s</label>\n' % escape(label) )

        def write_child_epilogue(self, child, output, ntabs):
            if self.widget.children:
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
        # this is not called for toplevel classes
        self._reset_vars()
        wcodegen.CppWidgetCodeWriter._prepare_tmpl_content(self, window)

        id_name, id = self.codegen.generate_code_id(window)
        if id_name:
            ids = [id_name]
        else:
            ids = []

        parent = self.format_widget_access(window.parent_window)
        klass = window.get_instantiation_class()

        if window.IS_CLASS:
            l = ['%s = new %s(%s, %s);\n' % (window.name, klass, parent, id)]
            return l, ids, []
        init = ['%s = new %s(%s, %s%s);\n' % (window.name, klass, parent, id, self.tmpl_dict['style'])]

        init += self.codegen.generate_code_common_properties(window)

        return init, ids, []

    def get_layout_code(self, obj):
        # called for a toplevel class
        return self.codegen.generate_code_common_properties(obj)

    def get_code_per_child(self, obj, child):
        i = obj.children.index(child)
        label = self.codegen.quote_str( obj.tabs[i][0] )
        if obj.IS_CLASS:
            return ['AddPage(%s, %s);\n' % (child.name, label)]
        return ['%s->AddPage(%s, %s);\n' % (obj.name, child.name, label)]


def initialize():
    klass = 'wxNotebook'
    common.class_names['EditNotebook'] = klass
    common.class_names['NotebookPane'] = 'wxPanel'
    common.register('python', klass, PythonNotebookGenerator(klass) )
    common.register('C++',    klass, CppNotebookGenerator(klass) )
    common.register('XRC',    klass, xrc_code_generator )
