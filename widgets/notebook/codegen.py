# codegen.py: code generator functions for wxNotebook objects
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: GPL (see license.txt)

import common

class TabsCodeHandler:
    def __init__(self):
        self.tabs = []
        self.curr_tab_name = []
        self.tab_window = None

    def start_elem(self, name, attrs):
        if name == 'tab':
            window = attrs.get('window')
            if not window: return
            self.tab_window = window
            self.curr_tab_name = []

    def end_elem(self, name, code_obj):
        if name == 'tabs':
            # set a temporary attribute of the notebook, that will be used by
            # the last NotebookPane added: this is an ugly hack anyway...
            code_obj.properties['tabs'] = self.tabs
            return True
        elif name == 'tab':
            tab_name = "".join(self.curr_tab_name)
            if self.tab_window: self.tabs.append((tab_name, self.tab_window))
        return False

    def char_data(self, data):
        self.curr_tab_name.append(data)

# end of class TabsCodeHandler


def python_code_generator(window):
    """\
    generates the python code for wxNotebook
    """
    pygen = common.code_writers['python']
    prop = window.properties
    id_name, id = pygen.generate_code_id(window)

    layout_props = ['%s_sizer = wxNotebookSizer(self.%s)\n' % \
                    (window.name, window.name)]
    tabs = prop.get('tabs', [])
    for label, tab_win in tabs:
        layout_props.append('self.%s.AddPage(self.%s, "%s")\n' % \
                            (window.name, tab_win, label.replace('"', r'\"')))
        
    if not window.parent.is_toplevel: parent = 'self.%s' % window.parent.name
    else: parent = 'self'
    if window.is_toplevel:
        l = ['self.%s = %s(%s, %s)\n' % (window.name, window.klass, parent,id)]
        if id_name: l.append(id_name)
        return l, [], [] #layout_props #[]
    style = prop.get("style")
    if style: style = ", style=%s" % style
    else: style = ''
    init = ['self.%s = wxNotebook(%s, %s%s)\n' %
            (window.name, parent, id, style) ]
    if id_name: init.append(id_name)

    props_buf = pygen.generate_common_properties(window)
    return init, props_buf, layout_props 


def python_generate_properties(obj):
    prop = obj.properties
    pygen = common.code_writers['python']
    props_buf = ['nb_sizer = wxNotebookSizer(self)\n']
    tabs = prop.get('tabs', [])
    for label, window in tabs:
        props_buf.append('self.AddPage(self.%s, "%s")\n' % \
                         (window, label.replace('"', r'\"')))
    props_buf.extend(pygen.generate_common_properties(obj))
    return props_buf    


def xrc_code_generator(obj):
    xrcgen = common.code_writers['XRC']
    class NotebookXrcObject(xrcgen.DefaultXrcObject):
        def write(self, outfile, ntabs):
            if self.properties.has_key('tabs'):
                self.tabs = self.properties['tabs']
                del self.properties['tabs']
            else:
                self.tabs = []
            self.index = 0
            xrcgen.DefaultXrcObject.write(self, outfile, ntabs)

        def write_child_prologue(self, child, outfile, ntabs):
            from xml.sax.saxutils import escape
            if self.tabs:
                tab_s = '    ' * ntabs
                outfile.write(tab_s + '<object class="notebookpage">\n')
                outfile.write(tab_s + '<label>%s</label>\n' % \
                              escape(self.tabs[self.index][0]))
                self.index += 1

        def write_child_epilogue(self, child, outfile, ntabs):
            if self.tabs:
                outfile.write('    '*ntabs + '</object>\n')
                
    return NotebookXrcObject(obj)



def initialize():
    common.class_names['EditNotebook'] = 'wxNotebook'
    common.class_names['NotebookPane'] = 'wxPanel'
    # python code generation functions
    pygen = common.code_writers.get('python')
    if pygen:
        pygen.add_widget_handler('wxNotebook', python_code_generator,
                                 python_generate_properties)
        pygen.add_property_handler('tabs', TabsCodeHandler, 'wxNotebook')
    xrcgen = common.code_writers.get('XRC')
    if xrcgen:
        xrcgen.add_widget_handler('wxNotebook', xrc_code_generator)
        xrcgen.add_property_handler('tabs', TabsCodeHandler, 'wxNotebook')

