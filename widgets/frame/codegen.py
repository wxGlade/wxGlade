# codegen.py: code generator functions for wxDialog objects
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: GPL (see license.txt)

import common
from MenuTree import *

# python code generators

def python_menubar_code_generator(obj):
    """\
    function that generates code for the menubar of a wxFrame.
    """
    menus = obj.properties['menubar']
    init = [ '\n', '# Menu Bar\n', 'self.%s = wxMenuBar()\n' % obj.name,
             'self.SetMenuBar(self.%s)\n' % obj.name ]
    append = init.append
    def append_items(menu, items):
        for item in items:
            if item.name == '---': # item is a separator
                append('%s.AppendSeparator()\n' % menu)
            elif item.children:
                if item.name: name = item.name
                else: name = '%s_sub' % menu
                append('%s = wxMenu()\n' % name)
                if item.id: # generating id
                    tokens = item.id.split('=')
                    if len(tokens) > 1:
                        append('%s = %s\n' % tuple(tokens))
                    else:
                        try: int(item.id)
                        except: append('%s = wxNewId()\n' % item.id)
                append_items(name, item.children)
                append('%s.AppendMenu(%s, %s, %s)\n' %
                       (menu, item.id or 'wxNewId()', '"%s"' %
                        item.label.replace('"', '\"'), name))
            else:
                if item.id:
                    tokens = item.id.split('=')
                    if len(tokens) > 1:
                        append('%s = %s\n' % tuple(tokens))
                    else:
                        try: int(item.id)
                        except: append('%s = wxNewId()\n' % item.id)
                if item.checkable == '1':
                    append('%s.Append(%s, "%s", "", 1)\n' %
                           (menu, item.id or 'wxNewId()',
                            item.label.replace('"', '\"')))
                else:
                    append('%s.Append(%s, "%s")\n' %
                           (menu, item.id or 'wxNewId()',
                            item.label.replace('"', '\"')))
    #print 'menus = %s' % menus
    for m in menus:
        menu = m.root
        if menu.name: name = 'self.' + menu.name
        else: name = 'wxglade_tmp_menu'
        append('%s = wxMenu()\n' % name)
        if menu.children:
            append_items(name, menu.children)
        append('self.%s.Append(%s, "%s")\n' %
               (obj.name, name, menu.label.replace('"', '\"')))
    append('# Menu Bar end\n')
    init.reverse()
    return init, [], []

    
def python_statusbar_code_generator(obj):
    """\
    function that generates code for the statusbar of a wxFrame.
    """
    labels, widths = obj.properties['statusbar']
    init = [ 'self.%s = self.CreateStatusBar(%s)\n' % \
             (obj.name, len(labels)) ]
    props = []
    append = props.append
    append('self.%s.SetStatusWidths(%s)\n' % (obj.name, repr(widths)))
    append('# statusbar fields\n')
    append('%s_fields = %s\n' % (obj.name, repr(labels)))
    append('for i in range(len(%s_fields)):\n' % obj.name)
    append('    self.%s.SetStatusText(%s_fields[i], i)\n' % \
           (obj.name, obj.name))
    return init, props, []
    

def python_generate_frame_properties(frame):
    """\
    generates the code for the various wxFrame specific properties.
    Returns a list of strings containing the generated code
    """
    prop = frame.properties
    pygen = common.code_writers['python']
    out = []
    title = prop.get('title')
    if title: out.append('self.SetTitle("%s")\n' % title)
    out.extend(pygen.generate_common_properties(frame))
    return out


# property handlers for code generation

class StatusFieldsHandler:
    """Handler for statusbar fields"""
    def __init__(self):
        self.labels = []
        self.widths = []
        self.curr_label = []
        
    def start_elem(self, name, attrs):
        if name == 'field':
            self.widths.append(int(attrs.get('width', -1)))
            
    def end_elem(self, name, code_obj):
        if name == 'fields':
            code_obj.properties['statusbar'] = (self.labels, self.widths)
            return True
        self.labels.append("".join(self.curr_label))
        self.curr_label = []
        
    def char_data(self, data):
        self.curr_label.append(data)

# end of class StatusFieldsHandler


class MenuHandler:
    """Handler for menus and menu items of a menubar"""
    item_attrs = ('label', 'id', 'name', 'checkable')
    def __init__(self):
        self.menu_depth = 0
        self.menus = []
        self.curr_menu = None
        self.curr_item = None
        self.attr_val = []

    def start_elem(self, name, attrs):
        if name == 'menu':
            self.menu_depth += 1
            label = attrs['label']
            if self.menu_depth == 1:
                t = MenuTree(attrs['name'], label)
                self.curr_menu = t.root
                self.menus.append(t)
                return
            node = MenuTree.Node(label=label, name=attrs['name'])
            node.parent = self.curr_menu
            self.curr_menu.children.append(node)
            self.curr_menu = node
        elif name == 'item':
            self.curr_item = MenuTree.Node()

    def end_elem(self, name, code_obj):
        if name == 'menus':
            code_obj.properties['menubar'] = self.menus
            return True
        if name == 'item' and self.curr_menu:
            self.curr_menu.children.append(self.curr_item)
            self.curr_item.parent = self.curr_menu
        elif name == 'menu':
            self.menu_depth -= 1
            self.curr_menu = self.curr_menu.parent
        elif name in self.item_attrs:
            setattr(self.curr_item, name, "".join(self.attr_val))
            self.attr_val = []

    def char_data(self, data):
        self.attr_val.append(data)

# end of class MenuHandler


def initialize():
    cn = common.class_names
    cn['EditFrame'] = 'wxFrame'
    cn['EditStatusBar'] = 'wxStatusBar'
    cn['EditMenuBar'] = 'wxMenuBar'

    pygen = common.code_writers.get('python')
    if pygen:
        awh = pygen.add_widget_handler
        awh('wxFrame', lambda o: None, python_generate_frame_properties)
        awh('wxStatusBar', python_statusbar_code_generator)
        awh('wxMenuBar', python_menubar_code_generator)
        aph = pygen.add_property_handler
        aph('menubar', pygen.DummyPropertyHandler)
        aph('statusbar', pygen.DummyPropertyHandler)
        aph('fields', StatusFieldsHandler)
        aph('menus', MenuHandler)

