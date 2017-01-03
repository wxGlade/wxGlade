"""
Code generator functions for wxMenuBar objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2016-2017 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
from wcodegen import PythonWidgetCodeWriter, CppWidgetCodeWriter
from .menubar_base import MenubarMixin
from wcodegen.taghandler import BaseCodeWriterTagHandler
from MenuTree import *


class PythonMenubarGenerator(MenubarMixin, PythonWidgetCodeWriter):

    tmpl_menubar_new = 'self.%(menubar)s = wx.MenuBar()\n'
    tmpl_toplevel_set_menubar = 'self.SetMenuBar(self.%(menubar)s)\n'

    tmpl_menu_new = '%s = wx.Menu()\n'
    tmpl_menu_append_separator = '%(menu)s.AppendSeparator()\n'
    tmpl_menu_append_to_menubar = '%(menubar)s.Append(%(menu)s, %(label)s)\n'
    tmpl_menu_append_to_menu = '%(parent_menu)s.AppendSubMenu(%(sub_menu)s, %(label)s, %(help)s)\n'
    tmpl_menu_add_menuitem = '%(menu)s.Append(%(args)s)\n'
    tmpl_menu_add_and_assign_menuitem = '%(assigment)s = %(menu)s.Append(%(args)s)\n'
    tmpl_menu_tmpname = 'wxglade_tmp_menu'

    tmpl_menuitem_new = '%s = wx.MenuItem(%s)\n'
    tmpl_menuitem_append_to_menu = '%s.AppendItem(%s)\n'
    tmpl_menuitem_tmp_variable = 'item'

    tmpl_inline_access_class_scope = 'self.%(member)s'
    tmpl_inline_access_local_scope = 'self.%(member)s'

    tmpl_bind = 'self.Bind(wx.EVT_MENU, %(handler)s, id=%(item)s.GetId())\n'


class MenuHandler(BaseCodeWriterTagHandler):
    "Handler for menus and menu items of a menubar"
    item_attrs = ('label', 'id', 'name', 'help_str', 'checkable', 'radio', 'handler')

    def __init__(self):
        super(MenuHandler, self).__init__()
        self.menu_depth = 0
        self.menus = []
        self.curr_menu = None
        self.curr_item = None

    def start_elem(self, name, attrs):
        if name == 'menu':
            self.menu_depth += 1
            label = attrs['label']
            if self.menu_depth == 1:
                t = MenuTree(attrs['name'], label)
                self.curr_menu = t.root
                self.menus.append(t)
                return
            id = attrs.get('itemid', '')
            handler = attrs.get('handler', '')
            node = MenuTree.Node(label=label, name=attrs['name'], id=id, handler=handler)
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
            char_data = self.get_char_data()
            setattr(self.curr_item, name, char_data)


def xrc_code_generator(obj):
    "function that generates XRC code for the menubar of a wxFrame."
    from xml.sax.saxutils import escape, quoteattr
    xrcgen = common.code_writers['XRC']

    class MenuBarXrcObject(xrcgen.DefaultXrcObject):
        def append_item(self, item, outfile, tabs):
            write = outfile.write
            if item.name == '---':  # item is a separator
                write('    '*tabs + '<object class="separator"/>\n')
            else:
                if item.children:
                    name = self.get_name(item)
                    if name:
                        write('    '*tabs + '<object class="wxMenu" name=%s>\n' % quoteattr(name))
                    else:
                        write('    '*tabs + '<object class="wxMenu">\n')
                else:
                    name = self.get_name(item)
                    if name:
                        write('    '*tabs + '<object class="wxMenuItem" name=%s>\n' % quoteattr(name))
                    else:
                        write('    '*tabs + '<object class="wxMenuItem">\n')
                if item.label:
                    # translate & into _ as accelerator marker
                    val = item.label.replace('&', '_')
                    write('    '*(tabs+1) + '<label>%s</label>\n' % escape(val))
                if item.help_str:
                    write('    '*(tabs+1) + '<help>%s</help>\n' % escape(item.help_str))
                if item.children:
                    for c in item.children:
                        self.append_item(c, outfile, tabs+1)
                elif item.checkable == '1':
                    write('    '*(tabs+1) + '<checkable>1</checkable>\n')
                elif item.radio == '1':
                    write('    '*(tabs+1) + '<radio>1</radio>\n')
                write('    '*tabs + '</object>\n')

        def get_name(self, item):
            if item.name: return item.name.strip()
            tokens = item.id.split('=')
            if tokens: return tokens[0].strip()

        def write(self, outfile, tabs):
            menus = self.code_obj.properties['menubar']
            write = outfile.write
            write('    '*tabs + '<object class="wxMenuBar" name=%s>\n' % \
                  quoteattr(self.name))
            for m in menus:
                self.append_item(m.root, outfile, tabs+1)
            write('    '*tabs + '</object>\n')

    # end of class MenuBarXrcObject

    return MenuBarXrcObject(obj)


class CppMenubarGenerator(MenubarMixin, CppWidgetCodeWriter):
    constructor = []
    import_modules = ['<wx/menu.h>']

    add_menuids_to_initcode = False

    tmpl_bind = 'Bind(wxEVT_MENU, &%(handler)s, this, %(item)s->GetId());\n'
    tmpl_bind_28 = 'Connect(%(item)s->GetId(), wxEVT_COMMAND_MENU_SELECTED, wxCommandEventHandler(%(handler)s));\n'

    tmpl_menubar_new = '%(menubar)s = new wxMenuBar();\n'
    tmpl_toplevel_set_menubar = 'SetMenuBar(%(menubar)s);\n'

    tmpl_menu_new = '%s = new wxMenu();\n'
    tmpl_menu_append_separator = '%(menu)s->AppendSeparator();\n'
    tmpl_menu_append_to_menubar = '%(menubar)s->Append(%(menu)s, %(label)s);\n'
    tmpl_menu_append_to_menu = '%(parent_menu)s->AppendSubMenu(%(sub_menu)s, %(label)s, %(help)s);\n'
    tmpl_menu_add_menuitem = '%(menu)s->Append(%(args)s);\n'
    tmpl_menu_add_and_assign_menuitem = '%(assigment)s = %(menu)s->Append(%(args)s);\n'
    tmpl_menu_tmpname = 'wxglade_tmp_menu'

    tmpl_menuitem_new = '%s = new wxMenuItem(%s);\n'
    tmpl_menuitem_append_to_menu = '%s->Append(%s);\n'
    tmpl_menuitem_tmp_variable = 'item'

    tmpl_inline_access_class_scope = '%(toplevel)s::%(member)s'
    tmpl_inline_access_local_scope = '%(member)s'

    tmpl_menuitem_declare_tmp_variable = 'wxMenuItem* %s;\n'
    tmpl_menu_declare_tmp_variable = 'wxMenu* %s;\n'

    def get_code(self, obj):
        # called for non-toplevel widgets
        init_lines, prop_lines, layout_lines = super(CppMenubarGenerator, self).get_code(obj)
        return init_lines, self._menuitem_ids, prop_lines, layout_lines

    def get_init_code(self, obj):
        # called for toplevel widgets
        init_lines = super(CppMenubarGenerator, self).get_init_code(obj)
        return self._menuitem_ids + init_lines


def initialize():
    klass = 'wxMenuBar'
    common.class_names['EditMenuBar'] = klass
    common.toplevels['EditMenuBar'] = 1
    common.register('python', klass, PythonMenubarGenerator(klass), 'menus', MenuHandler)
    common.register('C++',    klass, CppMenubarGenerator(klass),    'menus', MenuHandler)
    common.register('XRC',    klass, xrc_code_generator,            'menus', MenuHandler)
