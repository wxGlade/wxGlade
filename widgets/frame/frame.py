# frame.py: wxFrame, wxMenuBar and wxStatusBar objects
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

from wxPython.wx import *
import common, math, misc
from tree import Tree
from MenuTree import *
from widget_properties import *
from edit_windows import EditBase, TopLevelBase

class MenuItemDialog(wxDialog):
    def __init__(self, parent, items=None):
        wxDialog.__init__(self, parent, -1, "Menu editor",
                          style=wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER)
        ADD_ID, REMOVE_ID, NAME_ID, LABEL_ID, ID_ID, CHECK_ID, LIST_ID, \
                ADD_SEP_ID, MOVE_LEFT_ID, MOVE_RIGHT_ID, MOVE_UP_ID, \
                MOVE_DOWN_ID, HELP_STR_ID = [wxNewId() for i in range(13)]
        self.menu_items = wxListCtrl(self, LIST_ID, style=wxLC_REPORT | \
                                     wxLC_SINGLE_SEL|wxSUNKEN_BORDER)
        self.menu_items.InsertColumn(0, "Label")
        self.menu_items.InsertColumn(1, "Id")
        self.menu_items.InsertColumn(2, "Name")
        self.menu_items.InsertColumn(3, "Help String")
        self.menu_items.InsertColumn(4, "Checkable")
        self.menu_items.SetColumnWidth(0, 250)
        self.menu_items.SetColumnWidth(2, 250)
        self.menu_items.SetColumnWidth(3, 250)
        # menu item fields
        self.id = wxTextCtrl(self, ID_ID)
        self.label = wxTextCtrl(self, LABEL_ID)
        self.name = wxTextCtrl(self, NAME_ID)
        self.help_str = wxTextCtrl(self, HELP_STR_ID)
        self.checkable = wxCheckBox(self, CHECK_ID, "") #Checkable")

        self.add = wxButton(self, ADD_ID, "Add")
        self.remove = wxButton(self, REMOVE_ID, "Remove")
        self.add_sep = wxButton(self, ADD_SEP_ID, "Add separator")

        # menu items navigation
        self.move_up = wxButton(self, MOVE_UP_ID, "Up")
        self.move_down = wxButton(self, MOVE_DOWN_ID, "Down")
        self.move_left = wxButton(self, MOVE_LEFT_ID, " < ")
        self.move_right = wxButton(self, MOVE_RIGHT_ID, " > ")

        self.ok = wxButton(self, wxID_OK, " OK ")
        self.cancel = wxButton(self, wxID_CANCEL, "Cancel")

        self.do_layout()
        self.selected_index = -1 # index of the selected element in the 
                                 # wxListCtrl menu_items
        # event handlers
        EVT_BUTTON(self, ADD_ID, self.add_menu_item)
        EVT_BUTTON(self, REMOVE_ID, self.remove_menu_item)
        EVT_BUTTON(self, ADD_SEP_ID, self.add_separator)
        EVT_BUTTON(self, MOVE_LEFT_ID, self.move_item_left)
        EVT_BUTTON(self, MOVE_RIGHT_ID, self.move_item_right)
        EVT_BUTTON(self, MOVE_UP_ID, self.move_item_up)
        EVT_BUTTON(self, MOVE_DOWN_ID, self.move_item_down)
        EVT_KILL_FOCUS(self.name, self.update_menu_item)
        EVT_KILL_FOCUS(self.label, self.update_menu_item)
        EVT_KILL_FOCUS(self.id, self.update_menu_item)
        EVT_KILL_FOCUS(self.help_str, self.update_menu_item)
        EVT_CHECKBOX(self, CHECK_ID, self.update_menu_item)
        EVT_LIST_ITEM_SELECTED(self, LIST_ID, self.show_menu_item)
        if items:
            self.add_items(items)

    def do_layout(self):
        self.label.Enable(0)
        self.id.Enable(0)
        self.name.Enable(0)
        self.help_str.Enable(0)
        self.checkable.Enable(0)
        
        sizer = wxBoxSizer(wxVERTICAL)
        sizer2 = wxStaticBoxSizer(wxStaticBox(self, -1, "Menu item:"), \
                                  wxVERTICAL)
        self.label.SetSize((150, -1))
        self.id.SetSize((150, -1))
        self.name.SetSize((150, -1))
        self.help_str.SetSize((150, -1))
        szr = wxFlexGridSizer(0, 2)
        szr.Add(wxStaticText(self, -1, "Id   "))
        szr.Add(self.id)
        szr.Add(wxStaticText(self, -1, "Label  "))
        szr.Add(self.label)
        szr.Add(wxStaticText(self, -1, "Name  "))
        szr.Add(self.name)
        szr.Add(wxStaticText(self, -1, "Help String  "))
        szr.Add(self.help_str)
        szr.Add(wxStaticText(self, -1, "Checkable  "), 0,
                wxALIGN_CENTER_VERTICAL)
        szr.Add(self.checkable, 0, wxTOP|wxBOTTOM, 2)
        sizer2.Add(szr, 1, wxALL|wxEXPAND, 5)
        szr = wxGridSizer(0, 2, 3, 3)
        szr.Add(self.add, 0, wxEXPAND); szr.Add(self.remove, 0, wxEXPAND)
        sizer2.Add(szr, 0, wxEXPAND)
        sizer2.Add(self.add_sep, 0, wxTOP|wxEXPAND, 3)

        sizer3 = wxBoxSizer(wxVERTICAL)
        sizer3.Add(self.menu_items, 1, wxALL|wxEXPAND, 5)
        sizer4 = wxBoxSizer(wxHORIZONTAL)

        sizer4.Add(self.move_up, 0, wxLEFT|wxRIGHT, 3)
        sizer4.Add(self.move_down, 0, wxLEFT|wxRIGHT, 5)
        sizer4.Add(self.move_left, 0, wxLEFT|wxRIGHT, 5)
        sizer4.Add(self.move_right, 0, wxLEFT|wxRIGHT, 5)
        sizer3.Add(sizer4, 0, wxALIGN_CENTER|wxALL, 5)
        szr = wxBoxSizer(wxHORIZONTAL)
        szr.Add(sizer3, 1, wxALL|wxEXPAND, 5) 
        szr.Add(sizer2, 0, wxTOP|wxBOTTOM|wxRIGHT, 5)
        sizer.Add(szr, 1, wxEXPAND)
        sizer2 = wxBoxSizer(wxHORIZONTAL)
        sizer2.Add(self.ok, 0, wxALL, 5)
        sizer2.Add(self.cancel, 0, wxALL, 5)
        sizer.Add(sizer2, 0, wxALL|wxALIGN_CENTER, 3)
        self.SetAutoLayout(1)
        self.SetSizer(sizer)
        sizer.Fit(self)
        self.SetSize((-1, 350))
        self.CenterOnParent()

    def add_menu_item(self, event):
        """\
        Event handler called when the Add button is clicked
        """
        index = self.selected_index = self.selected_index+1
        if not self.menu_items.GetItemCount():
            [s.Enable(1) for s in (self.label, self.id, self.name, \
                                   self.help_str, self.checkable)]
        if index < 0: index = self.menu_items.GetItemCount()
        elif index > 0: indent = "    " * self.item_level(index-1)
        else: indent = ""
        name, label, id, check = "", "item", "", "0"
        self.menu_items.InsertStringItem(index, indent + label)
        self.menu_items.SetStringItem(index, 1, id)
        self.menu_items.SetStringItem(index, 2, name)
        self.menu_items.SetStringItem(index, 3, check)
        self.name.SetValue(name)
        self.label.SetValue(label)
        self.id.SetValue(id)
        self.checkable.SetValue(int(check))

    def add_separator(self, event):
        """\
        Event handler called when the Add Separator button is clicked
        """
        index = self.selected_index+1
        if not self.menu_items.GetItemCount():
            [s.Enable(1) for s in (self.label, self.id, self.name, \
                                   self.help_str, self.checkable)]
        if index < 0: index = self.menu_items.GetItemCount() 
        elif index > 0: label = "    " * self.item_level(index-1) + '---'
        else: label = '---'
        self.menu_items.InsertStringItem(index, label)
        self.menu_items.SetStringItem(index, 1, '---')
        self.menu_items.SetStringItem(index, 2, '---')

    def show_menu_item(self, event):
        """\
        Event handler called when a menu item in the list is selected
        """        
        self.selected_index = index = event.GetIndex()
        if not self.menu_items.GetItem(index, 2).m_text == '---':
            # skip if the selected item is a separator
            [s.SetValue(self.menu_items.GetItem(index, i).m_text) for (s, i) \
             in ((self.label, 0), (self.id, 1), (self.name, 2),
                 (self.help_str, 3))]
            self.label.SetValue(self.label.GetValue().lstrip())
            try:
                self.checkable.SetValue(int(self.menu_items.GetItem(index, \
                                                                    4).m_text))
            except: self.checkable.SetValue(0)
        event.Skip()

    def update_menu_item(self, event):
        """\
        Event handler called when some of the properties of the current menu
        item changes
        """        
        set_item = self.menu_items.SetStringItem
        index = self.selected_index
        if index < 0: return
        set_item(index, 0, "    " * self.item_level(index) + \
                 self.label.GetValue().lstrip())
        set_item(index, 1, self.id.GetValue())
        set_item(index, 2, self.name.GetValue())
        set_item(index, 3, self.help_str.GetValue())
        set_item(index, 4, str(self.checkable.GetValue()))
        event.Skip()

    def item_level(self, index, label=None):
        """\
        returns the indentation level of the menu item at the given index
        """
        label = self.menu_items.GetItem(index, 0).m_text
        return (len(label) - len(label.lstrip())) / 4
    
    def remove_menu_item(self, event):
        """\
        Event handler called when the Remove button is clicked
        """        
        if self.selected_index >= 0:
            index = self.selected_index+1
            if index < self.menu_items.GetItemCount() and \
               (self.item_level(self.selected_index) < self.item_level(index)):
                self._move_item_left(index)
                self.selected_index = index-1
            [s.SetValue("") for s in (self.name, self.id, self.label)]
            self.checkable.SetValue(0)
            self.menu_items.DeleteItem(self.selected_index)
            if not self.menu_items.GetItemCount():
                [s.Enable(0) for s in (self.name, self.id, self.label, \
                                       self.help_str, self.checkable)]

    def add_items(self, menus):
        """\
        adds the content of 'menus' to self.menu_items. menus is a sequence of
        trees which describes the structure of the menus
        """
        indent = " " * 4
        set_item = self.menu_items.SetStringItem
        add_item = self.menu_items.InsertStringItem
        index = [0]
        def add(node, level):
            i = index[0]
            add_item(i, indent * level + node.label.lstrip())
            set_item(i, 1, node.id)
            set_item(i, 2, node.name)
            set_item(i, 3, node.help_str)
            set_item(i, 4, node.checkable)
            index[0] += 1
            [add(item, level+1) for item in node.children]
        for tree in menus:
            add(tree.root, 0)
        if self.menu_items.GetItemCount():
            [s.Enable(1) for s in (self.name, self.id, self.label, \
                                   self.help_str, self.checkable)]
            

    def get_menus(self):
        """\
        returns the contents of self.menu_items as a list of trees which
        describe the structure of the menus in the format used by EditMenuBar
        """
        def get(i, j): return self.menu_items.GetItem(i, j).m_text
        trees = []
        def add(node, index):
            n = MenuTree.Node(get(index, 0).lstrip(), *[ get(index, i) for i \
                                                         in range(1, 5) ])
            node.children.append(n)
            n.parent = node
            return n
        level = 0
        curr_item = None
        for index in range(self.menu_items.GetItemCount()):
            label = get(index, 0)
            lvl = self.item_level(index) # get the indentation level
            if not lvl:
                t = MenuTree(get(index, 2), label)
                curr_item = t.root
                level = 1
                trees.append(t)
                continue
            elif lvl < level:
                for i in range(level-lvl):
                    curr_item = curr_item.parent
                level = lvl
            elif lvl > level:
                curr_item = curr_item.children[-1]
                level = lvl
            add(curr_item, index)

        return trees

    def _move_item_left(self, index):
        if index > 0:
            if (index+1 < self.menu_items.GetItemCount() and \
                (self.item_level(index) < self.item_level(index+1))):
                return
            label = self.menu_items.GetItem(index, 0).m_text
            if label[:4] == " " * 4:
                self.menu_items.SetStringItem(index, 0, label[4:])
                self.menu_items.SetItemState(index, wxLIST_STATE_SELECTED, 
                                             wxLIST_STATE_SELECTED)
                self.menu_items.SetFocus()
                
    def move_item_left(self, event):
        """\
        moves the selected menu item one level up in the hierarchy, i.e.
        shifts its label 4 spaces left in self.menu_items
        """
        self._move_item_left(self.selected_index)

    def _move_item_right(self, index):
        if index > 0 and (self.item_level(index) <= self.item_level(index-1)): 
            label = self.menu_items.GetItem(index, 0).m_text
            self.menu_items.SetStringItem(index, 0, " " * 4 + label)
            self.menu_items.SetItemState(index, wxLIST_STATE_SELECTED, \
                                         wxLIST_STATE_SELECTED)
            self.menu_items.SetFocus()

    def move_item_right(self, event):
        """\
        moves the selected menu item one level down in the hierarchy, i.e.
        shifts its label 4 spaces right in self.menu_items
        """
        self._move_item_right(self.selected_index)

    def move_item_up(self, event):
        """\
        moves the selected menu item before the previous one at the same level
        in self.menu_items
        """
        index = self.selected_index
        if index <= 0: return
        def get(i, j): return self.menu_items.GetItem(i, j).m_text
        def getall(i): return [get(i, j) for j in range(4)]
        level = self.item_level(index)
        items_to_move = [ getall(index) ]
        i = index+1
        while i < self.menu_items.GetItemCount():
            # collect the items to move up
            if level < self.item_level(i):
                items_to_move.append(getall(i))
                i += 1
            else: break
        i = index-1
        while i >= 0:
            lvl = self.item_level(i)
            if level == lvl: break
            elif level > lvl: return
            i -= 1
        delete = self.menu_items.DeleteItem
        insert = self.menu_items.InsertStringItem
        set = self.menu_items.SetStringItem
        for j in range(len(items_to_move)-1, -1, -1):
            delete(index+j)
        items_to_move.reverse()
        for label, id, name, checkable in items_to_move:
            i = insert(i, label)
            set(i, 1, id)
            set(i, 2, name)
            set(i, 3, checkable)
        self.menu_items.SetItemState(i, wxLIST_STATE_SELECTED, \
                                     wxLIST_STATE_SELECTED)
        self.menu_items.SetFocus()
        
    def move_item_down(self, event):
        """\
        moves the selected menu item after the next one at the same level
        in self.menu_items
        """
        index = self.selected_index
        self.selected_index = -1
        if index < 0: return
        def get(i, j): return self.menu_items.GetItem(i, j).m_text
        def getall(i): return [get(i, j) for j in range(4)]
        level = self.item_level(index)
        i = index+1
        while i < self.menu_items.GetItemCount():
            # collect the items to move down
            if level < self.item_level(i):
                i += 1
            else: break
        if i < self.menu_items.GetItemCount():
            self.selected_index = i
            self.move_item_up(event)
                                                  
#end of class MenuItemDialog


class MenuProperty(Property):
    """\
    Property to edit the menus of an EditMenuBar instance.
    """
    def __init__(self, owner, name, parent):
        Property.__init__(self, owner, name, parent)
        self.panel = None
        self.menu_items = {}
        if parent is not None: self.display(parent)

    def display(self, parent):
        self.panel = wxPanel(parent, -1)
        edit_btn_id = wxNewId()
        self.edit_btn = wxButton(self.panel, edit_btn_id, "Edit menus...")
        sizer = wxBoxSizer(wxHORIZONTAL)
        sizer.Add(self.edit_btn, 1, wxEXPAND|wxALIGN_CENTER|wxTOP|wxBOTTOM, 4)
        self.panel.SetAutoLayout(1)
        self.panel.SetSizer(sizer)
        self.panel.SetSize(sizer.GetMinSize())
        EVT_BUTTON(self.panel, edit_btn_id, self.edit_menus)

    def bind_event(*args): pass

    def edit_menus(self, event):
        dialog = MenuItemDialog(self.panel, items=self.owner.get_menus())
        if dialog.ShowModal() == wxID_OK:
            self.owner.set_menus(dialog.get_menus())
            common.app_tree.app.saved = False # update the status of the app

    def write(self, outfile, tabs):
        fwrite = outfile.write
        fwrite('    ' * tabs + '<menus>\n')
        for menu in self.owner[self.name][0]():
            menu.write(outfile, tabs+1)
        fwrite('    ' * tabs + '</menus>\n')

# end of class MenuProperty


class EditMenuBar(EditBase):
    __hidden_frame = None # used on GTK to reparent a menubar before deletion
    
    def __init__(self, parent, property_window):
        #wxMenuBar.__init__(self)
        EditBase.__init__(self, parent.name + '_menubar', 'wxMenuBar',
                          parent, wxNewId(), property_window,
                          custom_class=False, show=False)
        def nil(*args): return ()
        self.menus = [] # list of MenuTree objects
        self.access_functions['menus'] = (self.get_menus, self.set_menus)
        prop = self.properties['menus'] = MenuProperty(self, 'menus', None) 
        self.node = Tree.Node(self)
        common.app_tree.add(self.node, parent.node)

    def create_widget(self):
        if wxPlatform == '__WXGTK__' and not EditMenuBar.__hidden_frame:
            EditMenuBar.__hidden_frame = wxFrame(common.palette, -1, "")
            EditMenuBar.__hidden_frame.Hide()
        self.widget = wxMenuBar()
        if wxPlatform == '__WXMSW__':
            self.widget.SetFocus = lambda : None
        if self.parent.widget: self.parent.widget.SetMenuBar(self.widget)
        EVT_LEFT_DOWN(self.widget, self.on_set_focus)
        self.set_menus(self.menus) # show the menus

    def create_properties(self):
        EditBase.create_properties(self)
        page = self._common_panel
        sizer = page.GetSizer()
        self.properties['menus'].display(page)
        if not sizer:
            sizer = wxBoxSizer(wxVERTICAL)
            sizer.Add(self.name_prop.panel, 0, wxEXPAND)
            sizer.Add(self.klass_prop.panel, 0, wxEXPAND)
            page.SetAutoLayout(1)
            page.SetSizer(sizer)
        sizer.Add(self.properties['menus'].panel, 0, wxALL|wxEXPAND, 3)
        sizer.Fit(page)
        page.SetSize(self.notebook.GetClientSize())
        sizer.Layout()
        self.notebook.AddPage(page, "Common")
        self.property_window.Layout()
        
    def __getitem__(self, key):
        return self.access_functions[key]

    def get_menus(self):
        return self.menus

    def set_menus(self, menus):
        self.menus = menus
        if not self.widget: return # nothing left to do
        for i in range(self.widget.GetMenuCount()): self.widget.Remove(i)
        def append(menu, items):
            for item in items:
                if item.name == '---': # item is a separator
                    menu.AppendSeparator()
                elif item.children:
                    m = wxMenu()
                    append(m, item.children)
                    menu.AppendMenu(wxNewId(), item.label, m, item.help_str)
                else:
                    try: checkable = int(item.checkable)
                    except: checkable = 0
                    menu.Append(wxNewId(), item.label, item.help_str,
                                checkable)
        first = self.widget.GetMenuCount()
        for menu in self.menus:
            m = wxMenu()
            append(m, menu.root.children)
            if first:
                self.widget.Replace(0, m, menu.root.label)
                first = 0
            else: self.widget.Append(m, menu.root.label)
        self.widget.Refresh()
      
    def remove(self, *args):
        self.parent.properties['menubar'].set_value(0)
        if self.parent.widget:
            if wxPlatform == '__WXGTK__':
                self.widget.Reparent(EditMenuBar.__hidden_frame)
                self.widget.Hide()
            self.parent.widget.SetMenuBar(None)
        EditBase.remove(self)

    def popup_menu(self, *args): pass

    def get_property_handler(self, name):
        class MenuHandler:
            itemattrs = ['label', 'id', 'name', 'help_str', 'checkable']
            def __init__(self, owner):
                self.owner = owner
                self.menu_items = []
                self.curr_menu = []
                self.curr_item = None
                self.curr_index = 0
                self.menu_depth = 0
            def start_elem(self, name, attrs):
                if name == 'menus': return
                if name == 'menu':
                    self.menu_depth += 1
                    label = misc._encode(attrs['label'])
                    if self.menu_depth == 1:
                        t = MenuTree(attrs['name'], label)
                        self.curr_menu.append( (t.root,) )
                        self.owner.menus.append(t)
                        return
                    node = MenuTree.Node(label=label, name=attrs['name'])
                    cm = self.curr_menu[-1]
                    cm[0].children.append(node)
                    node.parent = cm[0]
                    menu = wxMenu()
                    self.curr_menu.append( (node, menu) )
                elif name == 'item':
                    self.curr_item = MenuTree.Node()
                else:
                    try: self.curr_index = self.itemattrs.index(name)
                    except ValueError:
                        from xml_parse import XmlParsingError
                        raise XmlParsingError, "invalid menu item attribute"
            def end_elem(self, name):
                if name == 'item':
                    try: cm = self.curr_menu[-1]
                    except IndexError:
                        from xml_parse import XmlParsingError
                        raise XmlParsingError, "menu item outside a menu"
                    cm[0].children.append(self.curr_item)
                    self.curr_item.parent = cm[0]
                elif name == 'menu':
                    self.menu_depth -= 1
                    self.curr_menu.pop()
                elif name == 'menus':
                    self.owner.set_menus(self.owner.menus)
                    return True
            def char_data(self, data):
                setattr(self.curr_item, self.itemattrs[self.curr_index], data)
                
        if name == 'menus':
            return MenuHandler(self)
        return None

# end of class EditMenuBar


class EditStatusBar(EditBase):
    def __init__(self, parent, property_window):
        EditBase.__init__(self, parent.name + '_statusbar',
                          'wxStatusBar', parent, id, property_window,
                          custom_class=False, show=False)
        self.fields = [ [self.name, "-1"] ] # list of 2-lists label, size
                                            # for the statusbar fields
        self.access_functions['fields'] = (self.get_fields, self.set_fields) 
        prop = self.properties['fields'] = GridProperty(self, 'fields', None,
                                                        [("Text",
                                                          GridProperty.STRING),
                                                         ("Size",
                                                          GridProperty.INT)])
        # replace the default 'write' method of 'prop' with a custom one
        def write_prop(outfile, tabs):
            from xml.sax.saxutils import escape, quoteattr
            fwrite = outfile.write
            fwrite('    ' * tabs + '<fields>\n')
            tabs += 1
            import widget_properties
            for label, width in self.fields:
                fwrite('    ' * tabs + '<field width=%s>%s</field>\n' %
                       (quoteattr(width),
                        escape(widget_properties._encode(label))))
            tabs -= 1
            fwrite('    ' * tabs + '</fields>\n')
        prop.write = write_prop

        self.node = Tree.Node(self)
        common.app_tree.add(self.node, parent.node)

    def create_widget(self):
        self.widget = wxStatusBar(self.parent.widget, wxNewId())
        EVT_LEFT_DOWN(self.widget, self.on_set_focus)
        self.set_fields(self.fields)
        if self.parent.widget: self.parent.widget.SetStatusBar(self.widget)

    def create_properties(self):
        EditBase.create_properties(self)
        page = self._common_panel 
        prop = self.properties['fields']
        prop.display(page)
        sizer = page.GetSizer()
        if not sizer:
            sizer = wxBoxSizer(wxVERTICAL)
            sizer.Add(self.name_prop.panel, 0, wxEXPAND)
            sizer.Add(self.klass_prop.panel, 0, wxEXPAND)
            page.SetAutoLayout(1)
            page.SetSizer(sizer)
        sizer.Add(prop.panel, 1, wxALL|wxEXPAND, 3)
        sizer.Fit(page)
        page.SetSize(self.notebook.GetClientSize())
        sizer.Layout()
        self.notebook.AddPage(page, "Common")
        self.property_window.Layout()
        prop.set_col_sizes([190, 0])

    def set_fields(self, values):
        # values is a list of lists
        self.fields = []
        if self.widget: self.widget.SetFieldsCount(len(values))
        for i in range(len(values)):
            try: v = int(values[i][1])
            except: v = 0
            s = values[i][0]
            self.fields.append([s, str(v)])
            if self.widget: self.widget.SetStatusText(s, i)
        if self.widget:
            self.widget.SetStatusWidths([int(i[1]) for i in self.fields])

    def get_fields(self):
        return self.fields
    
    def __getitem__(self, key):
        return self.access_functions[key]

    def remove(self, *args):
        if self.parent.widget: self.parent.widget.SetStatusBar(None)
        self.parent.properties['statusbar'].set_value(0)
        if self.widget: self.widget.Hide()
        EditBase.remove(self)

    def popup_menu(self, *args): pass # to avoid strange segfault :)

    def get_property_handler(self, name):
        class FieldsHandler:
            """\
            custom Property handler for statusbar fields.
            """
            def __init__(self, owner):
                self.owner = owner
                self.width = -1
                self.value = []
            def start_elem(self, name, attrs):
                if name == 'fields': self.fields = []
                else: # name == 'field'
                    self.value = []
                    self.width = attrs.get('width', '-1')
            def end_elem(self, name): 
                if name == 'field':
                    self.fields.append(["".join(self.value), self.width])
                else: # name == 'fields'
                    self.owner.fields = self.fields
                    self.owner.set_fields(self.owner.fields)
                    self.owner.properties['fields'].set_value(
                        self.owner.fields)
                    return True
            def char_data(self, data):
                self.value.append(data)
                return False # tell there's no need to go further
                             # (i.e. to call add_property)

        if name == 'fields': return FieldsHandler(self)
        return None

# end of class EditStatusBar


class EditFrame(TopLevelBase):
    def __init__(self, name, parent, id, title, property_window,
                 style=wxDEFAULT_FRAME_STYLE, show=True, klass='wxFrame'):
        TopLevelBase.__init__(self, name, klass, parent, id,
                              property_window, show=show)
        self.style = style
        self.statusbar = None
        self.access_functions['statusbar'] = (self.get_statusbar,
                                              self.set_statusbar)
        self.menubar = None
        self.access_functions['menubar'] = (self.get_menubar, self.set_menubar)

        self.access_functions['style'] = (self.get_style, self.set_style)
        prop = self.properties
        style_labels = ('#section#Style', 'wxDEFAULT_FRAME_STYLE',
                        'wxICONIZE', 'wxCAPTION',
                        'wxMINIMIZE', 'wxMINIMIZE_BOX', 'wxMAXIMIZE',
                        'wxMAXIMIZE_BOX', 'wxSTAY_ON_TOP', 'wxSYSTEM_MENU',
                        'wxSIMPLE_BORDER', 'wxRESIZE_BORDER',
                        'wxFRAME_TOOL_WINDOW', 'wxFRAME_NO_TASKBAR')
        self.style_pos = (wxDEFAULT_FRAME_STYLE,
                          wxICONIZE, wxCAPTION, wxMINIMIZE,
                          wxMINIMIZE_BOX, wxMAXIMIZE, wxMAXIMIZE_BOX,
                          wxSTAY_ON_TOP, wxSYSTEM_MENU, wxSIMPLE_BORDER,
                          wxRESIZE_BORDER, wxFRAME_TOOL_WINDOW,
                          wxFRAME_NO_TASKBAR)
        prop['style'] = CheckListProperty(self, 'style', None, style_labels)
        # menubar property
        prop['menubar'] = CheckBoxProperty(self, 'menubar', None,
                                           'Has MenuBar')
        # statusbar property
        prop['statusbar'] = CheckBoxProperty(self, 'statusbar', None,
                                             'Has StatusBar')

    def create_widget(self):
        if self.parent: w = self.parent.widget
        else: w = None
        self.widget = wxFrame(w, self.id, self.get_title())

    def finish_widget_creation(self):
        TopLevelBase.finish_widget_creation(self)
        if not self.properties['size'].is_active():
            #if self.sizer: self.sizer.fit_parent()
            #else:
            self.widget.SetSize((400, 300))
        if wxPlatform == '__WXMSW__':
            self.widget.CenterOnScreen()
        if self.menubar and self.menubar.widget:
            self.widget.SetMenuBar(self.menubar.widget)
        if self.statusbar and self.statusbar.widget:
            self.widget.SetStatusBar(self.statusbar.widget)

    def create_properties(self):
        TopLevelBase.create_properties(self)
        prop = self.properties
        panel = wxScrolledWindow(self.notebook, -1)
        prop['style'].display(panel)
        prop['menubar'].display(panel)
        prop['statusbar'].display(panel)
        
        szr = wxBoxSizer(wxVERTICAL)
        szr.Add(prop['style'].panel, 0, wxEXPAND)
        szr.Add(prop['menubar'].panel, 0, wxEXPAND)
        szr.Add(prop['statusbar'].panel, 0, wxEXPAND)
        panel.SetAutoLayout(True)
        panel.SetSizer(szr)
        szr.Fit(panel)
        self.notebook.AddPage(panel, 'Widget')
        w, h = panel.GetClientSizeTuple()
        panel.SetScrollbars(5, 5, math.ceil(w/5.0), math.ceil(h/5.0))

    def get_menubar(self):
        return self.menubar is not None

    def set_menubar(self, value):
        if value:
            self.menubar = EditMenuBar(self, common.property_panel)
            if self.widget:
                self.menubar.show_widget(True)
                self.menubar.show_properties()
        else:
            self.menubar = self.menubar.remove()
            self.show_properties(None)

    def get_statusbar(self):
        return self.statusbar is not None

    def set_statusbar(self, value):
        if value:
            self.statusbar = EditStatusBar(self, common.property_panel)
            if self.widget:
                self.statusbar.show_widget(True)
                self.statusbar.show_properties()
        else:
            self.statusbar = self.statusbar.remove()
            self.show_properties(None)
        if self.widget:
            # this is needed at least on win32
            wxPostEvent(self.widget, wxSizeEvent(self.widget.GetSize(),
                                                 self.widget.GetId()))
        
    def get_style(self):
        retval = [0] * len(self.style_pos)
        try:
            if self.style == wxDEFAULT_FRAME_STYLE: retval[0] = 1
            else:
                for i in range(len(self.style_pos)):
                    if self.style & self.style_pos[i]: retval[i] = 1
                retval[0] = 0
        except AttributeError:
            pass
        return retval

    def set_style(self, value):
        value = self.properties['style'].prepare_value(value)
        style = 0
        for v in range(len(value)):
            if value[v]:
                style |= self.style_pos[v]
        self.style = style
        if self.widget: self.widget.SetWindowStyleFlag(style)

    def remove(self, *args):
        if self.statusbar: self.statusbar = self.statusbar.remove()
        if self.menubar: self.menubar = self.menubar.remove()
        TopLevelBase.remove(self, *args)

# end of class EditFrame


class EditMDIChildFrame(EditFrame):
    pass

        
def builder(parent, sizer, pos, number=[0]):
    """\
    factory function for EditFrame objects.
    """
    class Dialog(wxDialog):
        def __init__(self):
            wxDialog.__init__(self, None, -1, 'Select frame class')
            if not number[0]: self.klass = 'MyFrame'
            else: self.klass = 'MyFrame%s' % number[0]
            number[0] += 1
            self.base = 0
            base_prop = RadioProperty(self, 'base class', self,
                                      ['wxFrame', 'wxMDIChildFrame'])
            klass_prop = TextProperty(self, 'class', self)
            szr = wxBoxSizer(wxVERTICAL)
            szr.Add(base_prop.panel, 0, wxALL|wxEXPAND, 5)
            szr.Add(klass_prop.panel, 0, wxEXPAND)
            szr.Add(wxButton(self, wxID_OK, 'OK'), 0, wxALL|wxALIGN_CENTER, 3)
            self.SetAutoLayout(True)
            self.SetSizer(szr)
            szr.Fit(self)
        def __getitem__(self, value):
            if value == 'class':
                def set_klass(c): self.klass = c
                return (lambda : self.klass, set_klass)
            else:
                def set_base(b): self.base = b
                return (lambda : self.base, set_base)
    # end of inner class

    dialog = Dialog()
    dialog.ShowModal()
    label = 'frame_%d' % number[0]
    while common.app_tree.has_name(label):
        number[0] += 1
        label = 'frame_%d' % number[0]
    if dialog.base == 0: base_class = EditFrame
    else: base_class = EditMDIChildFrame
    frame = base_class(label, parent, wxNewId(), label, common.property_panel,
                       klass=dialog.klass)
    node = Tree.Node(frame)
    frame.node = node
    common.app_tree.add(node)
    frame.show_widget(True)

    # add a default vertical sizer to the frame
    import edit_sizers
    edit_sizers._builder(frame, None, 0)
    # now select the frame's node in the tree
    common.app_tree.select_item(node)
    
    dialog.Destroy()
    if wxPlatform == '__WXMSW__':
        #frame.widget.CenterOnScreen()
        frame.widget.Raise()


def _make_builder(base_class):
    def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
        from xml_parse import XmlParsingError
        try: label = attrs['name']
        except KeyError: raise XmlParsingError, "'name' attribute missing"
        frame = base_class(label, parent, wxNewId(), label,
                           common.property_panel,
                           show=False)
        node = Tree.Node(frame)
        frame.node = node
        common.app_tree.add(node)
        return frame
    return xml_builder
        
## def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
##     """\
##     factory to build EditFrame objects from an xml file
##     """
##     from xml_parse import XmlParsingError
##     try: label = attrs['name']
##     except KeyError: raise XmlParsingError, "'name' attribute missing"
##     frame = EditFrame(label, parent, wxNewId(), label, common.property_panel,
##                       show=False)
##     node = Tree.Node(frame)
##     frame.node = node
##     common.app_tree.add(node)
##     return frame

def statusbar_xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    """\
    factory to build EditStatusBar objects from an xml file
    """
    parent.statusbar.set_fields([])
    return parent.statusbar

def menubar_xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    """\
    factory to build EditStatusBar objects from an xml file
    """
    return parent.menubar


def initialize():
    """\
    initialization function for the module: returns a wxBitmapButton to be
    added to the main palette.
    """
    cwx = common.widgets_from_xml
    cwx['EditStatusBar'] = statusbar_xml_builder
    cwx['EditMenuBar'] = menubar_xml_builder
    cwx['EditFrame'] = _make_builder(EditFrame) #xml_builder
    cwx['EditMDIChildFrame'] = _make_builder(EditMDIChildFrame)

    common.widgets['EditFrame'] = builder
    
    # add statusbar and menubar icons to WidgetTree
    from tree import WidgetTree
    WidgetTree.images['EditStatusBar'] = 'icons/statusbar.xpm'
    WidgetTree.images['EditMenuBar'] = 'icons/menubar.xpm'
    WidgetTree.images['EditMDIChildFrame'] = 'icons/frame.xpm'
       
    return common.make_object_button('EditFrame', 'icons/frame.xpm', 1)
