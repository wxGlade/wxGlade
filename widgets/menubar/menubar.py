# menubar.py: wxMenuBar objects
# $Id: menubar.py,v 1.22 2005/05/06 21:48:20 agriggio Exp $
#
# Copyright (c) 2002-2005 Alberto Griggio <agriggio@users.sourceforge.net>
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

from wxPython.wx import *
import common, math, misc
from tree import Tree
from MenuTree import *
from widget_properties import *
from edit_windows import EditBase, TopLevelBase, PreviewMixin


class MenuItemDialog(wxDialog):
    def __init__(self, parent, items=None):
        wxDialog.__init__(self, parent, -1, "Menu editor",
                          style=wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER)
        ADD_ID, REMOVE_ID, NAME_ID, LABEL_ID, ID_ID, CHECK_RADIO_ID, LIST_ID, \
                ADD_SEP_ID, MOVE_LEFT_ID, MOVE_RIGHT_ID, MOVE_UP_ID, \
                MOVE_DOWN_ID, HELP_STR_ID = [wxNewId() for i in range(13)]

        self._staticbox = wxStaticBox(self, -1, "Menu item:")
        
        self.menu_items = wxListCtrl(self, LIST_ID, style=wxLC_REPORT | \
                                     wxLC_SINGLE_SEL|wxSUNKEN_BORDER)
        # ALB 2004-09-26: workaround to make the scroll wheel work...
        EVT_MOUSEWHEEL(self.menu_items, lambda e: e.Skip())
        
        self.menu_items.InsertColumn(0, "Label")
        self.menu_items.InsertColumn(1, "Id")
        self.menu_items.InsertColumn(2, "Name")
        self.menu_items.InsertColumn(3, "Help String")
        self.menu_items.InsertColumn(4, "Type")
        # ALB 2004-12-05
        self.menu_items.InsertColumn(5, "Event Handler")

        self.menu_items.SetColumnWidth(0, 250)
        self.menu_items.SetColumnWidth(2, 250)
        self.menu_items.SetColumnWidth(3, 250)
        self.menu_items.SetColumnWidth(5, 250)

        # menu item fields
        self.id = wxTextCtrl(self, ID_ID)
        self.label = wxTextCtrl(self, LABEL_ID)
        self.name = wxTextCtrl(self, NAME_ID)
        self.help_str = wxTextCtrl(self, HELP_STR_ID)

        # ALB 2004-12-05
        self.event_handler = wxTextCtrl(self, -1)
        import re
        self.handler_re = re.compile(r'^\s*\w*\s*$')

        #self.checkable = wxCheckBox(self, CHECK_ID, "") #Checkable")
        self.check_radio = wxRadioBox(
            self, CHECK_RADIO_ID, "Type",
            choices=['Normal', 'Checkable', 'Radio'], majorDimension=3)

        self.add = wxButton(self, ADD_ID, "Add")
        self.remove = wxButton(self, REMOVE_ID, "Remove")
        self.add_sep = wxButton(self, ADD_SEP_ID, "Add separator")

        # menu items navigation
        self.move_up = wxButton(self, MOVE_UP_ID, "Up")
        self.move_down = wxButton(self, MOVE_DOWN_ID, "Down")
        self.move_left = wxButton(self, MOVE_LEFT_ID, " < ")
        self.move_right = wxButton(self, MOVE_RIGHT_ID, " > ")

        self.ok = wxButton(self, wxID_OK, "OK")
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
        # ALB 2004-12-05
        EVT_KILL_FOCUS(self.event_handler, self.update_menu_item)
        #EVT_CHECKBOX(self, CHECK_ID, self.update_menu_item)
        EVT_RADIOBOX(self, CHECK_RADIO_ID, self.update_menu_item)
        EVT_LIST_ITEM_SELECTED(self, LIST_ID, self.show_menu_item)
        if items:
            self.add_items(items)

    def do_layout(self):
        self.label.Enable(False)
        self.id.Enable(False)
        self.name.Enable(False)
        self.help_str.Enable(False)
        self.event_handler.Enable(False)
        self.check_radio.Enable(False)
        
        sizer = wxBoxSizer(wxVERTICAL)
        sizer2 = wxStaticBoxSizer(self._staticbox, wxVERTICAL)
        self.label.SetSize((150, -1))
        self.id.SetSize((150, -1))
        self.name.SetSize((150, -1))
        self.help_str.SetSize((150, -1))
        self.event_handler.SetSize((150, -1))
        szr = wxFlexGridSizer(0, 2)
        if misc.check_wx_version(2, 5, 2):
            flag = wxFIXED_MINSIZE
        else:
            flag = 0
        label_flag = wxALIGN_CENTER_VERTICAL
        szr.Add(wxStaticText(self, -1, "Id   "), flag=label_flag)
        szr.Add(self.id, flag=flag)
        szr.Add(wxStaticText(self, -1, "Label  "), flag=label_flag)
        szr.Add(self.label, flag=flag)
        szr.Add(wxStaticText(self, -1, "Name  "), flag=label_flag)
        szr.Add(self.name, flag=flag)
        szr.Add(wxStaticText(self, -1, "Help String  "), flag=label_flag)
        szr.Add(self.help_str, flag=flag)
        szr.Add(wxStaticText(self, -1, "Event Handler  "), flag=label_flag)
        szr.Add(self.event_handler, flag=flag)
        sizer2.Add(szr, 1, wxALL|wxEXPAND, 5)
        sizer2.Add(self.check_radio, 0, wxLEFT|wxRIGHT|wxBOTTOM, 4)
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

    def _enable_fields(self, enable=True):
        for s in (self.label, self.id, self.name, self.help_str,
                  self.check_radio, self.event_handler):
            s.Enable(enable)

    def add_menu_item(self, event):
        """\
        Event handler called when the Add button is clicked
        """
        index = self.selected_index = self.selected_index+1
        if not self.menu_items.GetItemCount():
            self._enable_fields()
##             for s in (self.label, self.id, self.name, self.help_str,
##                       self.check_radio, self.event_handler):
##                 s.Enable(True)
        if index < 0: index = self.menu_items.GetItemCount()
        elif index > 0: indent = "    " * self.item_level(index-1)
        else: indent = ""
        name, label, id, check_radio = "", "item", "", "0"
        self.menu_items.InsertStringItem(index, indent + label)
        self.menu_items.SetStringItem(index, 1, id)
        self.menu_items.SetStringItem(index, 2, name)
        self.menu_items.SetStringItem(index, 4, check_radio)
        # fix bug 698074
        self.menu_items.SetItemState(index, wxLIST_STATE_SELECTED,
                                     wxLIST_STATE_SELECTED)
        self.name.SetValue(name)
        self.label.SetValue(label)
        self.id.SetValue(id)
        self.check_radio.SetSelection(int(check_radio))
        self.event_handler.SetValue("")

    def add_separator(self, event):
        """\
        Event handler called when the Add Separator button is clicked
        """
        index = self.selected_index+1
        if not self.menu_items.GetItemCount():
            self._enable_fields()
##             for s in (self.label, self.id, self.name, self.help_str,
##                       self.check_radio, self.event_handler):
##                 s.Enable(True)
        if index < 0: index = self.menu_items.GetItemCount() 
        elif index > 0: label = "    " * self.item_level(index-1) + '---'
        else: label = '---'
        self.menu_items.InsertStringItem(index, label)
        self.menu_items.SetStringItem(index, 1, '---')
        self.menu_items.SetStringItem(index, 2, '---')
        # fix bug 698074
        self.menu_items.SetItemState(index, wxLIST_STATE_SELECTED,
                                     wxLIST_STATE_SELECTED)

    def show_menu_item(self, event):
        """\
        Event handler called when a menu item in the list is selected
        """        
        self.selected_index = index = event.GetIndex()
        if not misc.streq(self.menu_items.GetItem(index, 2).m_text, '---'):
            # skip if the selected item is a separator
            for (s, i) in ((self.label, 0), (self.id, 1), (self.name, 2),
                           (self.help_str, 3), (self.event_handler, 5)):
                s.SetValue(self.menu_items.GetItem(index, i).m_text)
            self.label.SetValue(self.label.GetValue().lstrip())
            try:
                self.check_radio.SetSelection(
                    int(self.menu_items.GetItem(index, 4).m_text))
            except:
                self.check_radio.SetSelection(0)
        event.Skip()

    def update_menu_item(self, event):
        """\
        Event handler called when some of the properties of the current menu
        item changes
        """        
        set_item = self.menu_items.SetStringItem
        index = self.selected_index
        val = self.event_handler.GetValue()
        if not self.handler_re.match(val):
            event.GetEventObject().SetFocus()
            return
        if index < 0:
            return event.Skip()
        set_item(index, 0, "    " * self.item_level(index) + \
                 self.label.GetValue().lstrip())
        set_item(index, 1, self.id.GetValue())
        set_item(index, 2, self.name.GetValue())
        set_item(index, 3, self.help_str.GetValue())
        set_item(index, 4, str(self.check_radio.GetSelection()))
        set_item(index, 5, self.event_handler.GetValue())
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
            for s in (self.name, self.id, self.label, self.help_str,
                      self.event_handler):
                s.SetValue("")
            self.check_radio.SetSelection(0)
            self.menu_items.DeleteItem(self.selected_index)
            if not self.menu_items.GetItemCount():
                self._enable_fields(False)
##                 for s in (self.name, self.id, self.label, \
##                           self.help_str, self.check_radio, self.event_handler):
##                     s.Enable(False)

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
            add_item(i, misc.wxstr(indent * level + node.label.lstrip()))
            set_item(i, 1, misc.wxstr(node.id))
            set_item(i, 2, misc.wxstr(node.name))
            set_item(i, 3, misc.wxstr(node.help_str))
            # ALB 2004-12-05
            set_item(i, 5, misc.wxstr(node.handler))
            
            item_type = 0
            try:
                if node.checkable and int(node.checkable):
                    item_type = 1
                elif int(node.radio):
                    item_type = 2
            except ValueError:
                pass
            set_item(i, 4, misc.wxstr(item_type))
            index[0] += 1
            for item in node.children:
                add(item, level+1)
        for tree in menus:
            add(tree.root, 0)
        if self.menu_items.GetItemCount():
            self._enable_fields()
##             for s in (self.name, self.id, self.label, \
##                       self.help_str, self.check_radio, self.event_handler):
##                 s.Enable(True)
            

    def get_menus(self):
        """\
        returns the contents of self.menu_items as a list of trees which
        describe the structure of the menus in the format used by EditMenuBar
        """
        def get(i, j): return self.menu_items.GetItem(i, j).m_text
        trees = []
        def add(node, index):
            label = get(index, 0).lstrip()
            id = get(index, 1)
            name = get(index, 2)
            help_str = get(index, 3)
            event_handler = get(index, 5)
            try:
                item_type = int(get(index, 4))
            except ValueError:
                item_type = 0
            checkable = item_type == 1 and misc.wxstr("1") or misc.wxstr("")
            radio = item_type == 2 and misc.wxstr("1") or misc.wxstr("")
            n = MenuTree.Node(label, id, name, help_str, checkable, radio,
                              handler=event_handler)
            node.children.append(n)
            n.parent = node
            return n
        level = 0
        curr_item = None
        for index in range(self.menu_items.GetItemCount()):
            label = get(index, 0)
            lvl = self.item_level(index) # get the indentation level
            if not lvl:
                t = MenuTree(get(index, 2), label, id=get(index, 1),
                             handler=get(index, 5))
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
            if misc.streq(label[:4], " " * 4):
                self.menu_items.SetStringItem(index, 0, label[4:])
                self.menu_items.SetItemState(index, wxLIST_STATE_SELECTED, 
                                             wxLIST_STATE_SELECTED)
                
    def move_item_left(self, event):
        """\
        moves the selected menu item one level up in the hierarchy, i.e.
        shifts its label 4 spaces left in self.menu_items
        """
        self.menu_items.SetFocus()
        self._move_item_left(self.selected_index)

    def _move_item_right(self, index):
        if index > 0 and (self.item_level(index) <= self.item_level(index-1)): 
            label = self.menu_items.GetItem(index, 0).m_text
            self.menu_items.SetStringItem(index, 0, misc.wxstr(" " * 4)
                                          + label)
            self.menu_items.SetItemState(index, wxLIST_STATE_SELECTED, \
                                         wxLIST_STATE_SELECTED)

    def move_item_right(self, event):
        """\
        moves the selected menu item one level down in the hierarchy, i.e.
        shifts its label 4 spaces right in self.menu_items
        """
        self.menu_items.SetFocus()
        self._move_item_right(self.selected_index)


    def move_item_up(self, event):
        """\
        moves the selected menu item before the previous one at the same level
        in self.menu_items
        """
        self.menu_items.SetFocus()
        index = self._do_move_item(event, self.selected_index, False)
        if index is not None:
            state = wxLIST_STATE_SELECTED | wxLIST_STATE_FOCUSED
            self.menu_items.SetItemState(index, state, state)

    def _do_move_item(self, event, index, is_down):
        """\
        internal function used by move_item_up and move_item_down.
        Returns the new index of the moved item, or None if no change occurred
        """
        #index = self.selected_index
        if index <= 0: return None
        def get(i, j): return self.menu_items.GetItem(i, j).m_text
        def getall(i): return [get(i, j) for j in range(6)]
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
            elif level > lvl: return None
            i -= 1
        delete = self.menu_items.DeleteItem
        insert = self.menu_items.InsertStringItem
        set = self.menu_items.SetStringItem
        for j in range(len(items_to_move)-1, -1, -1):
            delete(index+j)
        items_to_move.reverse()
        for label, id, name, help_str, check_radio, event_handler in \
                items_to_move:
            i = insert(i, label)
            set(i, 1, id)
            set(i, 2, name)
            set(i, 3, help_str)
            set(i, 4, check_radio)
            set(i, 5, event_handler)
        ret_idx = i
        if is_down: ret_idx += len(items_to_move)
        return ret_idx
        
    def move_item_down(self, event):
        """\
        moves the selected menu item after the next one at the same level
        in self.menu_items
        """
        self.menu_items.SetFocus()
        index = self.selected_index
        self.selected_index = -1
        if index < 0: return
        def get(i, j): return self.menu_items.GetItem(i, j).m_text
        def getall(i): return [get(i, j) for j in range(6)]
        level = self.item_level(index)
        i = index+1
        while i < self.menu_items.GetItemCount():
            # collect the items to move down
            if level < self.item_level(i):
                i += 1
            else: break
        if i < self.menu_items.GetItemCount():
            # _do_move_item works with selected_index, so we must assing to
            # it the rigth value before the call
            #self.selected_index = i
            self.selected_index = self._do_move_item(event, i, True)
            # fix bug 698071
            state = wxLIST_STATE_SELECTED | wxLIST_STATE_FOCUSED
            self.menu_items.SetItemState(self.selected_index, state, state)
        else:
            # restore the selected index
            self.selected_index = index
                                                  
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


class EditMenuBar(EditBase, PreviewMixin):
    __hidden_frame = None # used on GTK to reparent a menubar before deletion
    
    def __init__(self, name, klass, parent, property_window):
        custom_class = parent is None
        EditBase.__init__(self, name, klass,
                          parent, wxNewId(), property_window,
                          custom_class=custom_class, show=False)
        self.base = 'wxMenuBar'
        
        def nil(*args): return ()
        self.menus = [] # list of MenuTree objects
        self._mb = None # the real menubar
        self.access_functions['menus'] = (self.get_menus, self.set_menus)
        prop = self.properties['menus'] = MenuProperty(self, 'menus', None) 
##         self.node = Tree.Node(self)
##         common.app_tree.add(self.node, parent.node)
        PreviewMixin.__init__(self)

    def create_widget(self):
        if wxPlatform == '__WXGTK__' and not EditMenuBar.__hidden_frame:
            EditMenuBar.__hidden_frame = wxFrame(common.palette, -1, "")
            EditMenuBar.__hidden_frame.Hide()
        if self.parent:
            self.widget = self._mb = wxMenuBar()
            if self.parent.widget: self.parent.widget.SetMenuBar(self.widget)
            if wxPlatform == '__WXMSW__' or wxPlatform == '__WXMAC__':
                self.widget.SetFocus = lambda : None
        else:
            # "top-level" menubar
            self.widget = wxFrame(None, -1, misc.design_title(self.name))
            self.widget.SetClientSize((400, 30))
            self._mb = wxMenuBar()
            self.widget.SetMenuBar(self._mb)
            self.widget.SetBackgroundColour(self._mb.GetBackgroundColour())
            EVT_CLOSE(self.widget, lambda e: self.hide_widget())
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
        if self.parent is not None:
            self.property_window.Layout()
        else:
            PreviewMixin.create_properties(self)
        
    def __getitem__(self, key):
        return self.access_functions[key]

    def get_menus(self):
        return self.menus

    def set_menus(self, menus):
        self.menus = menus
        if not self._mb: return # nothing left to do
        for i in range(self._mb.GetMenuCount()):
            self._mb.Remove(0)
        def append(menu, items):
            for item in items:
                if misc.streq(item.name, '---'): # item is a separator
                    menu.AppendSeparator()
                elif item.children:
                    m = wxMenu()
                    append(m, item.children)
                    menu.AppendMenu(wxNewId(), misc.wxstr(item.label), m,
                                    misc.wxstr(item.help_str))
                else:
                    check_radio = 0
                    try:
                        if int(item.checkable):
                            check_radio = 1
                    except:
                        check_radio = 0
                    if not check_radio:
                        try:
                            if int(item.radio):
                                check_radio = 2
                        except:
                            check_radio = 0
                    menu.Append(wxNewId(), misc.wxstr(item.label),
                                misc.wxstr(item.help_str), check_radio)
        first = self._mb.GetMenuCount()
        for menu in self.menus:
            m = wxMenu()
            append(m, menu.root.children)
            if first:
                self._mb.Replace(0, m, misc.wxstr(menu.root.label))
                first = 0
            else: self._mb.Append(m, misc.wxstr(menu.root.label))
        self._mb.Refresh()
      
    def remove(self, *args, **kwds):
        if self.parent is not None:
            self.parent.properties['menubar'].set_value(0)
            if kwds.get('gtk_do_nothing', False) and wxPlatform == '__WXGTK__':
                # workaround to prevent some segfaults on GTK: unfortunately,
                # I'm not sure that this works in all cases, and moreover it
                # could probably leak some memory (but I'm not sure)
                self.widget = None
            else:
                if self.parent.widget:
                    if wxPlatform == '__WXGTK__' and \
                           not misc.check_wx_version(2, 5):
                        self.widget.Reparent(EditMenuBar.__hidden_frame)
                        self.widget.Hide()
                    self.parent.widget.SetMenuBar(None)
        else:
            if self.widget:
                self.widget.Destroy()
                self.widget = None
        EditBase.remove(self)

    def popup_menu(self, event):
        if self.parent is not None:
            return # do nothing in this case
        if self.widget:
            if not self._rmenu:
                REMOVE_ID, HIDE_ID = [wxNewId() for i in range(2)]
                self._rmenu = misc.wxGladePopupMenu(self.name)
                misc.append_item(self._rmenu, REMOVE_ID, 'Remove\tDel',
                                 'remove.xpm')
                misc.append_item(self._rmenu, HIDE_ID, 'Hide')
                def bind(method):
                    return lambda e: misc.wxCallAfter(method)
                EVT_MENU(self.widget, REMOVE_ID, bind(self.remove))
                EVT_MENU(self.widget, HIDE_ID, bind(self.hide_widget))
                
            self.widget.PopupMenu(self._rmenu, event.GetPosition())

    def hide_widget(self, *args):
        if self.widget and self.widget is not self._mb:
            self.widget.Hide()
            common.app_tree.expand(self.node, False)
            common.app_tree.select_item(self.node.parent)
            common.app_tree.app.show_properties()

##     def show_widget(self, yes):
##         EditBase.show_widget(self, yes)
##         if self._frame:
##             self._frame.Show(yes)

    def set_name(self, name):
        EditBase.set_name(self, name)
        if self.widget is not self._mb:
            self.widget.SetTitle(misc.design_title(misc.wxstr(self.name)))

    def get_property_handler(self, name):
        class MenuHandler:
            itemattrs = ['label', 'id', 'name', 'help_str',
                         'checkable', 'radio', 'handler']
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
                        t = MenuTree(attrs['name'], label,
                                     attrs.get('itemid', ''),
                                     attrs.get('help_str', ''),
                                     handler=attrs.get('handler', ''))
                        self.curr_menu.append( (t.root,) )
                        self.owner.menus.append(t)
                        return
                    node = MenuTree.Node(label=label, name=attrs['name'],
                                         id=attrs.get('itemid', ''),
                                         help_str=attrs.get('help_str', ''),
                                         handler=attrs.get('handler', ''))
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
                        # ignore unknown attributes...
                        self.curr_index = -1
                        pass
##                         from xml_parse import XmlParsingError
##                         raise XmlParsingError, "invalid menu item attribute"
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


def builder(parent, sizer, pos, number=[0]):
    """\
    factory function for EditMenuBar objects.
    """
    class Dialog(wxDialog):
        def __init__(self):
            wxDialog.__init__(self, None, -1, 'Select menubar class')
            if common.app_tree.app.get_language().lower() == 'xrc':
                self.klass = 'wxMenuBar'
            else:
                if not number[0]: self.klass = 'MyMenuBar'
                else: self.klass = 'MyMenuBar%s' % number[0]
                number[0] += 1
            klass_prop = TextProperty(self, 'class', self)
            szr = wxBoxSizer(wxVERTICAL)
            szr.Add(klass_prop.panel, 0, wxEXPAND)
            sz2 = wxBoxSizer(wxHORIZONTAL)
            sz2.Add(wxButton(self, wxID_OK, 'OK'), 0, wxALL, 3)
            sz2.Add(wxButton(self, wxID_CANCEL, 'Cancel'), 0, wxALL, 3)
            szr.Add(sz2, 0, wxALL|wxALIGN_CENTER, 3)
            self.SetAutoLayout(True)
            self.SetSizer(szr)
            szr.Fit(self)
            if self.GetBestSize()[0] < 150:
                self.SetSize((150, -1))

        def undo(self):
            if number[0] > 0:
                number[0] -= 1

        def __getitem__(self, value):
            if value == 'class':
                def set_klass(c): self.klass = c
                return (lambda : self.klass, set_klass)
    # end of inner class

    dialog = Dialog()
    if dialog.ShowModal() == wxID_CANCEL:
        # cancel the operation
        dialog.undo()
        dialog.Destroy()
        return
    
    name = 'menubar_%d' % (number[0] or 1)
    while common.app_tree.has_name(name):
        number[0] += 1
        name = 'menubar_%d' % number[0]

    mb = EditMenuBar(name, dialog.klass, parent, common.property_panel)
    mb.node = Tree.Node(mb)
    common.app_tree.add(mb.node)
    mb.show_widget(True)
    mb.show_properties()
    

def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    """\
    factory to build EditMenuBar objects from an xml file
    """
    name = attrs.get('name')
    if parent is not None:
        if name:
            parent.menubar.set_name(name)
            parent.menubar.name_prop.set_value(name)
        return parent.menubar
    else:
        mb = EditMenuBar(name, attrs.get('class', 'wxMenuBar'), None,
                         common.property_panel)
        mb.node = Tree.Node(mb)
        common.app_tree.add(mb.node)
        return mb


def initialize():
    """\
    initialization function for the module: returns a wxBitmapButton to be
    added to the main palette.
    """
    cwx = common.widgets_from_xml
    cwx['EditMenuBar'] = xml_builder
    common.widgets['EditMenuBar'] = builder
    
    return common.make_object_button('EditMenuBar', 'icons/menubar.xpm', 1)
