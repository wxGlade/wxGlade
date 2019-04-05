"""\
wxMenuBar objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016-2019 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx

import common, compat, config, misc
from MenuTree import *
from tree import Node
from wcodegen.taghandler import BaseXmlBuilderTagHandler
import new_properties as np
from edit_windows import EditBase, PreviewMixin


class MenuItemDialog(wx.Dialog):
    def __init__(self, parent, owner, items=None):
        style = wx.DEFAULT_DIALOG_STYLE|wx.RESIZE_BORDER|wx.WANTS_CHARS
        wx.Dialog.__init__(self, parent, -1, _("Menu editor"), style=style)

        # menu item fields
        self.label         = wx.TextCtrl(self, wx.ID_ANY, "")
        self.event_handler = wx.TextCtrl(self, wx.ID_ANY, "")
        self.name          = wx.TextCtrl(self, wx.ID_ANY, "")
        self.help_str      = wx.TextCtrl(self, wx.ID_ANY, "")
        self.id            = wx.TextCtrl(self, wx.ID_ANY, "")
        # radio box for type
        self.check_radio = wx.RadioBox(self, wx.ID_ANY, "Type", choices=["Normal", "Checkable", "Radio"],
                                       majorDimension=1, style=wx.RA_SPECIFY_COLS)
        self.check_radio.SetSelection(0)
        # dialog action buttons; these will be handled, instead of using stock OK/Cancel buttons
        self.ok     = wx.Button(self, wx.ID_ANY, "OK")
        self.cancel = wx.Button(self, wx.ID_ANY, "Cancel")
        # editor action buttons
        self.move_left  = wx.Button(self, wx.ID_ANY, "&<")
        self.move_right = wx.Button(self, wx.ID_ANY, "&>")
        self.move_up    = wx.Button(self, wx.ID_ANY, "&Up")
        self.move_down  = wx.Button(self, wx.ID_ANY, "&Down")
        self.add     = wx.Button(self, wx.ID_ANY, "&Add")
        self.remove  = wx.Button(self, wx.ID_ANY, "&Remove")
        self.add_sep = wx.Button(self, wx.ID_ANY, "Add &Separator")
        self.menu_items = wx.ListCtrl(self, wx.ID_ANY, style=wx.BORDER_DEFAULT | wx.BORDER_SUNKEN | wx.LC_EDIT_LABELS |
                                                         wx.LC_REPORT | wx.LC_SINGLE_SEL | wx.NO_FULL_REPAINT_ON_RESIZE)
        self.SetTitle("Menu Editor")
        self.__do_layout()

        self.Bind(wx.EVT_TEXT, self.on_label_edited, self.label)
        self.Bind(wx.EVT_TEXT, self.on_event_handler_edited, self.event_handler)
        self.Bind(wx.EVT_TEXT, self.on_name_edited, self.name)
        self.Bind(wx.EVT_TEXT, self.on_help_str_edited, self.help_str)
        self.Bind(wx.EVT_TEXT, self.on_id_edited, self.id)
        self.Bind(wx.EVT_RADIOBOX, self.on_type_edited, self.check_radio)

        self.Bind(wx.EVT_BUTTON, self.move_item_left, self.move_left)
        self.Bind(wx.EVT_BUTTON, self.move_item_right, self.move_right)
        self.Bind(wx.EVT_BUTTON, self.move_item_up, self.move_up)
        self.Bind(wx.EVT_BUTTON, self.move_item_down, self.move_down)
        self.Bind(wx.EVT_BUTTON, self.add_item, self.add)
        self.Bind(wx.EVT_BUTTON, self.remove_item, self.remove)
        self.Bind(wx.EVT_BUTTON, self.add_separator, self.add_sep)
        self.Bind(wx.EVT_BUTTON, self.on_cancel, self.cancel)
        self.Bind(wx.EVT_BUTTON, self.on_OK, self.ok)
        self.Bind(wx.EVT_LIST_ITEM_SELECTED, self.show_item, self.menu_items)

        self.Bind(wx.EVT_CHAR_HOOK, self.on_char)
        self.remove.Bind(wx.EVT_CHAR_HOOK, self.on_button_char)  # to ignore the Enter key while the focus is on Remove

        self.owner = owner

        # ALB 2004-09-26: workaround to make the scroll wheel work...
        self.menu_items.Bind(wx.EVT_MOUSEWHEEL, lambda e: e.Skip())

        self.menu_items.InsertColumn(0, _("Label"))
        self.menu_items.InsertColumn(1, _("Event Handler"))
        self.menu_items.InsertColumn(2, _("Name"))
        self.menu_items.InsertColumn(3, _("Type"))
        self.menu_items.InsertColumn(4, _("Help String"))
        self.menu_items.InsertColumn(5, _("Id"))

        self.menu_items.SetColumnWidth(0, 180)
        self.menu_items.SetColumnWidth(1, 180)
        self.menu_items.SetColumnWidth(2, 120)
        self.menu_items.SetColumnWidth(3, 35)
        self.menu_items.SetColumnWidth(4, 250)
        self.menu_items.SetColumnWidth(5, 50)
        self.SetSize( (900, 600) )

        import re
        self.handler_re = self.name_re = re.compile(r'^[a-zA-Z_]+[\w-]*(\[\w*\])*$')

        self.selected_index = -1  # index of the selected element in the wx.ListCtrl menu_items
        self._ignore_events = False
        self._last_focus = None
        if items:
            self.add_items(items)
            self._select_item(0)

    def on_char(self, event):
        # keyboard navigation: up/down arrows
        focus = self.FindFocus()
        if focus is self.check_radio:
            event.Skip()
            return
        if isinstance(focus, wx.Button):
            self.label.SetFocus()
        elif isinstance(focus, wx.TextCtrl):
            self._last_focus = focus
        k = event.GetKeyCode()
        if k==wx.WXK_RETURN:  # ignore Enter key
            return
        if k==wx.WXK_DOWN:
            if event.AltDown():
                self.move_item_down(event)
            else:
                self._select_item(self.selected_index+1)
            return
        if k==wx.WXK_UP:
            if event.AltDown():
                self.move_item_up(event)
            else:
                self._select_item(self.selected_index-1)
            return
        if k==wx.WXK_RIGHT and event.AltDown():
            self.move_item_right(event)
            return
        if k==wx.WXK_LEFT and event.AltDown():
            self.move_item_left(event)
            return
        event.Skip()

    def on_button_char(self, event):
        # for e.g. the Remove button we don't want an action on the Return button
        if event.GetKeyCode() != wx.WXK_RETURN:
            event.Skip()

    def __do_layout(self):
        sizer_1 = wx.BoxSizer(wx.VERTICAL)
        sizer_2 = wx.BoxSizer(wx.HORIZONTAL)
        sizer_5 = wx.BoxSizer(wx.HORIZONTAL)
        sizer_6 = wx.BoxSizer(wx.VERTICAL)
        grid_sizer_2 = wx.FlexGridSizer(5, 2, 0, 0)
        label_6 = wx.StaticText(self, wx.ID_ANY, "Label:")
        grid_sizer_2.Add(label_6, 0, wx.ALIGN_CENTER_VERTICAL | wx.LEFT | wx.RIGHT, 4)
        grid_sizer_2.Add(self.label, 1, wx.EXPAND, 0)
        label_7 = wx.StaticText(self, wx.ID_ANY, "Event Handler:")
        grid_sizer_2.Add(label_7, 0, wx.ALIGN_CENTER_VERTICAL | wx.LEFT | wx.RIGHT, 4)
        grid_sizer_2.Add(self.event_handler, 1, wx.EXPAND, 0)
        label_8 = wx.StaticText(self, wx.ID_ANY, "(Attribute) Name:")
        grid_sizer_2.Add(label_8, 0, wx.ALIGN_CENTER_VERTICAL | wx.LEFT | wx.RIGHT, 4)
        grid_sizer_2.Add(self.name, 1, wx.EXPAND, 0)
        label_9 = wx.StaticText(self, wx.ID_ANY, "Help String:")
        grid_sizer_2.Add(label_9, 0, wx.ALIGN_CENTER_VERTICAL | wx.LEFT | wx.RIGHT, 4)
        grid_sizer_2.Add(self.help_str, 1, wx.EXPAND, 0)
        label_10 = wx.StaticText(self, wx.ID_ANY, "ID:")
        grid_sizer_2.Add(label_10, 0, wx.ALIGN_CENTER_VERTICAL | wx.LEFT | wx.RIGHT, 4)
        grid_sizer_2.Add(self.id, 0, 0, 0)
        grid_sizer_2.AddGrowableCol(1)
        sizer_5.Add(grid_sizer_2, 2, wx.EXPAND, 0)
        sizer_5.Add(self.check_radio, 0, wx.ALL | wx.EXPAND, 4)
        sizer_5.Add((20, 20), 1, wx.ALIGN_CENTER_VERTICAL | wx.EXPAND, 0)
        sizer_6.Add(self.ok, 0, wx.ALL, 5)
        sizer_6.Add(self.cancel, 0, wx.ALL, 5)
        sizer_5.Add(sizer_6, 0, wx.EXPAND, 0)
        sizer_1.Add(sizer_5, 0, wx.EXPAND, 0)
        sizer_2.Add(self.move_left, 0, wx.BOTTOM | wx.LEFT | wx.TOP, 8)
        sizer_2.Add(self.move_right, 0, wx.BOTTOM | wx.RIGHT | wx.TOP, 8)
        sizer_2.Add(self.move_up, 0, wx.BOTTOM | wx.LEFT | wx.TOP, 8)
        sizer_2.Add(self.move_down, 0, wx.BOTTOM | wx.RIGHT | wx.TOP, 8)
        sizer_2.Add((20, 20), 1, wx.ALIGN_CENTER_VERTICAL, 0)
        sizer_2.Add(self.add, 0, wx.BOTTOM | wx.LEFT | wx.TOP, 8)
        sizer_2.Add(self.remove, 0, wx.BOTTOM | wx.TOP, 8)
        sizer_2.Add(self.add_sep, 0, wx.ALL, 8)
        sizer_2.Add((20, 20), 2, wx.ALIGN_CENTER_VERTICAL, 0)
        sizer_1.Add(sizer_2, 0, wx.EXPAND, 0)
        sizer_1.Add(self.menu_items, 1, wx.EXPAND, 0)
        self.SetSizer(sizer_1)
        sizer_1.Fit(self)
        sizer_1.SetSizeHints(self)
        self.Layout()

        # set tooltips
        for c in (label_6, self.label):
            compat.SetToolTip(c, "The menu entry text;\nenter & for access keys (using ALT key)\nappend e.g. \\tCtrl-X for keyboard shortcut")
        for c in (label_7, self.event_handler):
            compat.SetToolTip(c, "Enter the name of an event handler method; this will be created as stub")
        for c in (label_8, self.name):
            compat.SetToolTip(c, "optional: enter a name to store the menu item as attribute of the menu bar")
        for c in (label_10, self.id):
            compat.SetToolTip(c, "optional: enter wx ID")
        compat.SetToolTip( self.move_up, "Move selected item up" )
        compat.SetToolTip( self.move_down, "Move selected item down" )
        compat.SetToolTip( self.menu_items, "For navigation use the mouse or the up/down arrows" )
        compat.SetToolTip( self.move_left,  "Move the selected item up by one menu level" )
        compat.SetToolTip( self.move_right, "Move the selected item down by one menu level" )

    def _enable_fields(self, enable=True):
        for s in (self.event_handler, self.id, self.name, self.help_str, self.check_radio, self.label):
            s.Enable(enable)

    def add_item(self, event):
        "Event handler called when the Add button is clicked"
        index = self.selected_index = self.selected_index + 1
        indent = ""
        if not self.menu_items.GetItemCount():
            self._enable_fields()
        if index < 0:
            index = self.menu_items.GetItemCount()
        elif index > 0:
            indent = "    " * self.item_level(index-1)
        name, label, id, check_radio = "", "item", "", "0"
        self.menu_items.InsertStringItem(index, indent + label)
        self.menu_items.SetStringItem(index, 2, name)
        self.menu_items.SetStringItem(index, 3, check_radio)
        self.menu_items.SetStringItem(index, 5, id)
        # fix bug 698074
        self.menu_items.SetItemState(index, wx.LIST_STATE_SELECTED, wx.LIST_STATE_SELECTED)
        self._select_item(index, force=True)

    def add_separator(self, event):
        "Event handler called when the Add Separator button is clicked"
        index = self.selected_index + 1
        label = '---'
        if not self.menu_items.GetItemCount():
            self._enable_fields()
        if index < 0:
            index = self.menu_items.GetItemCount()
        elif index > 0:
            label = "    " * self.item_level(index-1) + '---'
        self.menu_items.InsertStringItem(index, label)
        self.menu_items.SetStringItem(index, 2, '---')  # name
        self.menu_items.SetStringItem(index, 5, '---')  # id
        # fix bug 698074
        self.menu_items.SetItemState(index, wx.LIST_STATE_SELECTED, wx.LIST_STATE_SELECTED)

    def show_item(self, event):
        "Event handler called when a menu item in the list is selected"
        if not self._ignore_events:
            self._select_item(event.GetIndex())
        event.Skip()

    def _select_item(self, index, force=False):
        if index >= self.menu_items.GetItemCount() or index<0 or (index==self.selected_index and not force): return
        self._ignore_events = True
        self.menu_items.Select(index)
        self.selected_index = index
        if self.menu_items.GetItem(index, 2).GetText() != '---':
            # skip if the selected item is a separator
            for (s, i) in ((self.label, 0), (self.event_handler, 1), (self.name, 2), (self.help_str, 4), (self.id, 5)):
                # at this point, the value should be validated already
                s.SetBackgroundColour( compat.wx_SystemSettings_GetColour(wx.SYS_COLOUR_WINDOW) )
                s.SetValue(self.menu_items.GetItem(index, i).GetText())
            self.label.SetValue(self.label.GetValue().lstrip())
            try:
                self.check_radio.SetSelection( int(self.menu_items.GetItem(index, 3).GetText()) )
            except:
                self.check_radio.SetSelection(0)
            self._enable_fields(True)
            # set focus to text field again
            focus = self.FindFocus()
            if not isinstance(focus, wx.TextCtrl) and isinstance(self._last_focus, wx.TextCtrl):
                self._last_focus.SetFocus()
        else:
            for c in (self.label, self.event_handler, self.name, self.help_str, self.id):
                c.SetValue("")
            self._enable_fields(False)
        self._enable_buttons()
        if force:
            self.label.SetFocus()
            self.label.SelectAll()

    def _enable_buttons(self):
        # activate the left/right/up/down buttons
        index = self.selected_index
        item_level = self.item_level(index)
        item_count = self.menu_items.GetItemCount()
        self.move_left.Enable( not (index+1<item_count and (item_level < self.item_level(index+1)) ))
        self.move_right.Enable( index>=1 and item_level <= self.item_level(index-1) )
        self.move_up.Enable( index>0 )
        self.move_down.Enable( index<item_count-1 )
        self._ignore_events = False

    def on_label_edited(self, event):
        if not self._ignore_events:
            value = "    " * self.item_level(self.selected_index) + self.label.GetValue().lstrip()
            self.menu_items.SetStringItem(self.selected_index, 0, value)
        event.Skip()

    def on_event_handler_edited(self, event):
        value = self.event_handler.GetValue()
        if not value or self.handler_re.match(value):
            self.event_handler.SetBackgroundColour( compat.wx_SystemSettings_GetColour(wx.SYS_COLOUR_WINDOW) )
            valid = True
        else:
            self.event_handler.SetBackgroundColour(wx.RED)
            valid = False
        self.event_handler.Refresh()
        if valid and not self._ignore_events:
            self.menu_items.SetStringItem(self.selected_index, 1, value)
        event.Skip()

    def on_name_edited(self, event):
        value = self.name.GetValue()
        if not value or self.name_re.match(value):
            self.name.SetBackgroundColour( compat.wx_SystemSettings_GetColour(wx.SYS_COLOUR_WINDOW) )
            valid = True
        else:
            self.name.SetBackgroundColour(wx.RED)
            valid = False
        if value and valid and not self._ignore_events:
            # check for double names
            for i in range(self.menu_items.GetItemCount()):
                if i==self.selected_index: continue
                if value == self.menu_items.GetItem(i, 2).GetText():
                    valid = False
                    self.name.SetBackgroundColour( wx.Colour(255, 255, 0, 255) )  # YELLOW
                    break
        self.name.Refresh()
        if valid and not self._ignore_events:
            self.menu_items.SetStringItem(self.selected_index, 2, value)
        event.Skip()

    def on_type_edited(self, event):
        if not self._ignore_events:
            self.menu_items.SetStringItem(self.selected_index, 3, str(self.check_radio.GetSelection()))
        event.Skip()

    def on_help_str_edited(self, event):
        if not self._ignore_events:
            self.menu_items.SetStringItem(self.selected_index, 4, self.help_str.GetValue())
        event.Skip()

    def on_id_edited(self, event):
        if not self._ignore_events:
            self.menu_items.SetStringItem(self.selected_index, 5, self.id.GetValue())
        event.Skip()

    def item_level(self, index, label=None):
        "returns the indentation level of the menu item at the given index"
        label = self.menu_items.GetItem(index, 0).GetText()
        return (len(label) - len(label.lstrip())) // 4

    def remove_item(self, event):
        "Event handler called when the Remove button is clicked"
        if self.selected_index < 0: return
        index = self.selected_index+1
        if index<self.menu_items.GetItemCount() and (self.item_level(self.selected_index) < self.item_level(index)):
            # the item to be deleted is parent to the following item -> move up the following item
            self._move_item_left(index)
            #self.selected_index = index-1
        for s in (self.name, self.id, self.label, self.help_str, self.event_handler):
            s.SetValue("")
        self.check_radio.SetSelection(0)
        self.menu_items.DeleteItem(self.selected_index)
        if not self.menu_items.GetItemCount():
            self._enable_fields(False)
        self.selected_index -= 1
        self.menu_items.Select(self.selected_index)

    def add_items(self, menus):
        """adds the content of 'menus' to self.menu_items. menus is a sequence of
        trees which describes the structure of the menus"""
        indent = " " * 4
        if compat.IS_CLASSIC:
            set_item = self.menu_items.SetStringItem
            add_item = self.menu_items.InsertStringItem
        else:
            set_item = self.menu_items.SetItem
            add_item = self.menu_items.InsertItem
        index = [0]

        def add(node, level):
            i = index[0]
            add_item(i, misc.wxstr(indent * level + node.label.lstrip().replace("\t","\\t")))
            set_item(i, 1, misc.wxstr(node.handler))
            set_item(i, 2, misc.wxstr(node.name))
            set_item(i, 4, misc.wxstr(node.help_str))
            set_item(i, 5, misc.wxstr(node.id))
            if node.label==node.name==node.id=='---':
                set_item(i, 3, '')
            else:
                item_type = 0
                try:
                    if node.checkable and int(node.checkable):
                        item_type = 1
                    elif int(node.radio):
                        item_type = 2
                except ValueError:
                    pass
                set_item(i, 3, misc.wxstr(item_type))
            index[0] += 1
            for item in node.children:
                add(item, level+1)
        for tree in menus:
            add(tree.root, 0)
        if self.menu_items.GetItemCount():
            self._enable_fields()

    def get_menus(self):
        """returns the contents of self.menu_items as a list of trees which
        describe the structure of the menus in the format used by EditMenuBar"""
        #def get(i, j): return self.menu_items.GetItem(i, j).GetText()
        def get(i, j): return self.menu_items.GetItem(i, j).GetText()
        trees = []

        def add(node, index):
            label = get(index, 0).lstrip().replace("\\t", "\t")
            id = get(index, 5)
            name = get(index, 2)
            help_str = get(index, 4)
            event_handler = get(index, 1)
            try:
                item_type = int(get(index, 3))
            except ValueError:
                item_type = 0
            checkable = item_type == 1 and misc.wxstr("1") or misc.wxstr("")
            radio = item_type == 2 and misc.wxstr("1") or misc.wxstr("")
            n = MenuTree.Node(label, id, name, help_str, checkable, radio, handler=event_handler)
            node.children.append(n)
            n.parent = node
            return n
        level = 0
        curr_item = None
        for index in range(self.menu_items.GetItemCount()):
            label = get(index, 0).replace("\\t", "\t")
            lvl = self.item_level(index)  # get the indentation level
            if not lvl:
                t = MenuTree( get(index, 2), label, id=get(index, 5), handler=get(index, 1) )
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
            if ( index+1 < self.menu_items.GetItemCount() and (self.item_level(index) < self.item_level(index+1)) ):
                return
            label = self.menu_items.GetItem(index, 0).GetText()
            if label[:4] == "    ":
                self.menu_items.SetStringItem(index, 0, label[4:])
                self.menu_items.SetItemState(index, wx.LIST_STATE_SELECTED, wx.LIST_STATE_SELECTED)
            self._enable_buttons()

    def move_item_left(self, event):
        """moves the selected menu item one level up in the hierarchy, i.e.
        shifts its label 4 spaces left in self.menu_items"""
        self.menu_items.SetFocus()
        self._move_item_left(self.selected_index)

    def _move_item_right(self, index):
        if index > 0 and (self.item_level(index) <= self.item_level(index-1)):
            label = self.menu_items.GetItem(index, 0).GetText()
            self.menu_items.SetStringItem(index, 0, misc.wxstr(" "*4) + label)
            self.menu_items.SetItemState(index, wx.LIST_STATE_SELECTED, wx.LIST_STATE_SELECTED)
            self._enable_buttons()

    def move_item_right(self, event):
        """moves the selected menu item one level down in the hierarchy, i.e.
        shifts its label 4 spaces right in self.menu_items"""
        self.menu_items.SetFocus()
        self._move_item_right(self.selected_index)

    def move_item_up(self, event):
        "moves the selected menu item before the previous one at the same level in self.menu_items"
        self.menu_items.SetFocus()
        index = self._do_move_item(event, self.selected_index, False)
        if index is not None:
            state = wx.LIST_STATE_SELECTED | wx.LIST_STATE_FOCUSED
            self.menu_items.SetItemState(index, state, state)

    def _do_move_item(self, event, index, is_down):
        """internal function used by move_item_up and move_item_down.
        Returns the new index of the moved item, or None if no change occurred"""
        #index = self.selected_index
        if index <= 0: return None

        def get(i, j): return self.menu_items.GetItem(i, j).GetText()

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
        for label, id, name, help_str, check_radio, event_handler in items_to_move:
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
        "moves the selected menu item after the next one at the same level in self.menu_items"
        self.menu_items.SetFocus()
        index = self.selected_index
        self.selected_index = -1
        if index < 0: return

        def get(i, j): return self.menu_items.GetItem(i, j).GetText()

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
            # it the right value before the call
            #self.selected_index = i
            self.selected_index = self._do_move_item(event, i, True)
            # fix bug 698071
            state = wx.LIST_STATE_SELECTED | wx.LIST_STATE_FOCUSED
            self.menu_items.SetItemState(self.selected_index, state, state)
        else:
            # restore the selected index
            self.selected_index = index

    # the action buttons are not linked to ESC and Enter to avoid accidental modifications
    def on_cancel(self, event):
        self.EndModal(wx.ID_CANCEL)
    def on_OK(self, event):
        self.EndModal(wx.ID_OK)



class MenuProperty(np.Property):
    "Property to edit the menus of an EditMenuBar instance"

    def __init__(self):
        np.Property.__init__(self, [])
        #self.menu_items = {}

    def create_editor(self, panel, sizer):
        self.edit_btn = wx.Button(panel, -1, _("Edit menus..."))
        sizer.Add(self.edit_btn, 0, wx.EXPAND|wx.TOP|wx.LEFT|wx.RIGHT, 4)
        self.edit_btn.Bind(wx.EVT_BUTTON, self.edit_menus)

    def edit_menus(self, event=None):
        if hasattr(self, "edit_btn") and self.edit_btn:
            parent = self.edit_btn.GetTopLevelParent()
        elif self.owner.widget:
            parent = self.owner.widget.GetTopLevelParent()
        else:
            parent = None
        dialog = MenuItemDialog( parent, self.owner, items=self.value )
        if not self.value: dialog.add_item(None)
        if dialog.ShowModal() == wx.ID_OK:
            self.on_value_edited(dialog.get_menus())
        dialog.Destroy()

    def write(self, output, tabs):
        inner_xml = []
        for menu in self.get():
            menu.write(inner_xml, tabs+1)
        output.extend( common.format_xml_tag( u'menus', inner_xml, tabs, is_xml=True ) )


class MenuHandler(BaseXmlBuilderTagHandler):
    itemattrs = ['label', 'id', 'name', 'help_str', 'checkable', 'radio', 'handler']

    def __init__(self, owner):
        super(MenuHandler, self).__init__()
        self.owner = owner
        self.menus = []
        self.curr_menu = []
        self.curr_item = None
        self.curr_index = 0
        self.menu_depth = 0

    def start_elem(self, name, attrs):
        if name == 'menus': return
        if name == 'menu':
            self.menu_depth += 1
            if self.menu_depth == 1:
                t = MenuTree(attrs['name'],
                             attrs['label'],
                             attrs.get('itemid', ''),
                             attrs.get('help_str', ''),
                             handler=attrs.get('handler', ''))
                self.curr_menu.append( (t.root,) )
                #self.owner.menus.append(t)
                #self.owner.properties["menus"].value.append(t)
                self.menus.append(t)
                return
            node = MenuTree.Node(label=attrs['label'],
                                 name=attrs['name'],
                                 id=attrs.get('itemid', ''),
                                 help_str=attrs.get('help_str', ''),
                                 handler=attrs.get('handler', ''))
            cm = self.curr_menu[-1]
            cm[0].children.append(node)
            node.parent = cm[0]
            menu = wx.Menu()
            self.curr_menu.append( (node, menu) )
        elif name == 'item':
            self.curr_item = MenuTree.Node()
        else:
            try: self.curr_index = self.itemattrs.index(name)
            except ValueError:
                # ignore unknown attributes...
                self.curr_index = -1
                pass

    def end_elem(self, name):
        if name == 'item':
            if self.curr_item.handler == self.curr_item.name == self.curr_item.label == '---' and not self.curr_item.id:
                # fix bug from 0.8 where separators were created with handler '---' instead of id
                self.curr_item.id = '---'
                self.curr_item.handler = ''
            try: cm = self.curr_menu[-1]
            except IndexError:
                from xml_parse import XmlParsingError
                raise XmlParsingError(_("menu item outside a menu"))
            cm[0].children.append(self.curr_item)
            self.curr_item.parent = cm[0]
        elif name == 'menu':
            self.menu_depth -= 1
            self.curr_menu.pop()
        elif name == 'menus':
            #self.owner.set_menus(self.owner.menus)
            #self.owner.properties["menus"].set(self.owner.menus)
            self.owner.properties["menus"].set(self.menus)
            self.owner.properties_changed(["menus"])
            return True

    def char_data(self, data):
        super(MenuHandler, self).char_data(data)
        char_data = self.get_char_data()
        setattr(self.curr_item, self.itemattrs[self.curr_index], char_data)



class EditMenuBar(EditBase, PreviewMixin):
    __hidden_frame = None  # used on GTK to reparent a menubar before deletion

    _PROPERTIES = ["menus", "preview"]
    PROPERTIES = EditBase.PROPERTIES + _PROPERTIES + EditBase.EXTRA_PROPERTIES

    def __init__(self, name, klass, parent):
        custom_class = parent is None
        EditBase.__init__(self, name, klass, parent, wx.NewId(), custom_class=custom_class)
        self.base = 'wxMenuBar'

        self.menus = MenuProperty()
        self.window_id = None  # just a dummy for code generation

        self._mb = None  # the real menubar
        if not self.parent:
            PreviewMixin.__init__(self)  # add a preview button
            self._is_toplevel = True
        else:
            self.preview = None
            self._is_toplevel = False

    def create_widget(self):
        if wx.Platform == '__WXGTK__' and not EditMenuBar.__hidden_frame:
            EditMenuBar.__hidden_frame = wx.Frame(common.main, -1, "")
            EditMenuBar.__hidden_frame.Hide()
        if self.parent:
            self.widget = self._mb = wx.MenuBar()
            if self.parent.widget: self.parent.widget.SetMenuBar(self.widget)
            if wx.Platform == '__WXMSW__' or wx.Platform == '__WXMAC__':
                self.widget.SetFocus = lambda : None
        else:
            # "top-level" menubar
            self.widget = wx.Frame(None, -1, misc.design_title(self.name))
            self.widget.SetClientSize((400, 30))
            self._mb = wx.MenuBar()
            self.widget.SetMenuBar(self._mb)
            self.widget.SetBackgroundColour(self._mb.GetBackgroundColour())
            import os
            icon = compat.wx_EmptyIcon()
            xpm = os.path.join(config.icons_path, 'menubar.xpm')
            icon.CopyFromBitmap(misc.get_xpm_bitmap(xpm))
            self.widget.SetIcon(icon)
            self.widget.Bind(wx.EVT_CLOSE, lambda e: self.hide_widget())
        self.widget.Bind(wx.EVT_LEFT_DOWN, self.on_set_focus)
        self.set_menus()  # show the menus

    def set_menus(self):
        if not self._mb: return  # nothing left to do
        for i in range(self._mb.GetMenuCount()):
            self._mb.Remove(0)

        def append(menu, items):
            for item in items:
                if item.name == '---':  # item is a separator
                    menu.AppendSeparator()
                elif item.children:
                    m = wx.Menu()
                    append(m, item.children)
                    menu.AppendMenu( wx.NewId(), misc.wxstr(item.label), m, misc.wxstr(item.help_str) )
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
                    menu.Append( wx.NewId(), misc.wxstr(item.label), misc.wxstr(item.help_str), check_radio )
        first = self._mb.GetMenuCount()
        for menu in self.menus or []:
            m = wx.Menu()
            append(m, menu.root.children)
            if first:
                self._mb.Replace(0, m, misc.wxstr(menu.root.label))
                first = 0
            else: self._mb.Append(m, misc.wxstr(menu.root.label))
        self._mb.Refresh()

    def remove(self, *args, **kwds):
        if self.parent is not None:
            self.parent.properties['menubar'].set(False)
            self.parent._menubar = None
            if kwds.get('gtk_do_nothing', False) and wx.Platform == '__WXGTK__':
                # workaround to prevent some segfaults on GTK: unfortunately,
                # I'm not sure that this works in all cases, and moreover it
                # could probably leak some memory (but I'm not sure)
                self.widget = None
            else:
                if self.parent.widget:
                    self.parent.widget.SetMenuBar(None)
        else:
            if self.widget:
                compat.DestroyLater(self.widget)
                self.widget = None
        EditBase.remove(self)

    def popup_menu(self, event, pos=None):
        if self.parent is not None:
            return  # do nothing in this case
        super(EditMenuBar, self).popup_menu(event, pos)

    def _create_popup_menu(self, widget=None):
        if widget is None: widget = self.widget
        menu = misc.wxGladePopupMenu(self.name)

        if self.widget and self.is_visible():
            item = misc.append_menu_item(menu, -1, _('Hide'))
            misc.bind_menu_item_after(widget, item, self.hide_widget)
        else:
            i = misc.append_menu_item(menu, -1, _('Show'))
            misc.bind_menu_item_after(widget, i, common.app_tree.show_toplevel, None, self)
        menu.AppendSeparator()

        item = misc.append_menu_item(menu, -1, _('Remove MenuBar\tDel'), wx.ART_DELETE)
        misc.bind_menu_item_after(widget, item, self.remove)

        item = misc.append_menu_item(menu, -1, _('Edit menus ...'))
        misc.bind_menu_item_after(widget, item, self.properties["menus"].edit_menus)

        return menu

    def hide_widget(self, *args):
        if self.widget and self.widget is not self._mb:
            self.widget.Hide()
            common.app_tree.expand(self.node, False)
            common.app_tree.select_item(self.node.parent)

    def set_name(self, name):
        EditBase.set_name(self, name)
        if self.widget is not self._mb:
            self.widget.SetTitle(misc.design_title(misc.wxstr(self.name)))

    def get_property_handler(self, name):
        if name == 'menus':
            return MenuHandler(self)
        return None

    def properties_changed(self, modified):
        if not modified or "menus" in modified:
            self.set_menus()
        EditBase.properties_changed(self, modified)

    def check_compatibility(self, widget, typename=None, report=False):
        return (False,"No pasting possible here.")
    def check_drop_compatibility(self):
        return (False,"Use menu editor: Properties -> Edit menus...")



def builder(parent, sizer, pos, number=[0]):
    "factory function for EditMenuBar objects"
    import window_dialog as wd
    klass = 'wxMenuBar' if common.app_tree.app.language.lower()=='xrc' else 'MyMenuBar'

    # if e.g. on a frame, suggest the user to add the menu bar to this
    toplevel_widget = None
    if misc.focused_widget is not None and misc.focused_widget.node.parent:
        toplevel_widget = common.app_tree._find_toplevel(misc.focused_widget.node).widget
        if not "menubar" in toplevel_widget.properties:
            toplevel_widget = None
    if toplevel_widget is not None:
        dialog = wd.StandaloneOrChildDialog(klass, "Select menubar type and class", toplevel_widget, "menubar")
    else:
        dialog = wd.WindowDialog(klass, None, 'Select standalone menubar class', True)

    klass = dialog.show()
    dialog.Destroy()
    if klass is None: return
    if klass is True:
        # add to toplevel widget
        toplevel_widget.properties["menubar"].set(True, notify=True)
        return
    name = dialog.get_next_name("menubar")
    with parent and parent.frozen() or misc.dummy_contextmanager():
        mb = EditMenuBar(name, klass, parent)
        mb.node = Node(mb)
        common.app_tree.add(mb.node)
        mb.create()
        mb.widget.Show()



def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    "factory to build EditMenuBar objects from a XML file"
    name = attrs.get('name')
    if parent is not None:
        if name:
            parent._menubar.properties["name"].set(name)
            parent._menubar.properties_changed(["name"])
        return parent._menubar
    else:
        mb = EditMenuBar(name, attrs.get('class', 'wxMenuBar'), None)
        mb.node = Node(mb)
        common.app_tree.add(mb.node)
        return mb


def initialize():
    "initialization function for the module: returns a wx.BitmapButton to be added to the main palette"
    cwx = common.widgets_from_xml
    cwx['EditMenuBar'] = xml_builder
    common.widgets['EditMenuBar'] = builder

    return common.make_object_button('EditMenuBar', 'menubar.xpm', 1)
