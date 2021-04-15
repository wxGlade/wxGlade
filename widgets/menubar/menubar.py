"""\
wxMenuBar objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016-2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
import re

import common, compat, config, misc, clipboard
from wcodegen.taghandler import BaseXmlBuilderTagHandler
from MenuTree import MenuTree
import new_properties as np
from edit_windows import EditBase, PreviewMixin

if compat.IS_CLASSIC:
    from .MenuItemDialog28 import MenuItemDialog as _MenuItemDialog
else:
    from .MenuItemDialog import MenuItemDialog as _MenuItemDialog


class MenuItemDialog(_MenuItemDialog):
    # _MenuItemDialog is generated from res/MenuItemDialog
    columns = ["level", "label", "event_handler", "name", "type", "help_str", "id"]
    coltypes = {"type":int}
    # these will be copied:
    default_item = (None,"item","","",0,"","")
    separator_item = (None,"---","---","---","---","---","---")

    name_re = re.compile(r'^[a-zA-Z_]+[\w-]*(\[\w*\])*$')
    handler_re = re.compile(r'^(([a-zA-Z_]+[a-zA-Z0-9_-]*)|()|(lambda .*))$')

    def __init__(self, parent, owner, items=None):
        _MenuItemDialog.__init__(self, parent)

        self.owner = owner

        self.selected_index = -1  # index of the selected element in the wx.ListCtrl menu_items
        self._ignore_events = False

        if items:
            self.add_items(items)
            self._select_item(0)
        else:
            self._enable_fields(False)

        self.Bind(wx.EVT_CHAR_HOOK, self.on_char)
        self.remove.Bind(wx.EVT_CHAR_HOOK, self.on_button_char)  # to ignore the Enter key while the focus is on Remove
        self.items.Bind(wx.EVT_MOUSEWHEEL, lambda e: e.Skip())  # workaround to make the scroll wheel work...


    def on_char(self, event):
        # keyboard navigation: up/down arrows
        focus = self.FindFocus()
        k = event.GetKeyCode()
        if k==wx.WXK_TAB:
            if focus is self.type:
                self.label.SetFocus()
            else:
                event.Skip()
            return

        if k in (wx.WXK_DOWN, wx.WXK_UP) and focus is self.type:
            event.Skip()
            return

        if event.AltDown():
            if k==wx.WXK_RETURN or k==ord("O"):
                self.EndModal(wx.ID_OK)
                return
            if k==ord("C"):
                self.EndModal(wx.ID_CANCEL)
                return

        if event.ControlDown() and k==wx.WXK_RETURN:
            self.EndModal(wx.ID_OK)
            return

        if k==wx.WXK_RETURN:  # ignore Enter key except when editing an item in the list control
            if not (self.items.EditControl and event.GetEventObject() is self.items.EditControl):
                return

        if k==wx.WXK_DOWN:
            if event.AltDown():
                self.move_item_down(event)
            else:
                if self.selected_index+1 < self.items.GetItemCount():
                    self._select_item(self.selected_index+1)
                else:
                    wx.Bell()
            return
        if k==wx.WXK_UP:
            if event.AltDown():
                self.move_item_up(event)
            else:
                if self.selected_index>0:
                    self._select_item(self.selected_index-1)
                else:
                    wx.Bell()
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


    def _enable_fields(self, enable=True, clear=False):
        if clear:
            restore = self._ignore_events
            self._ignore_events = True
        for name in self.columns:
            control = getattr(self, name, None)
            if not control: continue
            control.Enable(enable)
            if clear and isinstance(control, wx.TextCtrl): control.SetValue("")
        if clear: self._ignore_events = restore

    def _get_item_text(self, index, col):
        if isinstance(col, str): col = self.columns.index(col)
        return self.items.GetItem(index, col).GetText()

    def _get_all_texts(self, index):
        return [self._get_item_text(index, j) for j in range(len(self.columns))]

    def _set_item_string(self, index, col, s):
        if not isinstance(s, compat.unicode): s = misc.wxstr(s)
        if isinstance(col, str): col = self.columns.index(col)
        compat.ListCtrl_SetStringItem(self.items, index, col, s)
    
    def _insert_item_string(self, index, s):
        if not isinstance(s, compat.unicode): s = misc.wxstr(s)
        return compat.ListCtrl_InsertStringItem(self.items, index, s)

    def _add_new_item(self, unindented_item):
        # helper for the next two methods
        index = self.selected_index + 1
        item_level = 0
        if not self.items.GetItemCount():
            self._enable_fields()
        if index < 0:
            index = self.items.GetItemCount()
        elif index > 0:
            item_level = self.item_level(index-1)
        item = list(unindented_item)
        if item[1]=="---" and item_level==0: return wx.Bell()
        indent = "    " * item_level
        item[0] = str(item_level)
        item[1] = indent+item[1]
        self._insert_item(index, item)
        self._select_item(index, force=True)

    def add_item(self, event):
        "Event handler called when the Add button is clicked"
        self._add_new_item(self.default_item)

    def add_separator(self, event):
        "Event handler called when the Add Separator button is clicked"
        self._add_new_item(self.separator_item)

    def show_item(self, event):
        "Event handler called when a menu item in the list is selected"
        if not self._ignore_events:
            self._select_item(event.GetIndex())
        event.Skip()

    def _select_item(self, index, force=False):
        item_count = self.items.GetItemCount()
        if index == -1 and item_count: index = 0
        if index >= item_count and item_count: index = item_count-1
        if index==self.selected_index and not force: return
        self.selected_index = index
        if index == -1:
            self._enable_fields(False, clear=True)
            self._enable_buttons()
            return

        self._ignore_events = True
        self.items.Select(index)

        if self._get_item_text(index, "name") != '---':
            # skip if the selected item is a separator
            for i,colname in enumerate(self.columns):
                s = getattr(self, colname, None)
                if not s: continue
                coltype = self.coltypes.get(colname,None)
                value = self._get_item_text(index, i)
                if coltype is None:
                    # at this point, the value should be validated already
                    s.SetBackgroundColour( compat.wx_SystemSettings_GetColour(wx.SYS_COLOUR_WINDOW) )
                    s.SetValue(value)
                elif coltype is int:
                    s.SetSelection( int(value) )
            self.label.SetValue(self.label.GetValue().lstrip())
            self._enable_fields(True)
        else:
            self._enable_fields(False, clear=True)
        self._enable_buttons()
        state = wx.LIST_STATE_SELECTED | wx.LIST_STATE_FOCUSED
        self.items.SetItemState(index, state, state)  # fix bug 698071

    def _enable_buttons(self):
        # activate the left/right/up/down buttons
        index = self.selected_index
        if index>=0: item_level = self.item_level(index)
        item_count = self.items.GetItemCount()
        self.move_left.Enable( index!=0 and not (index+1<item_count and (item_level < self.item_level(index+1)) ))
        self.move_right.Enable( index>=1 and item_level <= self.item_level(index-1) )
        self.move_up.Enable( index>0 )
        self.move_down.Enable( index<item_count-1 )
        self.remove.Enable(item_count)
        self._ignore_events = False

    def on_label_edited(self, event):
        if not self._ignore_events:
            value = "    " * self.item_level(self.selected_index) + self.label.GetValue().lstrip()
            self._set_item_string(self.selected_index, "label", value)
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
        self._on_edited(event, "event_handler", value, valid)

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
            for i in range(self.items.GetItemCount()):
                if i==self.selected_index: continue
                if value == self._get_item_text(i, "name"):
                    valid = False
                    self.name.SetBackgroundColour( wx.Colour(255, 255, 0, 255) )  # YELLOW
                    break
        self.name.Refresh()
        self._on_edited(event, "name", value, valid)

    def _on_edited(self, event, colname, value, valid=True):
        if valid and not self._ignore_events:
            self._set_item_string(self.selected_index, colname, value)
        event.Skip()

    def on_type_edited(self, event):
        self._on_edited(event, "type", str(self.type.GetSelection()))

    def on_help_str_edited(self, event):
        self._on_edited(event, "help_str", self.help_str.GetValue())

    def on_id_edited(self, event):
        self._on_edited(event, "id", self.id.GetValue())

    def item_level(self, index, label=None):
        "returns the indentation level of the menu item at the given index"
        return int(self._get_item_text(index, 0))

    def remove_item(self, event):
        "Event handler called when the Remove button is clicked"
        if self.selected_index < 0: return
        index = self.selected_index
        if index+1 < self.items.GetItemCount() and (self.item_level(index) < self.item_level(index+1)):
            # the item to be deleted is parent to the following item -> move up the following item
            self.move_item_left(index=index+1)
        self.items.DeleteItem(index)
        index = max(self.selected_index-1,0) if self.items.GetItemCount() else -1
        self._select_item( index, force=True)

    def _insert_item(self, index, item):
        self._insert_item_string(index, item[0])
        for col, value in enumerate(item):
            if col==0: continue
            value = compat.unicode(value) if value is not None else ""
            self._set_item_string(index, col, value)
        self.items.SetItemState(index, wx.LIST_STATE_SELECTED, wx.LIST_STATE_SELECTED)  # fix bug 698074

    def _get_item(self, index):
        ret = []
        for colname in self.columns:
            value = self._get_item_text(index, colname)
            if colname in self.coltypes:
                value = self.coltypes[colname](value)
            ret.append(value)
        return ret

    def add_items(self, menus):
        """adds the content of 'menus' to self.items. menus is a sequence of
        trees which describes the structure of the menus"""
        indent = "    "

        def add(node, level):
            i = self.items.GetItemCount()
            self._insert_item_string(i, level)
            label = indent * level + node.label.lstrip().replace("\t","\\t")
            self._set_item_string(i, "label", label)
            self._set_item_string(i, "event_handler", node.handler)
            self._set_item_string(i, "name", node.name)
            self._set_item_string(i, "help_str", node.help_str)
            self._set_item_string(i, "id", node.id)
            if node.label==node.name==node.id=='---':
                self._set_item_string(i, "type", '')
            else:
                item_type = 0
                try:
                    if node.checkable and int(node.checkable):
                        item_type = 1
                    elif int(node.radio):
                        item_type = 2
                except ValueError:
                    pass
                self._set_item_string(i, "type", misc.wxstr(item_type))
            for item in node.children:
                add(item, level+1)

        for tree in menus:
            add(tree.root, 0)
        if self.items.GetItemCount():
            self._enable_fields()

    def get_menus(self):
        """returns the contents of self.menu_items as a list of trees which
        describe the structure of the menus in the format used by EditMenuBar"""
        trees = []

        def add(node, index):
            label         = self._get_item_text(index, "label").lstrip().replace("\\t", "\t")
            id            = self._get_item_text(index, "id")
            name          = self._get_item_text(index, "name")
            help_str      = self._get_item_text(index, "help_str")
            event_handler = self._get_item_text(index, "event_handler")
            try:
                item_type = int(self._get_item_text(index, "type"))
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
        for index in range(self.items.GetItemCount()):
            label = self._get_item_text(index, "label").replace("\\t", "\t")
            lvl = self.item_level(index)
            if not lvl:
                t = MenuTree( self._get_item_text(index, "name"), label,
                              id=self._get_item_text(index, "id"), handler=self._get_item_text(index, "event_handler") )
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

    def move_item_left(self, event=None, index=None):
        """moves the selected menu item one level up in the hierarchy, i.e.
        shifts its label 4 spaces left in self.menu_items"""
        if index is None:
            index = self.selected_index
        if index <= 0:
            return wx.Bell()
        level = self.item_level(index)
        if level==0 or ( index+1 < self.items.GetItemCount() and (level < self.item_level(index+1)) ):
            return wx.Bell()
        label = self._get_item_text(index, "label")
        if level==1 and label.endswith("---"):
            return wx.Bell()
        level -= 1
        self._set_item_string(index, "label", label[4:])
        self._set_item_string(index, "level", level)
        self.items.SetItemState(index, wx.LIST_STATE_SELECTED, wx.LIST_STATE_SELECTED)
        self._enable_buttons()

    def move_item_right(self, event):
        """moves the selected menu item one level down in the hierarchy, i.e.
        shifts its label 4 spaces right in self.menu_items"""
        index = self.selected_index
        if index <= 0:
            wx.Bell()
            return
        level = self.item_level(index)
        if level > self.item_level(index-1):
            wx.Bell()
            return
        level += 1
        label = self._get_item_text(index, "label")
        self._set_item_string(index, "label", misc.wxstr(" "*4) + label)
        self._set_item_string(index, "level", level)
        self.items.SetItemState(index, wx.LIST_STATE_SELECTED, wx.LIST_STATE_SELECTED)
        self._enable_buttons()

    def move_item_up(self, event):
        "moves the selected menu item before the previous one at the same level in self.menu_items"
        if self.selected_index<=0:
            wx.Bell()
            return
        self._do_move_item(event, self.selected_index, False)

    def _do_move_item(self, event, index, is_down):
        """internal function used by move_item_up and move_item_down.
        Returns the new index of the moved item, or None if no change occurred"""
        if index <= 0:
            return wx.Bell()

        level = self.item_level(index)
        items_to_move = [ self._get_all_texts(index) ]
        i = index+1
        while i < self.items.GetItemCount():
            # collect the items to move up
            if self.item_level(i) > level:
                items_to_move.append(self._get_all_texts(i))
                i += 1
            else: break
        i = index-1
        while i >= 0:
            lvl = self.item_level(i)
            if level == lvl: break
            elif level > lvl:
                return wx.Bell()
            i -= 1
        for j in range(len(items_to_move)-1, -1, -1):
            self.items.DeleteItem(index+j)
        items_to_move.reverse()
        for level, label, event_handler, name, type_, help_str, id in items_to_move:
            i = self._insert_item_string(i, level)
            self._set_item_string(i, "label", label)
            self._set_item_string(i, "name", name)
            self._set_item_string(i, "help_str", help_str)
            self._set_item_string(i, "type", type_)
            self._set_item_string(i, "event_handler", event_handler)
            self._set_item_string(i, "id", id)
        ret_idx = i
        if is_down: ret_idx += len(items_to_move)
        self._select_item(ret_idx, True)

    def move_item_down(self, event):
        "moves the selected menu item after the next one at the same level in self.menu_items"
        if self.selected_index < 0: return
        index = self.selected_index

        level = self.item_level(index)
        i = index+1
        while i < self.items.GetItemCount():
            # collect the items to move down
            if self.item_level(i) > level:
                i += 1
            else: break
        if i < self.items.GetItemCount():
            self._do_move_item(event, i, True)
        else:
            wx.Bell()

    # the action buttons are not linked to ESC and Enter to avoid accidental modifications
    def on_cancel(self, event):
        self.EndModal(wx.ID_CANCEL)

    def on_OK(self, event):
        self.EndModal(wx.ID_OK)


class MenuProperty(np.Property):
    "Property to edit the menus of an EditMenuBar instance"

    def __init__(self):
        np.Property.__init__(self, [])

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
        with misc.disable_stay_on_top(common.adding_window or parent):
            res = dialog.ShowModal()
        if res == wx.ID_OK:
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
                t = MenuTree( attrs['name'], attrs['label'], attrs.get('itemid', ''), attrs.get('help_str', ''),
                              handler=attrs.get('handler', '') )
                self.curr_menu.append( (t.root,) )
                self.menus.append(t)
                return
            node = MenuTree.Node( label=attrs['label'], name=attrs['name'], id=attrs.get('itemid', ''),
                                  help_str=attrs.get('help_str', ''), handler=attrs.get('handler', '') )
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
            self.owner.properties["menus"].load(self.menus)
            return True

    def char_data(self, data):
        super(MenuHandler, self).char_data(data)
        char_data = self.get_char_data()
        setattr(self.curr_item, self.itemattrs[self.curr_index], char_data)



class EditMenuBar(EditBase):#, PreviewMixin):

    WX_CLASS = "wxMenuBar"
    _PROPERTIES = ["menus",]
    PROPERTIES = EditBase.PROPERTIES + _PROPERTIES + EditBase.EXTRA_PROPERTIES
    CHILDREN = 0

    def __init__(self, name, parent):
        EditBase.__init__(self, name, parent, "_menubar")

        self.menus = MenuProperty()
        self.window_id = None  # just a dummy for code generation

        self._mb = None  # the real menubar
        self.parent.properties["menubar"].set(True, notify=False)

    def create(self):
        EditBase.create(self)
        if self.IS_TOPLEVEL and self.widget:
            self.widget.Show()
            self.widget.Raise()

    def create_widget(self):
        if self.IS_TOPLEVEL:
            # "top-level" menubar
            self.widget = wx.Frame(None, -1, misc.design_title(self.name))
            self.widget.SetClientSize((400, 30))
            self._mb = wx.MenuBar()
            self.widget.SetMenuBar(self._mb)
            self.widget.SetBackgroundColour(self._mb.GetBackgroundColour())
            import os
            icon = compat.wx_EmptyIcon()
            xpm = os.path.join(config.icons_path, 'menubar.png')
            icon.CopyFromBitmap(misc.get_xpm_bitmap(xpm))
            self.widget.SetIcon(icon)
            self.widget.Bind(wx.EVT_CLOSE, lambda e: self.hide_widget())
        else:
            if wx.Platform=="_WXMAC__": return   # XXX check how a toplevel menu bar behaves on Mac OS
            self.widget = self._mb = wx.MenuBar()
            if self.parent.widget: self.parent.widget.SetMenuBar(self.widget)
            if wx.Platform == '__WXMSW__' or wx.Platform == '__WXMAC__':
                self.widget.SetFocus = lambda : None

        self.widget.Bind(wx.EVT_LEFT_DOWN, self.on_set_focus)
        self.set_menus()  # show the menus

    def remove(self, user=True):
        EditBase.remove(self, user=user)
        if 'menubar' in self.parent.properties:
            self.parent.properties['menubar'].set(False)
        return None   # explicitely return not a Slot; see history

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

    def destroy_widget(self, level):
        # if parent is being deleted, we rely on this being destroyed
        if level==0 and not self.IS_TOPLEVEL and self.parent.widget:
            self.parent.widget.SetMenuBar(None)
        if level==0:
            EditBase.destroy_widget(self, level)

    def hide_widget(self, *args):
        if self.widget and self.IS_TOPLEVEL:
            self.widget.Hide()
            common.app_tree.Collapse(self.item)
            common.app_tree.select_item(self.parent)

    def _create_popup_menu(self, widget=None):
        if widget is None: widget = self.widget
        menu = misc.wxGladePopupMenu(self.name)

        if self.IS_TOPLEVEL:
            if self.widget and self.is_visible():
                item = misc.append_menu_item(menu, -1, _('Hide'))
                misc.bind_menu_item_after(widget, item, self.hide_widget)
            else:
                i = misc.append_menu_item(menu, -1, _('Show'))
                misc.bind_menu_item_after(widget, i, common.app_tree.show_toplevel, None, self)
            menu.AppendSeparator()

        item = misc.append_menu_item(menu, -1, _('Remove MenuBar\tDel'), wx.ART_DELETE)
        misc.bind_menu_item_after(widget, item, self.remove)
        i = misc.append_menu_item(menu, -1,  _('Copy\tCtrl+C'), wx.ART_COPY)
        misc.bind_menu_item_after(widget, i, clipboard.copy, self)
        i = misc.append_menu_item(menu, -1, _('Cut\tCtrl+X'),  wx.ART_CUT)
        misc.bind_menu_item_after(widget, i, clipboard.cut, self)

        item = misc.append_menu_item(menu, -1, _('Edit menus ...'))
        misc.bind_menu_item_after(widget, item, self.properties["menus"].edit_menus)

        return menu

    def set_name(self, name):
        EditBase.set_name(self, name)
        if self.widget is not self._mb:
            self.widget.SetTitle(misc.design_title(misc.wxstr(self.name)))

    def get_property_handler(self, name):
        if name == 'menus':
            return MenuHandler(self)
        return None

    def _properties_changed(self, modified, actions):
        if not modified or "menus" in modified: self.set_menus()
        EditBase._properties_changed(self, modified, actions)

    def check_compatibility(self, widget, typename=None):
        return (False,"No pasting possible here.")
    def check_drop_compatibility(self):
        return (False,"Use menu editor: Properties -> Edit menus...")



class EditTopLevelMenuBar(EditMenuBar, PreviewMixin):
    WXG_BASE = "EditMenuBar"
    IS_TOPLEVEL = True
    _PROPERTIES = ["menus", "preview"]
    PROPERTIES = EditBase.PROPERTIES + _PROPERTIES + EditBase.EXTRA_PROPERTIES
    np.insert_after(PROPERTIES, "name", "class")
    TREE_ICON = "EditMenuBar"

    def __init__(self, name, parent, class_):
        EditBase.__init__(self, name, parent, None, class_)

        self.menus = MenuProperty()
        self.window_id = None  # just a dummy for code generation

        self._mb = None  # the real menubar

        PreviewMixin.__init__(self)  # add a preview button

    def show_widget(self):
        if not self.widget: self.create()
        self.widget.Show()


def builder(parent, index, klass=None):
    "factory function for EditMenuBar objects"
    # this one is a bit special as usually it's called with parent=application
    # if a frame w/o menubar is focused, it will ask the user whether he wants to add a menubar to that
    if klass is None:
        import window_dialog as wd
        klass = 'wxMenuBar' if common.root.language.lower()=='xrc' else 'MyMenuBar'
    
        # if e.g. on a frame, suggest the user to add the menu bar to this
        toplevel_widget = None
        if misc.focused_widget is not None and not misc.focused_widget.IS_ROOT:
            toplevel_widget = misc.focused_widget.toplevel_parent
            if not "menubar" in toplevel_widget.properties: toplevel_widget = None
        if toplevel_widget is not None:
            dialog = wd.StandaloneOrChildDialog(klass, "Select menubar type and class", toplevel_widget, "menubar")
        else:
            dialog = wd.WindowDialog(klass, None, 'Select standalone menubar class', True)
    
        klass = dialog.show()
        dialog.Destroy()
        if klass is None: return None
    else:
        # allow to call builder(frame, None, True)
        toplevel_widget = parent

    if index=="_menubar" or klass is True:
        # add to frame
        editor = EditMenuBar(toplevel_widget.name+"_menubar", toplevel_widget)
        if toplevel_widget.widget: editor.create()
        return editor

    # a standalone menubar
    name = dialog.get_next_name("menubar")
    editor = EditTopLevelMenuBar(name, parent, klass)
    editor.create()
    editor.widget.Show()
    return editor


def xml_builder(parser, base, name, parent, index):
    "factory to build EditMenuBar objects from a XML file"
    if parent.IS_ROOT:
        return EditTopLevelMenuBar(name, parent, "MenuBar")
    return EditMenuBar(name, parent)


def initialize():
    "initialization function for the module: returns a wx.BitmapButton to be added to the main palette"
    common.widget_classes['EditMenuBar'] = EditMenuBar
    common.widgets['EditMenuBar'] = builder
    common.widgets_from_xml['EditMenuBar'] = xml_builder

    return common.make_object_button('EditMenuBar', 'menubar.png', 1)
