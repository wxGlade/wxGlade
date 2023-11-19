"""\
Classes to handle and display the structure of a wxGlade app

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016-2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import os.path
import wx
import misc, common, compat, config, clipboard

DEBUG = config.debugging and False
if DEBUG:
    import utilities

class WidgetTree(wx.TreeCtrl):#, Tree):
    "Tree with the ability to display the hierarchy of widgets"
    images = {} # Dictionary of icons of the widgets displayed
    def __init__(self, parent, application):
        style = wx.TR_DEFAULT_STYLE|wx.TR_HAS_VARIABLE_ROW_HEIGHT
        style |= wx.TR_EDIT_LABELS
        if wx.Platform == '__WXGTK__':    style |= wx.TR_NO_LINES|wx.TR_FULL_ROW_HIGHLIGHT
        elif wx.Platform == '__WXMAC__':  style &= ~wx.TR_ROW_LINES
        wx.TreeCtrl.__init__(self, parent, -1, style=style)
        scale = config.preferences.font_scale_tree
        if scale!=1.0:
            self.SetFont( self.GetFont().Scaled(scale) )
        self.scale_font( config.preferences.font_scale_tree )

        self.cur_widget = None  # reference to the selected widget
        self.root = application
        self._load_images()
        application.item = self.AddRoot(_('Application'), 0)
        self._SetItemData(application.item, application)
        self.skip_select = 0  # avoid an infinite loop on win32, as SelectItem fires an EVT_TREE_SEL_CHANGED event

        self.drop_target = clipboard.DropTarget(self, toplevel=True)
        self.SetDropTarget(self.drop_target)
        self._drag_ongoing = False
        self.auto_expand = True  # this control the automatic expansion of  nodes: it is set to False during xml loading
        self.Bind(wx.EVT_TREE_SEL_CHANGED, self.on_change_selection)
        self.Bind(wx.EVT_RIGHT_DOWN, self.popup_menu)
        self.Bind(wx.EVT_LEFT_DCLICK, self.on_left_dclick)
        self.Bind(wx.EVT_LEFT_DOWN, self.on_left_click) # allow direct placement of widgets
        self.Bind(wx.EVT_MENU, self.on_menu)  # for handling the selection of the first item
        self._popup_menu_widget = None  # the widget for the popup menu
        self.Bind(wx.EVT_TREE_BEGIN_DRAG, self.begin_drag)
        self.Bind(wx.EVT_LEAVE_WINDOW, self.on_leave_window)
        self.Bind(wx.EVT_MOUSE_EVENTS, self.on_mouse_events)

        self.Bind(wx.EVT_TREE_BEGIN_LABEL_EDIT, self.begin_edit_label)
        self.Bind(wx.EVT_TREE_END_LABEL_EDIT, self.end_edit_label)
        #self.Bind(wx.EVT_KEY_DOWN, misc.on_key_down_event)
        self.Bind(wx.EVT_KEY_DOWN, self.on_key_down_event)
        #self.Bind(wx.EVT_CHAR_HOOK, self.on_char)  # on wx 2.8 the event will not be delivered to the child
        self.Bind(wx.EVT_TREE_DELETE_ITEM, self.on_delete_item)

        if self.GetSelection().IsOk():
            # on some platforms, an item is pre-selected -> trigger an update
            self.Unselect()

    def scale_font(self, scale=1.0):
        if not hasattr(wx, "SpinCtrlDouble"): return
        if not hasattr(self, "_font"):
            if scale==1.0: return
            self._font = self.GetFont()
        self.SetFont( self._font if scale==1.0 else self._font.Scaled(scale) )

    def _load_images(self):
        bitmaps = {}
        for name, path in WidgetTree.images.items():
            img = misc.get_xpm_bitmap(path).ConvertToImage()
            # add 1 white pixel more at top and bottom
            img.Resize((21,23), (0,1), -1,-1,-1)
            bitmaps[name] = img.ConvertToBitmap()

        # grid sizer slots:
        # load template with one active slot and build new images with slot at any position top/left to bottom/right
        fn = os.path.join(config.icons_path, 'grid_sizer_slot_template.png')  # bottom / right in black
        template = wx.Image(fn, wx.BITMAP_TYPE_PNG)
        t_empty  = template.GetSubImage( (0,0,7,7) )    # empty slot
        t_active = template.GetSubImage( (0,7,7,7) )    # active slot
        t_bottom = template.GetSubImage( (0,20,20,1) )  # typically a black line at bottom
        t_right  = template.GetSubImage( (20,0,1,20) )  # typically a black line at right
        for pos_v in (0,1,2):           # active slot v
            for pos_h in (0,1,2):       # active slot h
                img = compat.wx_EmptyImage(21, 21)
                for x in (0,1,2):       # left, center, right
                    for y in (0,1,2):   # top, middle, button
                        t = t_active  if x==pos_h and y==pos_v else  t_empty
                        img.Paste(t, x*7, y*7)
                img.Paste(t_bottom, 0,20)
                img.Paste(t_right, 20,0)
                # add 1 white pixel more at top and bottom, store; store also -Disabled version
                img.Resize((21,23), (0,1), -1,-1,-1)
                name = 'EditGridSizerSlot-%d%d'%(pos_h,pos_v)
                bmp = bitmaps[name] = img.ConvertToBitmap()
                if hasattr(bmp, "ConvertToDisabled"):
                    bitmaps['%s-Disabled'%name] = bmp.ConvertToDisabled()
                else:
                    bitmaps['%s-Disabled'%name] = img.AdjustChannels(1.5, 1.5, 1.5).ConvertToBitmap()

        # store in the bitmap list
        image_list = wx.ImageList(21, 23)
        app_image = wx.Image(os.path.join(config.icons_path, 'application.png'), wx.BITMAP_TYPE_PNG)
        app_image.Resize((21,23), (0,1), -1,-1,-1)
        image_list.Add( app_image.ConvertToBitmap() )
        for name, bitmap in bitmaps.items():
            WidgetTree.images[name] = image_list.Add(bitmap)
        self.AssignImageList(image_list)

    def on_char(self, event):
        "called from main: start label editing on F2; skip events while editing"
        keycode = event.GetKeyCode()
        if keycode==wx.WXK_F2 and self.cur_widget and self.cur_widget._label_editable():
            # start label editing
            self.EditLabel( self.cur_widget.item )
            return True
        if isinstance(self.FindFocus(), wx.TextCtrl):
            # currently editing
            event.Skip()
            return True
        if keycode in (wx.WXK_UP, wx.WXK_DOWN, wx.WXK_LEFT, wx.WXK_RIGHT):
            event.Skip()
            return True
        if keycode==wx.WXK_RETURN and self.cur_widget and self.cur_widget.item:
            if self.cur_widget.IS_SLOT:
                if self.cur_widget.overlapped: return True
                return False
            if self.cur_widget.IS_TOPLEVEL and self.cur_widget.children:
                self.show_toplevel(None, editor=self.cur_widget)
                return True
            elif not self.IsExpanded(self.cur_widget.item) and self.cur_widget.children:
                self.ExpandAllChildren(self.cur_widget.item)
                return True
            elif self.IsExpanded(self.cur_widget.item):
                self.Collapse(self.cur_widget.item)
                return True
        return False

    def on_key_down_event(self, event):
        #if event.GetKeyCode()==13:
            ## go to property editor
            #common.property_panel.SetFocus()
            #return
        if event.GetKeyCode()==wx.WXK_WINDOWS_MENU and self.cur_widget:
            # windows menu key pressed -> display context menu for selected item
            item = self.RootItem  if self.cur_widget is common.root else  self.cur_widget.item
            if item:
                rect = self.GetBoundingRect(item, textOnly=True)
                if rect is not None:
                    pos = (rect.right, (rect.top+rect.bottom)//2)
                    self.cur_widget.popup_menu(event, pos=pos)
                    return
        #misc.on_key_down_event(event)
        event.Skip()

    def begin_drag(self, evt):
        # start drag & drop
        item = evt.GetItem()
        widget = self._GetItemData(item)
        if not widget:
            if config.debugging: raise ValueError("internal error")
            return
        if widget is self.root or widget.IS_SLOT: return  # application and slots can't be dragged
        self._drag_ongoing = True
        clipboard.begin_drag(self, widget)
        self._drag_ongoing = False

    def begin_edit_label(self, evt):
        # Begin editing a label. This can be prevented by calling Veto()
        widget = self._GetItemData( evt.Item )
        if not widget or not widget._label_editable(): evt.Veto()

    def _split_name_label(self, new_value):
        # split user input into name and label; if there's no colon but a quotation mark, it's just a label
        new_name = new_label = None
        new_value = new_value.strip()
        if new_value.endswith("'") and ": '" in new_value:
            new_name, new_label = new_value.split(": '", 1)
            new_label = new_label[:-1]
        elif new_value.endswith('"') and ': "' in new_value:
            new_name, new_label = new_value.split(': "', 1)
            new_label = new_label[:-1]
        elif new_value and (new_value[0] in ["'",'"'] or new_value[-1] in ["'",'"']):
            # just a label; for this we accept errors, we just want at least one quotation mark
            if new_value[0]==new_value[-1] and len(new_value)>2:
                new_label = new_value[1:-1]
            elif new_value[0] in ["'",'"']:
                new_label = new_value[1:]
            elif new_value[-1] in ["'",'"']:
                new_label = new_value[:-1]
        elif not ":" in new_value:
            new_name = new_value
        return new_name, new_label

    def end_edit_label(self, evt):
        # Finish editing a label. This can be prevented by calling Veto()
        if evt.IsEditCancelled(): return
        item = evt.Item
        widget = self._GetItemData( item )
        if "name" not in widget.properties: return

        new_value = evt.Label
        if new_value==widget._get_tree_label(): return

        new_name = new_label = new_title = new_tab = new_class = new_stockitem = None

        if widget.check_prop_truth("class"):
            if new_value.count("(")==1 and new_value.count(")")==1:
                pre, new_class = new_value.split("(")
                new_class, post = new_class.split(")")
                if pre.endswith(" "): pre = pre[:-1]
                new_value = pre+post

        if widget.check_prop_truth("stockitem"):
            if ":" in new_value:
                new_name, new_stockitem = new_value.split(":", 1)
                new_stockitem = new_stockitem.strip().upper()
                if not new_stockitem in widget.STOCKITEMS: new_stockitem = None
            elif new_value.strip().upper() in widget.STOCKITEMS:
                new_stockitem = new_value.strip().upper()
            else:
                new_name = new_value
        elif "label" in widget.properties and widget._label_editable():
            new_name, new_label = self._split_name_label(new_value)
        elif "label" in widget.properties:
            # label is not editable, but name may have changed
            new_name, dummy = self._split_name_label(new_value)
        elif getattr(widget, "has_title", None):
            new_name, new_title = self._split_name_label(new_value)
        elif getattr(widget, "parent", None) and widget.parent.WX_CLASS=="wxNotebook" and "]" in new_value:
            # notebook pages: include page title: "[title] name"
            new_tab, new_name = new_value.rsplit("]",1)
            if "[" in new_tab: new_tab = new_tab.split("[",1)[-1]
            new_name = new_name.strip()
        else:
            new_name = new_value

        # check name
        if new_name:
            name_p = widget.properties["name"]
            if new_name==name_p.get(): new_name = None
        if new_name:
            # check
            warning, error = name_p.check_value(new_name)
            if warning or error: new_name = None

        # check class/klass
        if new_class:
            class_p = widget.properties["klass"]
            if new_class==class_p.get(): new_class = None
        if new_class:
            # check
            warning, error = name_p.check_value(new_class)
            if warning or error: new_class = None

        # check label
        if new_label is not None:
            label_p = widget.properties["label"]
            if new_label==label_p.get(): new_label = None
        if (not new_name and new_label is None and new_title is None and new_tab is None and new_class is None
            and not new_stockitem):
            # no change or an error
            wx.Bell()
            evt.Veto()
            return
        # check title
        if new_stockitem is not None:
            stockitem_p = widget.properties["stockitem"]
            if new_stockitem==stockitem_p.get(): new_stockitem = None
        # check title
        if new_title is not None:
            title_p = widget.properties["title"]
            if new_title==title_p.get(): new_title = None
        # check notabook tab
        if new_tab is not None:
            notebook = widget.parent
            tabs_p = notebook.properties["tabs"]
            if widget in notebook.children:
                index = notebook.children.index(widget)
                if notebook.tabs[index][0]==new_tab:
                    new_tab = None
                else:
                    new_tabs = [t[:] for t in notebook.tabs]  # a GridProperty, i.e. a list of lists
                    new_tabs[index][0] = new_tab
            else:
                new_tab = None

        # helper to set a property as main or monitored for history
        changing_properties = []
        def add_to_history(p):
            if not changing_properties:
                common.history.property_changing(p)
            else:
                common.history.monitor_property(p)
            changing_properties.append(p)

        # actually modify the values
        modified = set()

        if new_name:
            add_to_history(name_p)
            name_p.previous_value = name_p.value
            name_p.set(new_name, notify=False)
            modified.add("name")
        if new_class:
            add_to_history(class_p)
            class_p.previous_value = class_p.value
            class_p.set(new_class, notify=False)
            modified.add("class")
        if new_label:
            add_to_history(label_p)
            label_p.previous_value = label_p.value
            label_p.set(new_label, notify=False)
            modified.add("label")
        if new_stockitem:
            add_to_history(stockitem_p)
            stockitem_p.previous_value = stockitem_p.value
            stockitem_p.set(new_stockitem, notify=False)
            modified.add("stockitem")
        if new_title:
            add_to_history(title_p)
            title_p.previous_value = title_p.value
            title_p.set(new_title, notify=False)
            modified.add("title")
        if new_tab:
            from widgets.notebook.notebook import HistoryNotebookTabsItem
            add_to_history(tabs_p)
            tabs_p.previous_value = tabs_p.value  # XXX the value is a list of lists; check whether it's used
            tabs_p.set(new_tabs, notify=True)
            modified.add("tabs")
        if modified:
            widget.properties_changed(modified)
            self.root.saved = False  # update the status of the app
            if new_tab and len(modified)==1:
                # "tabs" is the only changed property
                item = common.history._finalize_item(stop=True)  # property_changing was called for "tabs"
                common.history.add_item( HistoryNotebookTabsItem(notebook, item, [], []) )
            else:
                common.history.property_changed(changing_properties[0])
                if new_tab:
                    # "tabs" is a monitored property: replace history item in 'dependent'
                    item = common.history.actions[-1].dependent[-1]
                    common.history.actions[-1].dependent[-1] = HistoryNotebookTabsItem(notebook, item, [], [])

        wx.CallAfter( self.refresh, widget, refresh_label=True)  # setting from within the event handler does not work

    if compat.IS_CLASSIC:
        def _SetItemData(self, item, data):
            self.SetPyData(item, data)
        def _GetItemData(self, item):
            if not bool(item): return None
            return self.GetPyData(item)
    else:
        def _SetItemData(self, item, data):
            self.SetItemData(item, data)
        def _GetItemData(self, item):
            if not bool(item): return None
            try:
                return self.GetItemData(item)
            except RuntimeError:
                # on GTK the above bool may return True even if the item is being deleted
                return None

    def add2(self, child, parent, index, item=None):
        "insert an item for child into the list of parent's items; optionally re-use old item"
        image = self.images.get( child._get_tree_image(), -1)
        label = child._get_tree_label()
        if item is None:
            if index is None:
                item = child.item = self.AppendItem(parent.item, label, image)
            else:
                item = child.item = compat.wx_Tree_InsertItemBefore(self, parent.item, index, label, image)
        else:
            # re-use
            child.item = item
            self.refresh(child)
        self._SetItemData(item, child)
        if self.auto_expand:
            self.Expand(parent.item)
        if DEBUG:
            print("added item", utilities.hx(item), child, child.item)
        return item

    def remove(self, editor):
        # just remove the mutual references between editor and tree item
        if editor.item is None: return  # could be during common.root.clear() when there is an error on loading
        self._SetItemData(editor.item, None)
        editor.item = None

    ####################################################################################################################
    # new implementation:
    def on_delete_item(self, event):
        item = event.GetItem()
        editor = self._GetItemData( item )
        if DEBUG:
            print("on_delete_item", utilities.hx(item), editor, editor and editor.item or None)
        if editor is not None and editor.item is item:
            editor.item = None

    def _get_children_items(self, item):
        items = []
        child_item, cookie = self.GetFirstChild(item)
        while child_item.IsOk():
            items.append(child_item)
            child_item, cookie = self.GetNextChild(item, cookie)
        return items

    def _build_children(self, editor, item, recursive=True):
        # XXX a better algorithm would be nice
        # currently it's checked from the start and from the end how many are matching; all inbetween are replaced
        if DEBUG: print("_build_children", editor)
        children = editor.get_all_children()
        items = self._get_children_items(editor.item)
        if DEBUG: print("children", children)
        if DEBUG: print("items", items)
        item_editors = []
        for child_item in items:
            child = self._GetItemData(child_item)
            if child is not None and (not children or not child in children):
                self._SetItemData(child_item, None)
                if child.item is child_item:
                    if DEBUG: print("removed child.item", utilities.hx(child), utilities.hx(child.item))
                    child.item = None  # is probably None already
                item_editors.append(None)
            else:
                item_editors.append(child)
        if DEBUG: print("item_editors", item_editors)
        if DEBUG: print()
        match_beginning = 0
        for c,child in enumerate(children):
            if c<len(item_editors) and item_editors[c] is child:
                match_beginning = c+1
            else:
                break
        match_end = 0
        c = -1
        while match_beginning+match_end<len(children):
            if match_end<len(item_editors) and item_editors[c] is children[c]:
                match_end += 1
            else:
                break
            c -= 1

        if len(children) > len(item_editors):
            # insert or add items, right after match_beginning
            for n in range( len(children) - len(item_editors) ):
                index = match_beginning + n
                child = children[index]
                item = self.add2(child, parent=editor, index=index)
                items.insert(index, item)
        elif len(children) < len(item_editors):
            # remove items, right after match_beginning
            for n in range( len(item_editors) - len(children) ):
                index = match_beginning 
                child_item = items[index]
                self.Delete(child_item)
                del items[match_beginning]
        if match_beginning+match_end<len(children):
            # length matches, re-use item in the middle
            for index in range(match_beginning, len(children)-match_end):
                child = children[index]
                item = self.add2(child, parent=editor, index=index, item=items[index])
        
        if not recursive:
            # update labels and images, called e.g. when notebook pages change
            for child in children:
                self.refresh(child)
            return
        for child, item in zip(children, items):
            self._build_children(child, item)

    def build(self, editor=None, recursive=True, freeze=False):
        if DEBUG:
            print("="*80)
            print("build", editor, recursive)
        # (re-)build tree from data structure
        # e.g. .build(control)
        # XXX if recursive is not True, ensure that all children are refreshed, as e.g. slot numbers may have changed
        if freeze: self.Freeze()  # Freeze/Thaw makes the tree scroll around, so it should only be used for major change
        try:
            if editor is None:
                editor = self.root
                item = self.GetRootItem()
            else:
                item = editor.item
                if item is None and editor.parent.item:
                    # check whether at the same position there is an item already without an editor
                    pos = editor.parent._get_child_pos(editor)
                    items = self._get_children_items(editor.parent.item)
                    if len(items)>pos and self._GetItemData(items[pos]) is None:
                        item = items[pos]
                        editor.item = item
                        self._SetItemData(item, editor)
                        self.refresh(editor)
                while item is None:
                    editor = editor.parent
                    item = editor.item
            self._build_children(editor, item, recursive)
        finally:
            if freeze: self.Thaw()
        #if config.debugging or DEBUG:
        if DEBUG:
            import utilities
            utilities.TreePrinter(editor)

    def OnCompareItems(self, item1, item2):
        # only used when re-ordering toplevel items
        editor1 = self._GetItemData(item1)
        editor2 = self._GetItemData(item2)
        parent = editor1.parent
        assert parent is editor2.parent and parent is common.root
        index1 = parent.children.index(editor1)
        index2 = parent.children.index(editor2)
        if index1<index2: return -1
        return +1

    def refresh(self, editor, refresh_label=True, refresh_image=True):
        # refresh label and/or image
        if editor.item is None: return
        if refresh_label:
            self.SetItemText(editor.item, editor._get_tree_label())
        if refresh_image:
            image = self.images.get( editor._get_tree_image(), -1)
            self.SetItemImage(editor.item, image)

    def select_item(self, editor):
        self.skip_select = True
        self.SelectItem(editor.item)
        self.skip_select = False
        self._set_cur_widget(editor)
        misc.set_focused_widget(self.cur_widget)

    def _set_cur_widget(self, editor):
        # set self.cur_widget; adjust label colors and bold if required (on Windows)
        if self.cur_widget and wx.Platform == "__WXMSW__" and self.cur_widget.item:
            item = self.cur_widget.item
            self.SetItemTextColour(item, wx.NullColour)
            self.SetItemBold( item, False )
        self.cur_widget = editor
        item = editor.item
        self.EnsureVisible(item)
        # ensure that the icon is visible
        text_rect = self.GetBoundingRect(item, True)
        if text_rect.x<22:
            self.SetScrollPos(wx.HORIZONTAL, self.GetScrollPos(wx.HORIZONTAL) - 22 + text_rect.x)
        if wx.Platform == "__WXMSW__":
            self.SetItemBold(item, True)
            self.SetItemTextColour(item, wx.BLUE)
        s = editor._get_tooltip_string()
        common.main.user_message( s and s.replace("\n", " ") or "" )

    def set_current_widget(self, editor):
        # interface from common.set_focused_widget
        if editor is None or editor is self.cur_widget or editor.item is None: return
        self.skip_select = True
        self.SelectItem(editor.item)
        if not self.IsExpanded(editor.item) and (not hasattr(self, "HasFocus") or not self.HasFocus()):
            self.Expand(editor.item)
        self.skip_select = False
        self._set_cur_widget(editor)

    def on_change_selection(self, event):
        if self.skip_select:
            event.Skip()
            return  # triggered by self.SelectItem in self.set_current_widget
        item = event.GetItem()
        editor = self._GetItemData(item)
        if not editor:  # can happen during build/rebuild_tree
            event.Skip()
            return
        self._set_cur_widget(editor)
        misc.set_focused_widget(editor)
        if not self.IsExpanded(item) and (not hasattr(self, "HasFocus") or not self.HasFocus()):
            self.Expand(item)
        self.SetFocus()

    def on_left_click(self, event):
        if not common.adding_widget: return event.Skip()
        editor = self.find_editor_by_pos( *event.GetPosition() )
        if not editor: return event.Skip()

        compatible, message = editor.check_drop_compatibility()
        if not compatible:
            event.Skip()
            misc.error_message(message)
            return

        common.adding_window = event.GetEventObject().GetTopLevelParent()
        if editor.IS_SLOT:
            editor.on_drop_widget(event)
        elif common.adding_sizer or True:
            editor.drop_sizer()
        common.adding_window = None
        self.SetFocus()  # required instead of Skip if widget builder is showing a dialog

    def on_left_dclick(self, event):
        x, y = event.GetPosition()
        editor = self.find_editor_by_pos(x, y)
        if not editor:
            event.Skip()
            return
        if editor.WX_CLASS=='wxMenuBar':
            editor.properties["menus"].edit_menus()
        elif editor.WX_CLASS=='wxToolBar':
            editor.properties["tools"].edit_tools()
        elif editor.IS_TOPLEVEL:
            self.show_toplevel(None, editor)
        else:
            event.Skip()

    def on_leave_window(self, event):
        self.SetCursor(wx.STANDARD_CURSOR)
        event.Skip()

    def on_menu(self, event):
        # the first entry in the popup menu, i.e. the name was selected
        if self._popup_menu_widget is None: return
        if self._popup_menu_widget.IS_TOPLEVEL:
            self.show_toplevel( None, self._popup_menu_widget )

    def on_mouse_events(self, event):
        if not self._drag_ongoing and not event.IsButton():
            message = None
            # set cursor to indicate a possible drop
            x,y = event.GetPosition()
            editor = self.find_editor_by_pos(x, y, toplevels_only=False)
            if editor is not None:
                if not common.adding_widget:
                    self.SetCursor(wx.STANDARD_CURSOR)
                else:
                    # check whether the item can be dropped here
                    compatible, message = editor.check_drop_compatibility()
                    if compatible:
                        self.SetCursor(wx.CROSS_CURSOR) # a Cursor instance
                    else:
                        compat.SetCursor(self, wx.CURSOR_NO_ENTRY)
            else:
                self.SetCursor(wx.STANDARD_CURSOR)
            compat.SetToolTip(self, message or "")
        event.Skip()

    def popup_menu(self, event, pos=None):
        editor = self.find_editor_by_pos(*event.GetPosition())
        if not editor: return
        self.select_item(editor)
        self._popup_menu_widget = editor
        editor.popup_menu(event, pos)
        self._popup_menu_widget = None

    def show_toplevel(self, event, editor=None):
        "Event handler for left double-clicks: if the click is above a toplevel widget and this is hidden, shows it"
        if editor is None:
            try: x, y = event.GetPosition()
            except AttributeError:
                # if we are here, event is a CommandEvent and not a MouseEvent
                editor = self._GetItemData(self.GetSelection())
                self.ExpandAllChildren(editor.item)  # if we are here, the widget must be shown
            else:
                editor = self.find_editor_by_pos(x, y, toplevels_only=True)

        if editor is None or editor.IS_ROOT: return

        # the actual toplevel widget may be one level higher, e.g. for a Panel, which is embedded in a Frame
        set_size = None
        if editor.IS_TOPLEVEL:
            # toplevel window or a menu/status bar
            toplevel_widget = editor.widget
            if editor.check_prop("size") and editor.check_prop("toolbar") and editor.toolbar:
                # apply workaround for size changes due to a toolbar; this would cause problems with automatic testing
                set_size = editor.properties.get("size").get_size()
        else:
            toplevel_widget = editor.widget.GetParent()

        if not editor.is_visible():
            # added by rlawson to expand node on showing top level widget
            self.ExpandAllChildren(editor.item)
            editor.show_widget()

            if wx.Platform != '__WXMSW__' and set_size is not None:
                #  XXX integrate with above or remove above again?
                toplevel_widget = editor.widget  # above it was not yet created
                wx.CallAfter(toplevel_widget.SetSize, set_size)
        else:
            editor.hide_widget()
            #toplevel_widget.GetTopLevelParent().Hide()
            if event: event.Skip()
        if "design" in editor.properties: editor.design.update_label()

    def find_editor_by_pos(self, x, y, toplevels_only=False):
        """Finds the node which is displayed at the given coordinates. Returns None if there's no match.
        If toplevels_only is True, scans only root's children"""
        item, flags = self.HitTest((x, y))
        if item and flags & (wx.TREE_HITTEST_ONITEMLABEL | wx.TREE_HITTEST_ONITEMICON):
            editor = self._GetItemData(item)
            if not toplevels_only or editor.parent is self.root:
                return editor
        return None

    def change_item_editor(self, old, new, keep_children=False):
        # called from edit_sizers.change_sizer
        self._SetItemData(old.item, new)
        new.item = old.item
        old.item = None
        self.refresh(new)
