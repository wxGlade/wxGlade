"""\
wxToolBar objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2017-2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

from __future__ import absolute_import

import wx

import common, compat, config, misc, clipboard
import os, re
from .tool import *
import new_properties as np
from edit_windows import EditBase, PreviewMixin, EditStylesMixin
from gui_mixins import BitmapMixin
from wcodegen.taghandler import BaseXmlBuilderTagHandler

if compat.IS_CLASSIC:
    from .ToolsDialog28 import ToolsDialog as _ToolsDialog
else:
    from .ToolsDialog import ToolsDialog as _ToolsDialog

class ToolsDialog(_ToolsDialog):
    # _ToolsDialog is generated from res/ToolsDialog.wxg
    columns  = ["label","bitmap1","bitmap2","short_help","long_help","type","handler","id"]
    coltypes = {"type":int}
    # these will be copied:
    default_item = ("item","","","","",0,"","")
    separator_item = ("---","---","---","---","",0,"","---")

    name_re = re.compile(r'^[a-zA-Z_]+[\w-]*(\[\w*\])*$')
    handler_re = re.compile(r'^(([a-zA-Z_]+[a-zA-Z0-9_-]*)|()|(lambda .*))$')

    def __init__(self, parent, owner, items=None):
        _ToolsDialog.__init__(self, parent)

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

    def on_drop_files(self, screen_xy, ctrl, filenames):
        # as this dialog is shown modal with the main window as parent,
        # this method is called from the main window drop target
        if ctrl is not self.bitmap1 and ctrl is not self.bitmap2: return False
        
        if not wx.GetKeyState(wx.WXK_ALT) and not wx.GetKeyState(wx.WXK_CONTROL):
            # insert relative filename, if available and the filename is under the project directory
            filenames = [misc.get_relative_path(filename) for filename in filenames]

        if os.path.sep=="\\": filenames = [filename.replace("\\", "/") for filename in filenames]
        if len(filenames)==1:
            ctrl.SetValue(filenames[0])
        else:
            other_ctrl = self.bitmap2 if ctrl is self.bitmap1 else self.bitmap1
            other_ctrl.SetValue(filenames[1])

    def on_char(self, event):
        # keyboard navigation: up/down arrows and also Tab on some buttons
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

        self.bitmap1_button.Enable(enable)
        self.bitmap2_button.Enable(enable)

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

    def _add_new_item(self, item):
        # helper for the next two methods
        index = self.selected_index + 1
        if not self.items.GetItemCount():
            self._enable_fields()
        if index < 0:
            index = self.items.GetItemCount()
        self._insert_item(index, item)
        self._select_item(index, force=True)

    def add_item(self, event):
        "Event handler called when the Add button is clicked"
        self._add_new_item( list(self.default_item) )

    def add_separator(self, event):
        "Event handler called when the Add Separator button is clicked"
        self._add_new_item( self.separator_item )

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

        if self._get_item_text(index, "label") != '---':
            # skip if the selected item is a separator
            for i,colname in enumerate(self.columns):
                s = getattr(self, colname)
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
        item_count = self.items.GetItemCount()
        self.move_up.Enable( index>0 )
        self.move_down.Enable( index<item_count-1 )
        self.remove.Enable(item_count)
        self._ignore_events = False

    def on_label_edited(self, event):
        if not self._ignore_events:
            value = self.label.GetValue().lstrip()
            self._set_item_string(self.selected_index, "label", value)
        event.Skip()

    def on_event_handler_edited(self, event):
        value = self.handler.GetValue()
        if not value or self.handler_re.match(value):
            self.handler.SetBackgroundColour( compat.wx_SystemSettings_GetColour(wx.SYS_COLOUR_WINDOW) )
            valid = True
        else:
            self.handler.SetBackgroundColour(wx.RED)
            valid = False
        self.handler.Refresh()
        self._on_edited(event, "handler", value, valid)

    def _on_edited(self, event, colname, value, valid=True):
        if valid and not self._ignore_events:
            self._set_item_string(self.selected_index, colname, value)
        event.Skip()

    def on_type_edited(self, event):
        self._on_edited(event, "type", str(self.type.GetSelection()))

    def on_help_str_edited(self, event):
        self._on_edited(event, "short_help", self.short_help.GetValue())

    def on_long_help_str_edited(self, event):
        self._on_edited(event, "long_help", self.long_help.GetValue())

    def on_id_edited(self, event):
        self._on_edited(event, "id", self.id.GetValue())

    def on_bitmap1_edited(self, event):
        self._on_edited(event, "bitmap1", self.bitmap1.GetValue())

    def on_bitmap2_edited(self, event):
        self._on_edited(event, "bitmap2", self.bitmap2.GetValue())

    def remove_item(self, event):
        "Event handler called when the Remove button is clicked"
        if self.selected_index < 0: return
        self.items.DeleteItem(self.selected_index)
        self._select_item(self.selected_index-1, force=True)

    def _insert_item(self, index, item):
        self._insert_item_string(index, item[0])
        for col, value in enumerate(item):
            if col==0: continue
            self._set_item_string(index, col, value)
        # fix bug 698074
        self.items.SetItemState(index, wx.LIST_STATE_SELECTED, wx.LIST_STATE_SELECTED)

    def _get_item(self, index):
        ret = []
        for colname in self.columns:
            value = self._get_item_text(index, colname)
            if colname in self.coltypes:
                value = self.coltypes[colname](value)
            ret.append(value)
        return ret

    def add_items(self, tools):
        """adds the content of 'tools' to self.tool_items. tools is a sequence of (simple) tool items for the toolbar.
        At the moment there is no control support, but I hope to add it soon"""
        for i,tool in enumerate(tools):
            self._insert_item(i, tool)
        self._enable_fields(bool(tools))

    def get_items(self):
        "returns the contents of self.tool_items as a list of tools that describes the contents of the ToolBar"
        tools = []
        for i in range(self.items.GetItemCount()):
            item = self._get_item(i)
            kwargs = dict( key_value for key_value in zip(self.columns,item))
            tools.append( Tool( **kwargs ) )
        return tools

    def move_item_up(self, event):
        "moves the selected menu item before the previous one at the same level in self.items"
        self._do_move_item(event, self.selected_index, False)

    def _do_move_item(self, event, index, is_down):
        """internal function used by move_item_up and move_item_down.
        Returns the new index of the moved item, or None if no change occurred"""
        i = index+1 if is_down else index-1
        if i < 0 or i>=self.items.GetItemCount():
            wx.Bell()
            return None

        item = self._get_all_texts(index)
        self.items.DeleteItem(index)
        self._insert_item(i, item)
        self._select_item(i, force=True)

    def move_item_down(self, event):
        "moves the selected menu item after the next one at the same level in self.items"
        self._do_move_item(event, self.selected_index, True)

    def _select_bitmap(self, event, colname, title):
        control = getattr(self, colname)
        current = control.GetValue()
        directory = os.path.split(current)
        if os.path.isdir(current):
            directory = current
            current = ''
        elif directory and os.path.isdir(directory[0]):
            current = directory[1]
            directory = directory [0]
        elif common.root.filename:
            #directory = self.startDirectory
            directory = common.root.filename
            current = ""
        else:
            directory = ""
        value = misc.RelativeFileSelector(title, directory, current, wildcard="*.*", flags=wx.FD_OPEN)
        if value:
            control.SetValue(value)

    def select_bitmap1_art(self, event):
        # select wxART... standard bitmaps
        if compat.IS_CLASSIC:
            import _dialogs28 as _dialogs
        else:
            import _dialogs
        dlg = _dialogs.SelectArtDialog(self)

        default_width, default_height = getattr(self, "_last_ART_size", (None,None))
        old = self.bitmap1.GetValue().strip()
        if old.startswith("art:wxART_"):
            old = old[10:].split(",")
            if len(old)==4:
                try:
                    default_width, default_height = int(old[2]), int(old[3])
                except ValueError:
                    pass
            if old[0] in dlg.art_names:
                dlg.listctrl.Select(dlg.art_names.index(old[0]))

        if default_width: dlg.spin_width.SetValue(default_width)
        if default_height: dlg.spin_height.SetValue(default_height)

        user = dlg.ShowModal()
        if user!=wx.ID_OK: return
        width, height = self._last_ART_size = dlg.spin_width.GetValue(), dlg.spin_height.GetValue()
        art_index = dlg.listctrl.GetFirstSelected()
        if art_index==-1: return
        art_name = dlg.art_names[art_index]
        self.bitmap1.SetValue("art:wxART_%s,wxART_TOOLBAR,%d,%d"%(art_name, width, height))
        label = self.label.GetValue()
        if not label.strip() or label=="item":
            self.label.SetValue(art_name.replace("_"," ").capitalize())

    def select_bitmap1(self, event):
        self._select_bitmap(event, "bitmap1", 'Primary Bitmap')

    def select_bitmap2(self, event):
        self._select_bitmap(event, "bitmap2", 'Disabled Bitmap')

    # the action buttons are not linked to ESC and Enter to avoid accidental modifications
    def on_cancel(self, event):
        self.EndModal(wx.ID_CANCEL)

    def on_OK(self, event):
        self.EndModal(wx.ID_OK)



class ToolsProperty(np.Property):
    "Property to edit the tools of an EditToolBar instance"

    def __init__(self):
        np.Property.__init__(self, [])

    def create_editor(self, panel, sizer):
        self.edit_btn = wx.Button(panel, -1, _("Edit tools..."))
        sizer.Add(self.edit_btn, 0, wx.EXPAND|wx.TOP|wx.LEFT|wx.RIGHT, 4)
        self.edit_btn.Bind(wx.EVT_BUTTON, self.edit_tools)

    def edit_tools(self, event=None):
        if hasattr(self, "edit_btn") and self.edit_btn:
            parent = self.edit_btn.GetTopLevelParent()
        elif self.owner.widget:
            parent = self.owner.widget.GetTopLevelParent()
        else:
            parent = None
        dialog = ToolsDialog( common.main, self.owner, items=self.value )
        if not self.value: dialog.add_item(None)
        with misc.disable_stay_on_top(common.adding_window or parent):
            res = dialog.ShowModal()
        if res == wx.ID_OK:
            self.on_value_edited(dialog.get_items())
        dialog.Destroy()

    def write(self, output, tabs):
        inner_xml = []
        for tool in self.get():
            tool.write(inner_xml, tabs+1)
        output.extend( common.format_xml_tag( u'tools', inner_xml, tabs, is_xml=True) )



class ToolsHandler(BaseXmlBuilderTagHandler):
    itemattrs = ['label', 'id', 'short_help', 'long_help', 'bitmap1', 'bitmap2', 'type', 'handler']

    def __init__(self, owner):
        super(ToolsHandler, self).__init__()
        self.owner = owner
        self.tools = []
        self.curr_tool = None
        self.curr_index = -1

    def start_elem(self, name, attrs):
        if name == 'tools':
            return
        if name == 'tool':
            self.curr_tool = Tool()
        else:
            try:
                self.curr_index = self.itemattrs.index(name)
            except ValueError:
                self.curr_index = -1
                # just ignore the attributes we don't know

    def end_elem(self, name):
        if name == 'tool':
            self.tools.append(self.curr_tool)
        if name == 'tools':
            self.owner.properties["tools"].load(self.tools)
            return True

    def char_data(self, data):
        super(ToolsHandler, self).char_data(data)
        if self.curr_index >= 0:
            char_data = self.get_char_data()
            setattr(self.curr_tool, self.itemattrs[self.curr_index], char_data)



class EditToolBar(EditBase, PreviewMixin, EditStylesMixin, BitmapMixin):
    "Class to handle wxToolBar objects"

    WX_CLASS = 'wxToolBar'
    IS_TOPLEVEL = False
    _PROPERTIES = ["Widget", "bitmapsize", "margins", "packing", "separation", "style", "tools"]
    PROPERTIES = EditBase.PROPERTIES + _PROPERTIES + EditBase.EXTRA_PROPERTIES
    CHILDREN = 0

    def __init__(self, name, parent):
        EditBase.__init__( self, name, parent, "_toolbar" )
        EditStylesMixin.__init__(self)
        self._init_properties()
        self.parent.properties["toolbar"].set(True, notify=False)

    def _init_properties(self):
        self.tools = ToolsProperty()  # incl. the Edit button
        self.bitmapsize = np.IntPairPropertyD('16, 15', default_value='16, 15')
        self.margins    = np.IntPairPropertyD('0, 0',   default_value='0, 0')
        self.packing    = np.SpinPropertyD(1, val_range=(0,100), default_value=1, immediate=True)
        self.separation = np.SpinPropertyD(5, val_range=(0,100), default_value=5, immediate=True)

        self.window_id = None  # just a dummy for code generation

        self.widget = self._tb = None  # a panel and the actual ToolBar

    def create_widget(self):
        tb_style = self.style
        if wx.Platform == '__WXGTK__':
            tb_style |= wx.TB_DOCKABLE | wx.TB_FLAT
        if self.IS_TOPLEVEL:
            # "top-level" toolbar
            self.widget = wx.Frame(None, -1, misc.design_title(self.name))
            self.widget.SetClientSize((400, 30))
            self._tb = wx.ToolBar(self.widget, -1, style=tb_style)
            self.widget.SetToolBar(self._tb)
            self.widget.SetBackgroundColour(self.widget.GetBackgroundColour())
            icon = compat.wx_EmptyIcon()
            xpm = os.path.join(config.icons_path, 'toolbar.png')
            icon.CopyFromBitmap(misc.get_xpm_bitmap(xpm))
            self.widget.SetIcon(icon)
            self.widget.Bind(wx.EVT_CLOSE, lambda e: self.hide_widget())
            self.widget.Bind(wx.EVT_LEFT_DOWN, self.on_set_focus)
            if wx.Platform == '__WXMSW__':
                # MSW isn't smart enough to avoid overlapping windows, so
                # at least move it away from the 3 wxGlade frames
                self.widget.CenterOnScreen()
        else:
            # toolbar for a Frame
            self.widget = self._tb = wx.ToolBar(self.parent.widget, -1, style=tb_style)
            self.parent.widget.SetToolBar(self.widget)

        self.widget.Bind(wx.EVT_LEFT_DOWN, self.on_set_focus)

        # set the various property values
        self._set_bitmapsize()
        self._set_margins()
        self._set_packing()
        self._set_separation()

        self._set_tools()  # show the menus

    def set_style(self, value):
        EditStylesMixin.set_style(self, value)
        self._refresh_widget()

    # update widget ####################################################################################################
    def _set_margins(self):
        if not self._tb: return
        margins_p = self.properties["margins"]
        if not margins_p.is_active(): return
        self._tb.SetMargins(margins_p.get_tuple())

    def _set_bitmapsize(self):
        if not self._tb: return
        bitmapsize_p = self.properties["bitmapsize"]
        if not bitmapsize_p.is_active(): return
        self._tb.SetToolBitmapSize(bitmapsize_p.get_tuple())

    def _set_packing(self):
        if not self._tb: return
        packing_p = self.properties["packing"]
        if not packing_p.is_active(): return
        self._tb.SetToolPacking(packing_p.get())

    def _set_separation(self):
        if not self._tb: return
        separation_p = self.properties["separation"]
        if not separation_p.is_active(): return
        self._tb.SetToolSeparation(separation_p.get())

    def _set_tools(self):
        if not self._tb: return  # nothing left to do
        self._tb.ClearTools()
        # now add all the tools
        for tool in self.tools:
            if tool.id == '---':  # the tool is a separator
                self._tb.AddSeparator()
            else:
                bmp1 = self.get_preview_obj_bitmap(tool.bitmap1)
                bmp2 = self.get_preview_obj_bitmap(tool.bitmap2) if tool.bitmap2.strip() else None
                kinds = [wx.ITEM_NORMAL, wx.ITEM_CHECK, wx.ITEM_RADIO]
                try:
                    kind = kinds[int(tool.type)]
                except (ValueError, IndexError):
                    kind = wx.ITEM_NORMAL
                ADD = self._tb.AddLabelTool  if compat.IS_CLASSIC else  self._tb.AddTool
                if bmp2 is not None:
                    ADD( wx.NewId(), misc.wxstr(tool.label), bmp1, bmp2, kind,
                         misc.wxstr(tool.short_help), misc.wxstr(tool.long_help) )
                else:
                    ADD( wx.NewId(), misc.wxstr(tool.label), bmp1, shortHelp=misc.wxstr(tool.short_help) )
        # this is required to refresh the toolbar properly
        self._refresh_widget()

    def _refresh_widget(self):
        self._tb.Realize()
        self._tb.SetSize((-1, self._tb.GetBestSize()[1]))
        if self.IS_TOPLEVEL:
            widget = self.widget
            w = widget.GetClientSize()[0]
            h = self.widget.GetSize()[1] // 2
            widget.SetClientSize((w, h))

    def destroy_widget(self, level):
        # if parent is being deleted, we rely on this being destroyed
        if level==0 and not self.IS_TOPLEVEL and self.parent.widget:
            self.parent.widget.SetToolBar(None)
        if level==0:
            EditBase.destroy_widget(self, level)

    def hide_widget(self, *args):
        if self.widget and self.widget is not self._tb:
            self.widget.Hide()
            common.app_tree.Collapse(self.item)
            common.app_tree.select_item(self.parent)

    def remove(self, user=True):
        EditBase.remove(self, user=user)
        if 'toolbar' in self.parent.properties:
            self.parent.properties['toolbar'].set(False)
        return None   # explicitely return not a Slot; see history

    ####################################################################################################################

    def _create_popup_menu(self, widget):
        menu = misc.wxGladePopupMenu(self.name)

        if self.IS_TOPLEVEL:
            if self.widget and self.is_visible():
                item = misc.append_menu_item(menu, -1, _('Hide'))
                misc.bind_menu_item_after(widget, item, self.hide_widget)
            else:
                i = misc.append_menu_item(menu, -1, _('Show'))
                misc.bind_menu_item_after(widget, i, common.app_tree.show_toplevel, None, self)
            menu.AppendSeparator()

        i = misc.append_menu_item(menu, -1, _('Remove ToolBar\tDel'), wx.ART_DELETE)
        misc.bind_menu_item_after(widget, i, self.remove)
        i = misc.append_menu_item(menu, -1, _('Copy\tCtrl+C'), wx.ART_COPY)
        misc.bind_menu_item_after(widget, i, clipboard.copy, self)
        i = misc.append_menu_item(menu, -1, _('Cut\tCtrl+X'),  wx.ART_CUT)
        misc.bind_menu_item_after(widget, i, clipboard.cut, self)

        item = misc.append_menu_item(menu, -1, _('Edit tools ...'))
        misc.bind_menu_item_after(widget, item, self.properties["tools"].edit_tools)

        return menu

    def get_property_handler(self, name):
        if name == 'tools':
            return ToolsHandler(self)
        return None

    def _properties_changed(self, modified, actions):
        if modified and "style" in modified:
            actions.add("recreate_parent")
            return
        
        if not modified or "name" in modified and self.widget is not self._tb:
            self.widget.SetTitle(misc.design_title(misc.wxstr(self.name)))

        if not modified or "margins" in modified and self._tb:
            self._set_margins()
            actions.add("refresh")
        if not modified or "bitmapsize" in modified and self._tb:
            self._set_bitmapsize()
            actions.add("refresh")
        if not modified or "packing" in modified and self._tb:
            self._set_packing()
            actions.add("refresh")
        if not modified or "separation" in modified and self._tb:
            self._set_separation()
            actions.add("refresh")
        if not modified or "tools" in modified and self._tb:
            self._set_tools()
            actions.add("refresh")

        EditStylesMixin._properties_changed(self, modified, actions)
        EditBase._properties_changed(self, modified, actions)

    def properties_changed(self, modified):
        actions = EditBase.properties_changed(self, modified)
        # widget properties modified; trigger updates
        if self.widget:
            if config.debugging: print("Actions", actions)
            if "recreate_parent" in actions:
                self.parent.recreate_widget2()
        return actions

    def check_compatibility(self, widget, typename=None):
        return (False,"No pasting possible here.")
    def check_drop_compatibility(self):
        return (False,"Use toolbar editor: Properties -> Edit tools...")


class EditTopLevelToolBar(EditToolBar, PreviewMixin):
    WXG_BASE = "EditToolBar"
    IS_TOPLEVEL = True
    PROPERTIES = EditToolBar.PROPERTIES[:]
    np.insert_after(PROPERTIES, "name", "class")
    np.insert_after(PROPERTIES, "tools", "preview")
    TREE_ICON = "EditMenuBar"

    def __init__(self, name, parent, klass):
        EditBase.__init__( self, name, parent, None, klass )
        EditStylesMixin.__init__(self)
        self._init_properties()
        PreviewMixin.__init__(self)  # add a preview button

    def show_widget(self):
        if not self.widget: self.create()
        self.widget.Show()


def builder(parent, index, klass=None):
    "factory function for EditToolBar objects"
    if klass is None:
        import window_dialog as wd
        klass = 'wxToolBar' if common.root.language.lower()=='xrc' else 'MyToolBar'
    
        # if e.g. on a frame, suggest the user to add the tool bar to this
        toplevel_widget = None
        if misc.focused_widget is not None and not misc.focused_widget.IS_ROOT:
            toplevel_widget = misc.focused_widget.toplevel_parent
            if not "toolbar" in toplevel_widget.properties: toplevel_widget = None
        if toplevel_widget is not None:
            dialog = wd.StandaloneOrChildDialog(klass, "Select toolbar type and class", toplevel_widget, "toolbar")
        else:
            dialog = wd.WindowDialog(klass, None, 'Select standalone toolbar class', True)
    
        klass = dialog.show()
        dialog.Destroy()
        if klass is None: return None
    else:
        # allow to call builder(frame, None, True)
        toplevel_widget = parent

    if index=="_toolbar" or klass is True:
        # add to frame
        editor = EditToolBar(toplevel_widget.name+"_toolbar", toplevel_widget)
        if toplevel_widget.widget: editor.create()
        return editor

    # a standalone toolbar
    name = dialog.get_next_name("toolbar")
    editor = EditTopLevelToolBar(name, parent, klass)
    editor.create()
    editor.widget.Show()
    return editor


def xml_builder(parser, base, name, parent, index):
    "factory to build EditToolBar objects from a XML file"
    if parent.IS_ROOT:
        return EditTopLevelToolBar(name, parent, "ToolBar")
    return EditToolBar(name, parent)


def initialize():
    "initialization function for the module: returns a wxBitmapButton to be added to the main palette"
    common.widget_classes['EditToolBar'] = EditToolBar
    common.widgets_from_xml['EditToolBar'] = xml_builder
    common.widgets['EditToolBar'] = builder

    return common.make_object_button('EditToolBar', 'toolbar.png', 1)
