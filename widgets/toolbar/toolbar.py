"""\
wxToolBar objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2017-2019 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

from __future__ import absolute_import

import wx

import common, compat, config, misc
import os, re
from tree import Node
from .tool import *
import new_properties as np
from edit_windows import EditBase, PreviewMixin, EditStylesMixin
from gui_mixins import BitmapMixin
from wcodegen.taghandler import BaseXmlBuilderTagHandler


class ToolsDialog(wx.Dialog):
    # initially based on MenuItemDialog; with more abstraction, e.g. columns
    columns  = ["label","bitmap1","bitmap2","short_help","long_help","type","handler","id"]
    column_widths = [180,180,     120,       120,        180,        50,     120,           50]
    headers = ["Label","Primary Bitmap","Disabled Bitmap","Short Help","Long Help","Type","Event Handler","Id"]
    coltypes = {"type":int}
    default_item = ("item","","","","",0,"","")
    separator_item = ("---","---","---","---","",0,"","---")
    control_names = columns
    def __init__(self, parent, owner, items=None):
        style = wx.DEFAULT_DIALOG_STYLE|wx.RESIZE_BORDER|wx.WANTS_CHARS
        wx.Dialog.__init__(self, parent, -1, _("Toolbar editor"), style=style)

        # menu item fields
        self.label = wx.TextCtrl(self, wx.ID_ANY, "")
        self.bitmap1 = wx.TextCtrl(self, wx.ID_ANY, "")
        self.bitmap2 = wx.TextCtrl(self, wx.ID_ANY, "")
        self.handler = wx.TextCtrl(self, wx.ID_ANY, "")
        self.short_help = wx.TextCtrl(self, wx.ID_ANY, "")
        self.long_help = wx.TextCtrl(self, wx.ID_ANY, "")
        self.id = wx.TextCtrl(self, wx.ID_ANY, "")
        self.type = wx.RadioBox(self, wx.ID_ANY, "Type", choices=["Normal", "Checkable", "Radio"], majorDimension=1, style=wx.RA_SPECIFY_COLS)
        # dialog action buttons; these will be handled, instead of using stock OK/Cancel buttons
        self.ok     = wx.Button(self, wx.ID_ANY, "OK")
        self.cancel = wx.Button(self, wx.ID_ANY, "Cancel")
        # editor action buttons
        self.move_up = wx.Button(self, wx.ID_ANY, "Up")
        self.move_down = wx.Button(self, wx.ID_ANY, "Down")
        self.add = wx.Button(self, wx.ID_ANY, "&Add")
        self.remove = wx.Button(self, wx.ID_ANY, "&Remove")
        self.add_sep = wx.Button(self, wx.ID_ANY, "Add Separator")

        self.bitmap1_button = wx.Button(self, wx.ID_ANY, "...")
        self.bitmap2_button = wx.Button(self, wx.ID_ANY, "...")

        self.items = wx.ListCtrl(self, wx.ID_ANY, style=wx.BORDER_DEFAULT | wx.BORDER_SUNKEN | wx.LC_EDIT_LABELS | wx.LC_REPORT | wx.LC_SINGLE_SEL)

        self.__do_layout()
        self._set_tooltips()

        self.Bind(wx.EVT_TEXT, self.on_label_edited, self.label)
        self.Bind(wx.EVT_TEXT, self.on_event_handler_edited, self.handler)
        self.Bind(wx.EVT_TEXT, self.on_help_str_edited, self.short_help)
        self.Bind(wx.EVT_TEXT, self.on_long_help_str_edited, self.long_help)
        self.Bind(wx.EVT_TEXT, self.on_id_edited, self.id)
        self.Bind(wx.EVT_RADIOBOX, self.on_type_edited, self.type)

        self.Bind(wx.EVT_BUTTON, self.move_item_up, self.move_up)
        self.Bind(wx.EVT_BUTTON, self.move_item_down, self.move_down)
        self.Bind(wx.EVT_BUTTON, self.add_item, self.add)
        self.Bind(wx.EVT_BUTTON, self.remove_item, self.remove)
        self.Bind(wx.EVT_BUTTON, self.add_separator, self.add_sep)
        self.Bind(wx.EVT_BUTTON, self.on_cancel, self.cancel)
        self.Bind(wx.EVT_BUTTON, self.on_OK, self.ok)
        self.Bind(wx.EVT_BUTTON, self.select_bitmap1, self.bitmap1_button)
        self.Bind(wx.EVT_BUTTON, self.select_bitmap2, self.bitmap2_button)
        self.Bind(wx.EVT_TEXT, self.on_bitmap1_edited, self.bitmap1)
        self.Bind(wx.EVT_TEXT, self.on_bitmap2_edited, self.bitmap2)
        self.Bind(wx.EVT_LIST_ITEM_SELECTED, self.show_item, self.items)

        self.Bind(wx.EVT_CHAR_HOOK, self.on_char)
        self.remove.Bind(wx.EVT_CHAR_HOOK, self.on_button_char)  # to ignore the Enter key while the focus is on Remove

        self.owner = owner

        # ALB 2004-09-26: workaround to make the scroll wheel work...
        self.items.Bind(wx.EVT_MOUSEWHEEL, lambda e: e.Skip())
        for c,col in enumerate(self.columns):
            self.items.InsertColumn(c, _(col))
            self.items.SetColumnWidth(c, self.column_widths[c])

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
        if focus is self.type:
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
        event.Skip()

    def on_button_char(self, event):
        # for e.g. the Remove button we don't want an action on the Return button
        if event.GetKeyCode() != wx.WXK_RETURN:
            event.Skip()

    def __do_layout(self):
        # begin wxGlade: ToolsDialog.__do_layout
        sizer_1 = wx.BoxSizer(wx.VERTICAL)
        sizer_2 = wx.BoxSizer(wx.HORIZONTAL)
        sizer_5 = wx.BoxSizer(wx.HORIZONTAL)
        sizer_6 = wx.BoxSizer(wx.VERTICAL)
        grid_sizer = wx.FlexGridSizer(7, 2, 0, 0)
        sizer_bitmap1 = wx.BoxSizer(wx.HORIZONTAL)
        sizer_bitmap2 = wx.BoxSizer(wx.HORIZONTAL)
        self.label_6 = wx.StaticText(self, wx.ID_ANY, "Label:")
        grid_sizer.Add(self.label_6, 0, wx.ALIGN_CENTER_VERTICAL | wx.LEFT | wx.RIGHT, 4)
        grid_sizer.Add(self.label, 1, wx.EXPAND, 0)
        label_11 = wx.StaticText(self, wx.ID_ANY, "Primary Bitmap:")
        grid_sizer.Add(label_11, 0, wx.ALIGN_CENTER_VERTICAL | wx.LEFT | wx.RIGHT, 4)
        sizer_bitmap1.Add(self.bitmap1, 1, 0, 0)
        sizer_bitmap1.Add(self.bitmap1_button, 0, wx.BOTTOM | wx.LEFT | wx.TOP, 0)
        grid_sizer.Add(sizer_bitmap1, 1, wx.EXPAND, 0)
        label_12 = wx.StaticText(self, wx.ID_ANY, "Disabled Bitmap:")
        grid_sizer.Add(label_12, 0, wx.ALIGN_CENTER_VERTICAL | wx.LEFT | wx.RIGHT, 4)
        sizer_bitmap2.Add(self.bitmap2, 1, 0, 0)
        sizer_bitmap2.Add(self.bitmap2_button, 0, wx.BOTTOM | wx.LEFT | wx.TOP, 0)
        grid_sizer.Add(sizer_bitmap2, 1, wx.EXPAND, 0)
        self.label_7 = wx.StaticText(self, wx.ID_ANY, "Event Handler:")
        grid_sizer.Add(self.label_7, 0, wx.ALIGN_CENTER_VERTICAL | wx.LEFT | wx.RIGHT, 4)
        grid_sizer.Add(self.handler, 1, wx.EXPAND, 0)
        self.label_9 = wx.StaticText(self, wx.ID_ANY, "Short Help:")
        grid_sizer.Add(self.label_9, 0, wx.ALIGN_CENTER_VERTICAL | wx.LEFT | wx.RIGHT, 4)
        grid_sizer.Add(self.short_help, 1, wx.EXPAND, 0)
        self.label_9b = wx.StaticText(self, wx.ID_ANY, "Long Help:")
        grid_sizer.Add(self.label_9b, 0, wx.ALIGN_CENTER_VERTICAL | wx.LEFT | wx.RIGHT, 4)
        grid_sizer.Add(self.long_help, 1, wx.EXPAND, 0)

        self.label_10 = wx.StaticText(self, wx.ID_ANY, "ID:")
        grid_sizer.Add(self.label_10, 0, wx.ALIGN_CENTER_VERTICAL | wx.LEFT | wx.RIGHT, 4)
        grid_sizer.Add(self.id, 0, 0, 0)
        grid_sizer.AddGrowableCol(1)
        sizer_5.Add(grid_sizer, 2, wx.EXPAND, 0)
        sizer_5.Add(self.type, 0, wx.ALL, 4)
        sizer_5.Add((20, 20), 1, 0, 0)
        sizer_6.Add(self.ok, 0, wx.ALL, 5)
        sizer_6.Add(self.cancel, 0, wx.ALL, 5)
        sizer_5.Add(sizer_6, 0, wx.EXPAND, 0)
        sizer_1.Add(sizer_5, 0, wx.EXPAND, 0)
        sizer_2.Add(self.move_up, 0, wx.BOTTOM | wx.LEFT | wx.TOP, 8)
        sizer_2.Add(self.move_down, 0, wx.BOTTOM | wx.RIGHT | wx.TOP, 8)
        sizer_2.Add((20, 20), 1, 0, 0)
        sizer_2.Add(self.add, 0, wx.BOTTOM | wx.LEFT | wx.TOP, 8)
        sizer_2.Add(self.remove, 0, wx.BOTTOM | wx.TOP, 8)
        sizer_2.Add(self.add_sep, 0, wx.ALL, 8)
        sizer_2.Add((20, 20), 2, 0, 0)
        sizer_1.Add(sizer_2, 0, wx.EXPAND, 0)
        sizer_1.Add(self.items, 1, wx.EXPAND, 0)
        self.SetSizer(sizer_1)
        sizer_1.Fit(self)
        self.Layout()
        # end wxGlade
    def _set_tooltips(self):
        # set tooltips
        for c in (self.label_6, self.label):
            compat.SetToolTip(c, "The menu entry text;\nenter & for access keys (using ALT key)\nappend e.g. \\tCtrl-X for keyboard shortcut")
        for c in (self.label_7, self.handler):
            compat.SetToolTip(c, "Enter the name of an event handler method; this will be created as stub")
        for c in (self.label_10, self.id):
            compat.SetToolTip(c, "optional: enter wx ID")
        for c in (self.label_9, self.short_help):
            compat.SetToolTip(c , "This will be displayed as tooltip" )
        for c in (self.label_9b, self.long_help):
            compat.SetToolTip( c, "This will be displayed in the status bar" )
        compat.SetToolTip( self.move_up, "Move selected item up" )
        compat.SetToolTip( self.move_down, "Move selected item down" )
        compat.SetToolTip( self.items, "For navigation use the mouse or the up/down arrows" )

    def _enable_fields(self, enable=True):
        for name in self.control_names:
            control = getattr(self, name)
            control.Enable(enable)

    def add_item(self, event):
        "Event handler called when the Add button is clicked"
        index = self.selected_index = self.selected_index + 1
        if not self.items.GetItemCount():
            self._enable_fields()
        if index < 0:
            index = self.items.GetItemCount()
        item = list(self.default_item)
        self._insert_item(index, item)
        self._select_item(index, force=True)

    def add_separator(self, event):
        "Event handler called when the Add Separator button is clicked"
        index = self.selected_index + 1
        if not self.items.GetItemCount():
            self._enable_fields()
        if index < 0:
            index = self.items.GetItemCount()
        self._insert_item(index, self.separator_item)
        self._select_item(index, force=True)

    def show_item(self, event):
        "Event handler called when a menu item in the list is selected"
        if not self._ignore_events:
            self._select_item(event.GetIndex())
        event.Skip()

    def _select_item(self, index, force=False):
        if index >= self.items.GetItemCount() or index<0 or (index==self.selected_index and not force): return
        self._ignore_events = True
        self.items.Select(index)
        self.selected_index = index
        if self.items.GetItem(index, 2).GetText() != '---':
            # skip if the selected item is a separator
            for i,colname in enumerate(self.columns):
                s = getattr(self, colname)
                coltype = self.coltypes.get(colname,None)
                value = self.items.GetItem(index, i).GetText()
                if coltype is None:
                    # at this point, the value should be validated already
                    s.SetBackgroundColour( compat.wx_SystemSettings_GetColour(wx.SYS_COLOUR_WINDOW) )
                    s.SetValue(value)
                elif coltype is int:
                    s.SetSelection( int(value) )
            self.label.SetValue(self.label.GetValue().lstrip())
            self._enable_fields(True)
            # set focus to text field again
            focus = self.FindFocus()
            if not isinstance(focus, wx.TextCtrl) and isinstance(self._last_focus, wx.TextCtrl):
                self._last_focus.SetFocus()
        else:
            for c in (self.label, self.handler, self.long_help, self.short_help, self.id,
                      self.bitmap1, self.bitmap2):
                c.SetValue("")
            self._enable_fields(False)
        self._enable_buttons()
        if force:
            self.label.SetFocus()
            self.label.SelectAll()

    def _enable_buttons(self):
        # activate the left/right/up/down buttons
        index = self.selected_index
        item_count = self.items.GetItemCount()
        self.move_up.Enable( index>0 )
        self.move_down.Enable( index<item_count-1 )
        self._ignore_events = False

    def on_label_edited(self, event):
        if not self._ignore_events:
            value = self.label.GetValue().lstrip()
            compat.ListCtrl_SetStringItem(self.items, self.selected_index, self.columns.index("label"), value)
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
        if valid and not self._ignore_events:
            compat.ListCtrl_SetStringItem(self.items, self.selected_index, self.columns.index("handler"), value)
        event.Skip()

    def _on_edited(self, event, colname, value):
        if not self._ignore_events:
            idx = self.columns.index(colname)
            compat.ListCtrl_SetStringItem(self.items, self.selected_index, idx, value)
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
        index = self.selected_index+1
        for s in (self.label, self.bitmap1, self.bitmap2, self.id, self.label,
                  self.short_help, self.long_help, self.handler):
            s.SetValue("")
        self.type.SetSelection(0)
        self.items.DeleteItem(self.selected_index)
        if not self.items.GetItemCount():
            self._enable_fields(False)
        self.selected_index -= 1
        self.items.Select(self.selected_index)

    def _insert_item(self, index, item):
        compat.ListCtrl_InsertStringItem(self.items, index, item[0])
        for col, value in enumerate(item):
            if col==0: continue
            compat.ListCtrl_SetStringItem(self.items, index, col, compat.unicode(value))
        # fix bug 698074
        self.items.SetItemState(index, wx.LIST_STATE_SELECTED, wx.LIST_STATE_SELECTED)

    def _get_item(self, index):
        ret = []
        for c,colname in enumerate(self.columns):
            col = self.columns.index(colname)
            value = self.items.GetItem(index, col).GetText()
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
        state = wx.LIST_STATE_SELECTED | wx.LIST_STATE_FOCUSED
        self.items.SetItemState(self.selected_index, state, state)

    def _do_move_item(self, event, index, is_down):
        """internal function used by move_item_up and move_item_down.
        Returns the new index of the moved item, or None if no change occurred"""
        self.items.SetFocus()
        #index = self.selected_index
        i = index+1 if is_down else index-1
        if i < 0 or i>=self.items.GetItemCount(): return None

        def get(i, j): return self.items.GetItem(i, j).GetText()
        item = [get(index, j) for j in range(6)]
        self.items.DeleteItem(index)
        compat.ListCtrl_InsertStringItem(self.items, i, item[0])
        for col,content in enumerate(item):
            if col==0: continue
            compat.ListCtrl_SetStringItem(self.items, i,col, content)
        self._select_item(i, force=True)

    def move_item_down(self, event):
        "moves the selected menu item after the next one at the same level in self.items"
        self._do_move_item(event, self.selected_index, True)
        ## fix bug 698071
        #state = wx.LIST_STATE_SELECTED | wx.LIST_STATE_FOCUSED
        #self.items.SetItemState(self.selected_index, state, state)


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
        elif common.app_tree.app.filename:
            #directory = self.startDirectory
            directory = common.app_tree.app.filename
            current = ""
        else:
            directory = ""
        value = wx.FileSelector(_(title), directory, current, wildcard="*.*", flags=wx.FD_OPEN)
        if value:
            control.SetValue(value)

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
        self.tools = {}

    def create_editor(self, panel, sizer):
        self.edit_btn = wx.Button(panel, -1, _("Edit tools..."))
        sizer.Add(self.edit_btn, 0, wx.EXPAND|wx.TOP|wx.LEFT|wx.RIGHT, 4)
        self.edit_btn.Bind(wx.EVT_BUTTON, self.edit_tools)

    def edit_tools(self, event=None):
        if hasattr(self, "edit_btn"):
            parent = self.edit_btn.GetTopLevelParent()
        elif self.owner.widget:
            parent = self.owner.widget.GetTopLevelParent()
        else:
            parent = None
        dialog = ToolsDialog( parent, self.owner, items=self.value )
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
            self.owner.properties["tools"].set(self.tools)
            self.owner.properties_changed(["tools"])
            return True

    def char_data(self, data):
        super(ToolsHandler, self).char_data(data)
        if self.curr_index >= 0:
            char_data = self.get_char_data()
            setattr(self.curr_tool, self.itemattrs[self.curr_index], char_data)



class EditToolBar(EditBase, PreviewMixin, EditStylesMixin, BitmapMixin):
    "Class to handle wxToolBar objects"

    _PROPERTIES = ["Widget", "bitmapsize", "margins", "packing", "separation", "style", "tools", "preview"]
    PROPERTIES = EditBase.PROPERTIES + _PROPERTIES + EditBase.EXTRA_PROPERTIES

    def __init__(self, name, klass, parent):
        custom_class = parent is None
        EditBase.__init__( self, name, 'wxToolBar', parent, wx.NewId(), custom_class=custom_class )
        EditStylesMixin.__init__(self)

        # initialise instance properties
        self.bitmapsize = np.IntPairPropertyD('16, 15', default_value='16, 15')
        self.margins    = np.IntPairPropertyD('0, 0',   default_value='0, 0')
        self.packing    = np.SpinPropertyD(1, val_range=(0,100), default_value=1, immediate=True)
        self.separation = np.SpinPropertyD(5, val_range=(0,100), default_value=5, immediate=True)
        self.tools = ToolsProperty()  # incl. the Edit button

        self.window_id = None  # just a dummy for code generation

        self.widget = self._tb = None  # a panel and the actual ToolBar

        if not self.parent:
            PreviewMixin.__init__(self)  # add a preview button
            self._is_toplevel = True
        else:
            self.preview = None
            self._is_toplevel = False

    def create_widget(self):
        tb_style = wx.TB_HORIZONTAL | self.style
        if wx.Platform == '__WXGTK__':
            tb_style |= wx.TB_DOCKABLE | wx.TB_FLAT
        if self.parent:
            self.widget = self._tb = wx.ToolBar(self.parent.widget, -1, style=tb_style)
            self.parent.widget.SetToolBar(self.widget)
        else:
            # "top-level" toolbar
            self.widget = wx.Frame(None, -1, misc.design_title(self.name))
            self.widget.SetClientSize((400, 30))
            self._tb = wx.ToolBar(self.widget, -1, style=tb_style)
            self.widget.SetToolBar(self._tb)
            self.widget.SetBackgroundColour(self.widget.GetBackgroundColour())
            icon = compat.wx_EmptyIcon()
            xpm = os.path.join(config.icons_path, 'toolbar.xpm')
            icon.CopyFromBitmap(misc.get_xpm_bitmap(xpm))
            self.widget.SetIcon(icon)
            self.widget.Bind(wx.EVT_CLOSE, lambda e: self.hide_widget())
            self.widget.Bind(wx.EVT_LEFT_DOWN, self.on_set_focus)
            if wx.Platform == '__WXMSW__':
                # MSW isn't smart enough to avoid overlapping windows, so
                # at least move it away from the 3 wxGlade frames
                self.widget.CenterOnScreen()
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
        if self.parent:
            widget = self.parent.widget
            w, h = widget.GetClientSize()
            widget.SetClientSize((w, h+1))
            widget.SetClientSize((w, h))
        else:
            widget = self.widget
            w = widget.GetClientSize()[0]
            h = self.widget.GetSize()[1] // 2
            widget.SetClientSize((w, h))
    ####################################################################################################################

    def remove(self, *args, **kwds):
        if self.parent is not None:
            self.parent.properties['toolbar'].set(False)
            self.parent._toolbar = None
            if kwds.get('do_nothing', False):
                # this probably leaks memory, but avoids segfaults
                self.widget = None
            else:
                if self.parent.widget:
                    self.parent.widget.SetToolBar(None)
        else:
            if self.widget:
                compat.DestroyLater(self.widget)
                self.widget = None
        EditBase.remove(self)

    def popup_menu(self, event, pos=None):
        if self.parent is not None:
            return  # do nothing in this case
        super(EditToolBar, self).popup_menu(event, pos)

    def _create_popup_menu(self, widget):
        menu = misc.wxGladePopupMenu(self.name)

        if self.widget and self.is_visible():
            item = misc.append_menu_item(menu, -1, _('Hide'))
            misc.bind_menu_item_after(widget, item, self.hide_widget)
        else:
            i = misc.append_menu_item(menu, -1, _('Show'))
            misc.bind_menu_item_after(widget, i, common.app_tree.show_toplevel, None, self)
        menu.AppendSeparator()

        i = misc.append_menu_item(menu, -1, _('Remove ToolBar\tDel'), wx.ART_DELETE)
        misc.bind_menu_item_after(widget, i, self.remove)

        item = misc.append_menu_item(menu, -1, _('Edit tools ...'))
        misc.bind_menu_item_after(widget, item, self.properties["tools"].edit_tools)

        item = misc.append_menu_item(menu, -1, _('Hide'))
        misc.bind_menu_item_after(widget, item, self.hide_widget)

        return menu

    def hide_widget(self, *args):
        if self.widget and self.widget is not self._tb:
            self.widget.Hide()
            common.app_tree.expand(self.node, False)
            common.app_tree.select_item(self.node.parent)
            #common.app_tree.app.show_properties()

    def get_property_handler(self, name):
        if name == 'tools':
            return ToolsHandler(self)
        return None
    
    def properties_changed(self, modified):
        if not modified or "name" in modified and self.widget is not self._tb:
            self.widget.SetTitle(misc.design_title(misc.wxstr(self.name)))
        refresh = False
        if not modified or "margins" in modified and self._tb:
            self._set_margins()
            refresh = True
        if not modified or "bitmapsize" in modified and self._tb:
            self._set_bitmapsize()
            refresh = True
        if not modified or "packing" in modified and self._tb:
            self._set_packing()
            refresh = True
        if not modified or "separation" in modified and self._tb:
            self._set_separation()
            refresh = True
        if not modified or "tools" in modified and self._tb:
            self._set_tools()
            refresh = True

        EditStylesMixin.properties_changed(self, modified)
        if refresh: self._refresh_widget()

        EditBase.properties_changed(self, modified)

    def check_compatibility(self, widget, typename=None, report=False):
        return (False,"No pasting possible here.")
    def check_drop_compatibility(self):
        return (False,"Use toolbar editor: Properties -> Edit tools...")




def builder(parent, sizer, pos):
    "factory function for EditToolBar objects"
    import window_dialog as wd
    klass = 'wxToolBar' if common.app_tree.app.language.lower()=='xrc' else 'MyToolBar'

    # if e.g. on a frame, suggest the user to add the tool bar to this
    toplevel_widget = None
    if misc.focused_widget is not None and misc.focused_widget.node.parent:
        toplevel_widget = common.app_tree._find_toplevel(misc.focused_widget.node).widget
        if not "toolbar" in toplevel_widget.properties:
            toplevel_widget = None
    if toplevel_widget is not None:
        dialog = wd.StandaloneOrChildDialog(klass, "Select toolbar type and class", toplevel_widget, "toolbar")
    else:
        dialog = wd.WindowDialog(klass, None, 'Select standalone toolbar class', True)

    klass = dialog.show()
    dialog.Destroy()
    if klass is None: return
    if klass is True:
        # add to toplevel widget
        toplevel_widget.properties["toolbar"].set(True, notify=True)
        return
    name = dialog.get_next_name("toolbar")
    with parent and parent.frozen() or misc.dummy_contextmanager():
        tb = EditToolBar(name, klass, parent)
        tb.node = Node(tb)
        common.app_tree.add(tb.node)
        if parent and parent.widget: tb.create()



def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    "factory to build EditToolBar objects from a XML file"
    name = attrs.get('name')
    if parent is not None:
        if name:
            parent._toolbar.properties["name"].set(name)
            parent._toolbar.properties_changed(["name"])
        return parent._toolbar
    else:
        tb = EditToolBar(name, attrs.get('class', 'wxToolBar'), None)
        tb.node = Node(tb)
        common.app_tree.add(tb.node)
        return tb


def initialize():
    "initialization function for the module: returns a wxBitmapButton to be added to the main palette"
    common.widgets_from_xml['EditToolBar'] = xml_builder
    common.widgets['EditToolBar'] = builder

    return common.make_object_button('EditToolBar', 'toolbar.xpm', 1)
