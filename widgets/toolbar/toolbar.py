"""\
wxToolBar objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

from __future__ import absolute_import

import wx
from wx.lib.filebrowsebutton import FileBrowseButton

import common, compat, config, misc
import os, re
from tree import Node
from .tool import *
import new_properties as np
from edit_windows import EditBase, PreviewMixin, EditStylesMixin
from gui_mixins import BitmapMixin
from wcodegen.taghandler import BaseXmlBuilderTagHandler


class _MyBrowseButton(FileBrowseButton):
    def createBrowseButton( self):
        "Create the browse-button control"
        ID = wx.NewId()
        button =wx.Button(self, ID, misc.wxstr(self.buttonText))
        compat.SetToolTip(button, self.toolTip)
        w = button.GetTextExtent(self.buttonText)[0] + 10
        button.SetMinSize((w, -1))
        wx.EVT_BUTTON(button, ID, self.OnBrowse)
        return button

    def OnBrowse(self, event=None):
        " Going to browse for file... "
        current = self.GetValue()
        directory = os.path.split(current)
        if os.path.isdir(current):
            directory = current
            current = ''
        elif directory and os.path.isdir(directory[0]):
            current = directory[1]
            directory = directory [0]
        else:
            directory = self.startDirectory
        value = wx.FileSelector(self.dialogTitle, directory, current, wildcard=self.fileMask, flags=self.fileMode)
        if value:
            self.SetValue(value)



class ToolsDialog(wx.Dialog):
    def __init__(self, parent, owner, items=None):
        wx.Dialog.__init__(self, parent, -1, _("Toolbar editor"), style=wx.DEFAULT_DIALOG_STYLE|wx.RESIZE_BORDER)
        ADD_ID, REMOVE_ID, LABEL_ID, ID_ID, CHECK_RADIO_ID, LIST_ID, \
                ADD_SEP_ID, MOVE_UP_ID, MOVE_DOWN_ID, HELP_STR_ID, \
                LONG_HELP_STR_ID, BITMAP1_ID, BITMAP2_ID \
                = [wx.NewId() for i in range(13)]

        self._staticbox = wx.StaticBox(self, -1, _("Tool:"))

        self.owner = owner

        self.tool_items = wx.ListCtrl(self, LIST_ID, style=wx.LC_REPORT|wx.LC_SINGLE_SEL|wx.BORDER_SUNKEN, size=(300, -1) )
        self.selected_index = -1  # index of the selected element in the
                                 # wxListCtrl
        self.tool_items.InsertColumn(0, _("Label"))
        self.tool_items.InsertColumn(1, _("Id"))
        self.tool_items.InsertColumn(2, _("Primary Bitmap"))
        self.tool_items.InsertColumn(3, _("Disabled Bitmap"))
        self.tool_items.InsertColumn(4, _("Short Help"))
        self.tool_items.InsertColumn(5, _("Long Help"))
        self.tool_items.InsertColumn(6, _("Type"))
        self.tool_items.InsertColumn(7, _("Event Handler"))

        self.tool_items.SetColumnWidth(0, 100)
        self.tool_items.SetColumnWidth(2, 100)
        self.tool_items.SetColumnWidth(3, 150)
        self.tool_items.SetColumnWidth(4, 150)
        self.tool_items.SetColumnWidth(5, 100)
        self.tool_items.SetColumnWidth(6, 150)
        self.tool_items.SetColumnWidth(7, 150)

        # tool fields
        self.id = wx.TextCtrl(self, ID_ID)
        self.label = wx.TextCtrl(self, LABEL_ID)
        self.help_str = wx.TextCtrl(self, HELP_STR_ID)
        self.long_help_str = wx.TextCtrl(self, LONG_HELP_STR_ID)
        self.event_handler = wx.TextCtrl(self, -1)
        self.handler_re = re.compile(r'^\s*\w*\s*$')

        self.bitmap1 = _MyBrowseButton( self, BITMAP1_ID, labelText=_('Primary Bitmap'), buttonText='...',
                                        changeCallback=self.update_tool)
        self.bitmap2 = _MyBrowseButton( self, BITMAP2_ID, labelText=_('Disabled Bitmap'), buttonText='...',
                                        changeCallback=self.update_tool)
        self.check_radio = wx.RadioBox( self, CHECK_RADIO_ID, _("Type"),
                                        choices=['Normal', 'Checkable', 'Radio'], majorDimension=3 )

        self.add = wx.Button(self, ADD_ID, _("Add"))
        self.remove = wx.Button(self, REMOVE_ID, _("Remove"))
        self.add_sep = wx.Button(self, ADD_SEP_ID, _("Add separator"))

        # tools navigation
        self.move_up = wx.Button(self, MOVE_UP_ID, _("Up"))
        self.move_down = wx.Button(self, MOVE_DOWN_ID, _("Down"))

        self.ok = wx.Button(self, wx.ID_OK, _("OK"))
        self.apply = wx.Button(self, wx.ID_APPLY, _("Apply"))
        self.cancel = wx.Button(self, wx.ID_CANCEL, _("Cancel"))

        self.do_layout()

        # event handlers
        wx.EVT_BUTTON(self, ADD_ID, self.add_tool)
        wx.EVT_BUTTON(self, REMOVE_ID, self.remove_tool)
        wx.EVT_BUTTON(self, ADD_SEP_ID, self.add_separator)
        wx.EVT_BUTTON(self, MOVE_UP_ID, self.move_item_up)
        wx.EVT_BUTTON(self, MOVE_DOWN_ID, self.move_item_down)
        wx.EVT_BUTTON(self, wx.ID_APPLY, self.on_apply)
        wx.EVT_KILL_FOCUS(self.label, self.update_tool)
        wx.EVT_KILL_FOCUS(self.id, self.update_tool)
        wx.EVT_KILL_FOCUS(self.help_str, self.update_tool)
        wx.EVT_KILL_FOCUS(self.long_help_str, self.update_tool)
        wx.EVT_KILL_FOCUS(self.event_handler, self.update_tool)
        wx.EVT_RADIOBOX(self, CHECK_RADIO_ID, self.update_tool)
        wx.EVT_LIST_ITEM_SELECTED(self, LIST_ID, self.show_tool)
        if items:
            self.add_tools(items)

    def do_layout(self):
        self.label.Enable(False)
        self.id.Enable(False)
        self.help_str.Enable(False)
        self.long_help_str.Enable(False)
        self.event_handler.Enable(False)
        self.bitmap1.Enable(False)
        self.bitmap2.Enable(False)
        self.check_radio.Enable(False)

        sizer = wx.BoxSizer(wx.VERTICAL)
        sizer2 = wx.StaticBoxSizer(self._staticbox, wx.VERTICAL)
        self.label.SetSize((150, -1))
        self.id.SetSize((150, -1))
        self.help_str.SetSize((150, -1))
        self.long_help_str.SetSize((150, -1))
        self.event_handler.SetSize((150, -1))
        szr = wx.FlexGridSizer(2,0,0)
        flag = wx.FIXED_MINSIZE
        label_flag = wx.ALIGN_CENTER_VERTICAL
        szr.Add(wx.StaticText(self, -1, _("Id   ")), flag=label_flag)
        szr.Add(self.id, flag=flag)
        szr.Add(wx.StaticText(self, -1, _("Label  ")), flag=label_flag)
        szr.Add(self.label, flag=flag)
        szr.Add(wx.StaticText(self, -1, _("Short Help  ")), flag=label_flag)
        szr.Add(self.help_str, flag=flag)
        szr.Add(wx.StaticText(self, -1, _("Long Help  ")), flag=label_flag)
        szr.Add(self.long_help_str, flag=flag)
        szr.Add(wx.StaticText(self, -1, _("Event Handler  ")), flag=label_flag)
        szr.Add(self.event_handler, flag=flag)
        sizer2.Add(szr, 1, wx.ALL|wx.EXPAND, 5)
        sizer2.Add(self.bitmap1, 0, wx.EXPAND)
        sizer2.Add(self.bitmap2, 0, wx.EXPAND)
        sizer2.Add(self.check_radio, 0, wx.LEFT|wx.RIGHT|wx.BOTTOM, 4)
        szr = wx.GridSizer(0, 2, 3, 3)
        szr.Add(self.add, 0, wx.EXPAND); szr.Add(self.remove, 0, wx.EXPAND)
        sizer2.Add(szr, 0, wx.EXPAND)
        sizer2.Add(self.add_sep, 0, wx.TOP|wx.EXPAND, 3)

        sizer3 = wx.BoxSizer(wx.VERTICAL)
        sizer3.Add(self.tool_items, 1, wx.ALL|wx.EXPAND, 5)
        sizer4 = wx.BoxSizer(wx.HORIZONTAL)

        sizer4.Add(self.move_up, 0, wx.LEFT|wx.RIGHT, 3)
        sizer4.Add(self.move_down, 0, wx.LEFT|wx.RIGHT, 5)
        sizer3.Add(sizer4, 0, wx.ALIGN_CENTER|wx.ALL, 5)
        szr = wx.BoxSizer(wx.HORIZONTAL)
        szr.Add(sizer3, 1, wx.ALL|wx.EXPAND, 5)
        szr.Add(sizer2, 0, wx.TOP|wx.BOTTOM|wx.RIGHT, 5)
        sizer.Add(szr, 1, wx.EXPAND)
        sizer2 = wx.BoxSizer(wx.HORIZONTAL)
        sizer2.Add(self.ok, 0, wx.ALL, 5)
        sizer2.Add(self.apply, 0, wx.ALL, 5)
        sizer2.Add(self.cancel, 0, wx.ALL, 5)
        sizer.Add(sizer2, 0, wx.ALL|wx.ALIGN_CENTER, 3)
        self.SetAutoLayout(1)
        self.SetSizer(sizer)
        sizer.Fit(self)
        #self.SetSize((-1, 350))
        self.CenterOnScreen()

    def add_tool(self, event):
        "Event handler called when the Add button is clicked"
        index = self.selected_index = self.selected_index + 1
        if not self.tool_items.GetItemCount():
            for s in (self.label, self.id, self.help_str, self.long_help_str,
                      self.bitmap1, self.bitmap2, self.check_radio, self.event_handler):
                s.Enable(True)
        if index < 0:
            index = self.tool_items.GetItemCount()
        label, wid, check_radio = "item", "", "0"
        bitmap1, bitmap2, help_str, long_help_str = [""] * 4
        self.tool_items.InsertStringItem(index, label)
        self.tool_items.SetStringItem(index, 1, wid)
        self.tool_items.SetStringItem(index, 2, bitmap1)
        self.tool_items.SetStringItem(index, 3, bitmap2)
        self.tool_items.SetStringItem(index, 4, help_str)
        self.tool_items.SetStringItem(index, 5, long_help_str)
        self.tool_items.SetStringItem(index, 6, check_radio)
        self.tool_items.SetItemState(index, wx.LIST_STATE_SELECTED, wx.LIST_STATE_SELECTED)
        self.label.SetValue(label)
        self.id.SetValue(wid)
        self.check_radio.SetSelection(int(check_radio))
        self.bitmap1.SetValue(bitmap1, False)
        self.bitmap2.SetValue(bitmap2, False)
        self.help_str.SetValue(help_str)
        self.long_help_str.SetValue(long_help_str)
        self.event_handler.SetValue("")

    def add_separator(self, event):
        "Event handler called when the Add Separator button is clicked"
        index = self.selected_index+1
        if not self.tool_items.GetItemCount():
            for s in (self.label, self.id, self.help_str, self.long_help_str,
                      self.bitmap1, self.bitmap2, self.check_radio, self.event_handler):
                s.Enable(True)
        if index < 0: index = self.tool_items.GetItemCount()
        self.tool_items.InsertStringItem(index, '---')
        for i in range(1, 5):
            self.tool_items.SetStringItem(index, i, '---')
        self.tool_items.SetItemState(index, wx.LIST_STATE_SELECTED, wx.LIST_STATE_SELECTED)

    def show_tool(self, event):
        "Event handler called when a tool in the list is selected"
        self.selected_index = index = event.GetIndex()
        get_item = self.tool_items.GetItem
        if not self.tool_items.GetItem(index, 2).GetText() == '---':
            # skip if the selected item is a separator
            for (s, i) in ((self.label, 0), (self.id, 1),
                           (self.help_str, 4), (self.long_help_str, 5),
                           (self.event_handler, 7)):
                s.SetValue(get_item(index, i).GetText())
            self.bitmap1.SetValue(get_item(index, 2).GetText(), False)
            self.bitmap2.SetValue(get_item(index, 3).GetText(), False)
            try:
                self.check_radio.SetSelection( int(self.tool_items.GetItem(index, 6).GetText()) )
            except:
                self.check_radio.SetSelection(0)
        event.Skip()

    def update_tool(self, event):
        "Event handler called when some of the properties of the current tool changes"
        set_item = self.tool_items.SetStringItem
        index = self.selected_index
        handler = self.event_handler.GetValue()
        if not self.handler_re.match(handler):
            event.GetEventObject().SetFocus()
            return
        if index < 0:
            return event.Skip()
        set_item(index, 0, self.label.GetValue())
        set_item(index, 1, self.id.GetValue())
        set_item(index, 2, self.bitmap1.GetValue())
        set_item(index, 3, self.bitmap2.GetValue())
        set_item(index, 4, self.help_str.GetValue())
        set_item(index, 5, self.long_help_str.GetValue())
        set_item(index, 6, str(self.check_radio.GetSelection()))
        set_item(index, 7, self.event_handler.GetValue())
        try:
            event.Skip()
        except AttributeError:
            # this happens on wx2.4.0.1 for FileBrowseButton events
            pass
        # update the directory of the browse buttons
        directory = os.path.split(self.bitmap1.GetValue())[0]
        if not os.path.isdir(directory):
            directory = os.path.split(self.bitmap2.GetValue())[0]
        if os.path.isdir(directory):
            self.bitmap1.startDirectory = directory
            self.bitmap2.startDirectory = directory

    def remove_tool(self, event):
        "Event handler called when the Remove button is clicked"
        if self.selected_index >= 0:
            for s in (self.id, self.label, self.help_str, self.long_help_str, self.event_handler):
                s.SetValue("")
            for s in (self.bitmap1, self.bitmap2):
                s.SetValue("", False)
            self.check_radio.SetSelection(0)
            self.tool_items.DeleteItem(self.selected_index)
            if not self.tool_items.GetItemCount():
                for s in (self.id, self.label, self.help_str, self.long_help_str, self.bitmap1, self.bitmap2,
                          self.check_radio, self.event_handler):
                    s.Enable(False)

    def add_tools(self, tools):
        """adds the content of 'tools' to self.tool_items. tools is a sequence of (simple) tool items for the toolbar.
        At the moment there is no control support, but I hope to add it soon"""
        set_item = self.tool_items.SetStringItem
        add_item = self.tool_items.InsertStringItem
        index = [0]

        def add(tool):
            i = index[0]
            add_item(i, misc.wxstr(tool.label))
            set_item(i, 1, misc.wxstr(tool.id))
            set_item(i, 2, misc.wxstr(tool.bitmap1))
            set_item(i, 3, misc.wxstr(tool.bitmap2))
            set_item(i, 4, misc.wxstr(tool.short_help))
            set_item(i, 5, misc.wxstr(tool.long_help))
            set_item(i, 7, misc.wxstr(tool.handler))
            item_type = 0
            set_item(i, 6, misc.wxstr(tool.type))
            index[0] += 1
        for tool in tools:
            add(tool)
        if self.tool_items.GetItemCount():
            for s in (self.id, self.label, self.help_str, self.long_help_str,
                      self.bitmap1, self.bitmap2, self.check_radio, self.event_handler):
                s.Enable(True)

    def get_tools(self):
        "returns the contents of self.tool_items as a list of tools that describes the contents of the ToolBar"
        def get(i, j):
            return self.tool_items.GetItem(i, j).GetText()
        tools = []

        def add(index):
            label = get(index, 0)
            id = get(index, 1)
            bitmap1 = get(index, 2)
            bitmap2 = get(index, 3)
            short_help = get(index, 4)
            long_help = get(index, 5)
            event_handler = get(index, 7)
            try:
                item_type = int(get(index, 6))
            except ValueError:
                item_type = 0
            tools.append( Tool( label=label, id=id, type=item_type, short_help=short_help, long_help=long_help,
                                bitmap1=bitmap1, bitmap2=bitmap2, handler=event_handler ) )
        for index in range( self.tool_items.GetItemCount() ):
            add(index)

        return tools

    def move_item_up(self, event):
        "moves the selected tool before the previous one at the same level in self.tool_items"
        self.tool_items.SetFocus()
        if self.selected_index > 0:
            index = self.selected_index - 1
            vals1 = [ self.tool_items.GetItem(self.selected_index, i).GetText() for i in range(8) ]
            vals2 = [ self.tool_items.GetItem(index, i).GetText() for i in range(8) ]
            for i in range(8):
                self.tool_items.SetStringItem(index, i, vals1[i])
                self.tool_items.SetStringItem(self.selected_index, i, vals2[i])
            state = wx.LIST_STATE_SELECTED | wx.LIST_STATE_FOCUSED
            self.tool_items.SetItemState(index, state, state)
            self.selected_index = index

    def move_item_down(self, event):
        "moves the selected tool after the next one at the same level in self.tool_items"
        self.tool_items.SetFocus()
        if self.selected_index < self.tool_items.GetItemCount()-1:
            index = self.selected_index + 1
            vals1 = [ self.tool_items.GetItem(self.selected_index, i).GetText() for i in range(8) ]
            vals2 = [ self.tool_items.GetItem(index, i).GetText() for i in range(8) ]
            for i in range(8):
                self.tool_items.SetStringItem(index, i, vals1[i])
                self.tool_items.SetStringItem(self.selected_index, i, vals2[i])
            state = wx.LIST_STATE_SELECTED | wx.LIST_STATE_FOCUSED
            self.tool_items.SetItemState(index, state, state)
            self.selected_index = index

    def on_apply(self, event):
        self.owner.set_tools(self.get_tools())
        common.app_tree.app.saved = False



class ToolsProperty(np.Property):
    "Property to edit the tools of an EditToolBar instance"

    def __init__(self):
        np.Property.__init__(self, [])
        self.tools = {}

    def create_editor(self, panel, sizer):
        self.edit_btn = wx.Button(panel, -1, _("Edit tools..."))
        sizer.Add(self.edit_btn, 1, wx.EXPAND|wx.ALIGN_CENTER|wx.TOP|wx.BOTTOM, 4)
        self.edit_btn.Bind(wx.EVT_BUTTON, self.edit_tools)

    def edit_tools(self, event):
        dialog = ToolsDialog( self.edit_btn.GetTopLevelParent(), self.owner, items=self.value )
        if dialog.ShowModal() == wx.ID_OK:
            self.on_value_edited(dialog.get_tools())
        dialog.Destroy()

    def write(self, outfile, tabs):
        inner_xml = compat.StringIO()
        for tool in self.get():
            tool.write(inner_xml, tabs+1)
        stmt = common.format_xml_tag( u'tools', inner_xml.getvalue(), tabs, is_xml=True) 
        outfile.write(stmt)



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
            setattr(self.curr_tool,
                    self.itemattrs[self.curr_index], char_data)



class EditToolBar(EditBase, PreviewMixin, EditStylesMixin, BitmapMixin):
    "Class to handle wxToolBar objects"

    _PROPERTIES = ["bitmapsize", "margins", "packing", "separation", "tools", "preview"]
    PROPERTIES = EditBase.PROPERTIES + _PROPERTIES + EditBase.EXTRA_PROPERTIES

    def __init__(self, name, klass, parent):
        custom_class = parent is None
        EditBase.__init__( self, name, 'wxToolBar', parent, wx.NewId(), custom_class=custom_class )
        EditStylesMixin.__init__(self)

        # initialise instance properties
        self.bitmapsize = np.ScrollRatePropertyD('16, 15', default_value='16, 15')
        self.margins    = np.ScrollRatePropertyD('0, 0',   default_value='0, 0')
        self.packing    = np.SpinPropertyD(1, val_range=(0,100), default_value=1, immediate=True)
        self.separation = np.SpinPropertyD(5, val_range=(0,100), default_value=5, immediate=True)
        self.tools = ToolsProperty()  # the Edit button

        self.pwidget = self.widget = None  # a panel and the actual ToolBar

        if not self.parent:
            PreviewMixin.__init__(self)  # add a preview button
        else:
            self.preview = None

    def create_widget(self):
        tb_style = wx.TB_HORIZONTAL | self.style
        if wx.Platform == '__WXGTK__':
            tb_style |= wx.TB_DOCKABLE | wx.TB_FLAT
        if self.parent:
            self.pwidget = self.widget = wx.ToolBar(
                self.parent.widget, -1, style=tb_style)
            self.parent.widget.SetToolBar(self.pwidget)
        else:
            # "top-level" toolbar
            self.pwidget = wx.Frame(None, -1, misc.design_title(self.name))
            self.pwidget.SetClientSize((400, 30))
            self.widget = wx.ToolBar(self.pwidget, -1, style=tb_style)
            self.pwidget.SetToolBar(self.widget)
            self.pwidget.SetBackgroundColour(self.widget.GetBackgroundColour())
            icon = wx.EmptyIcon()
            xpm = os.path.join(config.icons_path, 'toolbar.xpm')
            icon.CopyFromBitmap(misc.get_xpm_bitmap(xpm))
            self.pwidget.SetIcon(icon)
            wx.EVT_CLOSE(self.pwidget, lambda e: self.hide_widget())
            wx.EVT_LEFT_DOWN(self.widget, self.on_set_focus)
            if wx.Platform == '__WXMSW__':
                # MSW isn't smart enough to avoid overlapping windows, so
                # at least move it away from the 3 wxGlade frames
                self.pwidget.CenterOnScreen()
        wx.EVT_LEFT_DOWN(self.pwidget, self.on_set_focus)

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
        if not self.widget: return
        margins_p = self.properties["margins"]
        if not margins_p.is_active(): return
        self.widget.SetMargins(margins_p.get_tuple())

    def _set_bitmapsize(self):
        if not self.widget: return
        bitmapsize_p = self.properties["bitmapsize"]
        if not bitmapsize_p.is_active(): return
        self.widget.SetToolBitmapSize(bitmapsize_p.get_tuple())

    def _set_packing(self):
        if not self.widget: return
        packing_p = self.properties["packing"]
        if not packing_p.is_active(): return
        self.widget.SetToolPacking(packing_p.get())

    def _set_separation(self):
        if not self.widget: return
        separation_p = self.properties["separation"]
        if not separation_p.is_active(): return
        self.widget.SetToolSeparation(separation_p.get())

    def _set_tools(self):
        if not self.widget: return  # nothing left to do
        self.widget.ClearTools()
        # now add all the tools
        for tool in self.tools:
            if misc.streq(tool.id, '---'):  # the tool is a separator
                self.widget.AddSeparator()
            else:
                bmp1 = self.get_preview_obj_bitmap(tool.bitmap1)
                bmp2 = self.get_preview_obj_bitmap(tool.bitmap2)
                kinds = [wx.ITEM_NORMAL, wx.ITEM_CHECK, wx.ITEM_RADIO]
                try:
                    kind = kinds[int(tool.type)]
                except (ValueError, IndexError):
                    kind = wx.ITEM_NORMAL
                self.widget.AddLabelTool( wx.NewId(), misc.wxstr(tool.label), bmp1, bmp2, kind,
                                          misc.wxstr(tool.short_help), misc.wxstr(tool.long_help) )
        # this is required to refresh the toolbar properly
        self._refresh_widget()

    def _refresh_widget(self):
        self.widget.Realize()
        self.widget.SetSize((-1, self.widget.GetBestSize()[1]))
        if self.parent:
            widget = self.parent.widget
            w, h = widget.GetClientSize()
            widget.SetClientSize((w, h+1))
            widget.SetClientSize((w, h))
        else:
            widget = self.pwidget
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
                self.pwidget = None
            else:
                if self.parent.widget:
                    self.parent.widget.SetToolBar(None)
        else:
            if self.pwidget:
                self.pwidget.Destroy()
                self.pwidget = None
        EditBase.remove(self)

    def popup_menu(self, event, pos=None):
        if self.parent is not None:
            return  # do nothing in this case
        super(EditToolBar, self).popup_menu(event, pos)

    def _create_popup_menu(self):
        REMOVE_ID = wx.NewId()
        HIDE_ID = wx.NewId()
        self._rmenu = misc.wxGladePopupMenu(self.name)
        misc.append_menu_item(self._rmenu, REMOVE_ID, _('Remove ToolBar\tDel'), wx.ART_DELETE)
        misc.append_menu_item(self._rmenu, HIDE_ID, _('Hide'))

        wx.EVT_MENU(self.pwidget, REMOVE_ID, misc.exec_after(self.remove))
        wx.EVT_MENU(self.pwidget, HIDE_ID, misc.exec_after(self.hide_widget))

        self._rmenu = (menu, widget) # store for destryoing and unbinding
        return menu

    def hide_widget(self, *args):
        if self.pwidget and self.pwidget is not self.widget:
            self.pwidget.Hide()
            common.app_tree.expand(self.node, False)
            common.app_tree.select_item(self.node.parent)
            common.app_tree.app.show_properties()

    def get_property_handler(self, name):
        if name == 'tools':
            return ToolsHandler(self)
        return None
    
    def properties_changed(self, modified):
        if not modified or "name" in modified and self.pwidget is not self.widget:
            self.pwidget.SetTitle(misc.design_title(misc.wxstr(self.name)))
        refresh = False
        if not modified or "margins" in modified and self.widget:
            self._set_margins()
            refresh = True
        if not modified or "bitmapsize" in modified and self.widget:
            self._set_bitmapsize()
            refresh = True
        if not modified or "packing" in modified and self.widget:
            self._set_packing()
            refresh = True
        if not modified or "separation" in modified and self.widget:
            self._set_separation()
            refresh = True
        if not modified or "tools" in modified and self.widget:
            self._set_tools()
            refresh = True
            
        if refresh: self._refresh_widget()

        EditBase.properties_changed(self, modified)



def builder(parent, sizer, pos):
    "factory function for EditToolBar objects"
    import window_dialog
    klass = 'wxToolBar' if common.app_tree.app.language.lower()=='xrc' else 'MyToolBar'
    dialog = window_dialog.WindowDialog(klass, None, 'Select toolbar class', True)
    klass = dialog.show()
    dialog.Destroy()
    if klass is None: return
    name = dialog.get_next_name("toolbar")

    tb = EditToolBar(name, klass, parent)
    tb.node = Node(tb)
    common.app_tree.add(tb.node)
    if parent.widget: tb.create()



def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    "factory to build EditMenuBar objects from a XML file"
    name = attrs.get('name')
    if parent is not None:
        if name:
            parent._toolbar.properties["name"].set(name)
            parent._toolbar.properties_changed(["name"])
        return parent._toolbar
    else:
        tb = EditToolBar(name, attrs.get('class', 'wxMenuBar'), None)
        tb.node = Node(tb)
        common.app_tree.add(tb.node)
        return tb


def initialize():
    "initialization function for the module: returns a wxBitmapButton to be added to the main palette"
    common.widgets_from_xml['EditToolBar'] = xml_builder
    common.widgets['EditToolBar'] = builder

    return common.make_object_button('EditToolBar', 'toolbar.xpm', 1)
