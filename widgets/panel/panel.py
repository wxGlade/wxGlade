"""\
wxPanel objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016-2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import logging
import wx
import common, config, misc, clipboard, compat
import new_properties as np
from edit_windows import ManagedBase, TopLevelBase, EditStylesMixin
from edit_base import EditBase



class PanelBase(EditStylesMixin):
    "Class PanelBase"

    _PROPERTIES = ["Widget", "style", "scrollable", "scroll_rate"]
    CHILDREN = -1  # 0 or 1; either a sizer or nothing
    TREE_ICON = "EditPanel"

    def __init__(self, style='wxTAB_TRAVERSAL'):
        "Class to handle wxPanel objects"
        EditStylesMixin.__init__(self, style, 'wxPanel')

        # initialise properties
        self.scrollable      = np.CheckBoxProperty(False, default_value=False)
        self.scroll_rate = prop = np.IntPairPropertyD( "10, 10" )
        prop.set_blocked(True)

    def get_editor_name(self):
        ret = self.__class__.__name__
        if self.WX_CLASS=="wxScrolledWindow":
            # self.scrolled is True
            # "EditPanel"         -> 'EditScrolledWindow'
            # "EditTopLevelPanel" -> "EditTopLevelScrolledWindow"
            ret = ret.replace("Panel", "ScrolledWindow")
        return ret

    def finish_widget_creation(self, level):
        if self.IS_TOPLEVEL:
            super(PanelBase, self).finish_widget_creation(level)
        else:
            super(PanelBase, self).finish_widget_creation(level, sel_marker_parent=self.widget)

        if self.scrollable and self.check_prop("scroll_rate"):
            self.widget.SetScrollRate( *self.properties["scroll_rate"].get_tuple() )
        # this must be done here since ManagedBase.finish_widget_creation normally sets EVT_LEFT_DOWN to update_view
        if not self.widget.Disconnect(-1, -1, wx.wxEVT_LEFT_DOWN):
            logging.warning( _("EditPanel: Unable to disconnect the event handler") )
        self.widget.Bind(wx.EVT_LEFT_DOWN, self.drop_sizer)

    def _update_markers(self, event):
        def get_pos():
            x, y = self.widget.GetPosition()
            xx, yy = self.widget.GetViewStart()
            return x+xx, y+yy
        old = self.widget.GetPosition
        self.widget.GetPosition = get_pos
        #self._logger.debug("%s, %s", self.widget, self.sel_marker.owner)
        self.sel_marker.update()
        self.widget.GetPosition = old
        event.Skip()

    def on_enter(self, event):
        if self.children and common.adding_sizer:
            self.widget.SetCursor(wx.CROSS_CURSOR)
        else:
            self.widget.SetCursor(wx.STANDARD_CURSOR)

    def set_sizer(self, sizer):
        if sizer and sizer.widget and self.widget:
            self.widget.SetAutoLayout(True)
            self.widget.SetSizer(sizer.widget)
            self.widget.Layout()
        elif sizer is None and self.widget:
            self.widget.SetSizer(None)

    def drop_sizer(self, event=None, reset=None):
        if self.children or not common.adding_sizer:
            self.on_set_focus(event)  # default behaviour: call show_properties
            return
        if self.widget: self.widget.SetCursor(wx.NullCursor)
        common.history.widget_adding(self)
        new_widget = common.widgets[common.widget_to_add](self, None)
        if new_widget is None: return
        misc.rebuild_tree(new_widget)
        if reset is False: return
        if event is None or not misc.event_modifier_copy(event):
            common.adding_widget = common.adding_sizer = False
            common.widget_to_add = None
        common.history.widget_added(new_widget)

    def check_drop_compatibility(self):
        if self.children:
            return (False, 'Sizer or child widget already set for this panel.')

        if common.adding_sizer:
            return (True, None)
        return (False, 'Only sizers can be added here; optionally, delete the panel and add e.g. a notebook. ')

    def get_widget_best_size(self):
        if self.children and self.widget.GetSizer():
            self.children[0].fit_parent()
            return self.widget.GetSize()
        return self.widget.__class__.GetBestSize(self.widget)

    def _properties_changed(self, modified, actions):
        if not modified or "scrollable" in modified:
            if self.scrollable:
                if self.klass == 'wxPanel':
                    self.properties["class"].set('wxScrolledWindow')
                self.properties['scroll_rate'].set_active(True)
                self.properties['scroll_rate'].set_blocked(False)
            else:
                if self.klass == 'wxScrolledWindow':
                    self.properties["class"].set('wxPanel')
                self.properties['scroll_rate'].set_blocked(True)

        if self.widget and modified:
            if "scrollable" in modified and self.properties["scrollable"].previous_value!=self.scrollable:
                actions.add("recreate")
            elif "scroll_rate" in modified and self.scrollable and isinstance(self.widget, wx.ScrolledWindow):
                self.widget.SetScrollRate( *self.properties["scroll_rate"].get_tuple() )
        EditStylesMixin._properties_changed(self, modified, actions)
        if self.widget and modified and "style" in modified:
            # unfortunately, there seems no way to trigger a refresh after style change other than resizing the window
            sz = self.widget.GetSize()
            self.skip_on_size = True  # required for EditTopLevelPanel
            self.widget.SetSize( (sz[0]-1,sz[1]-1) )
            self.skip_on_size = True
            self.widget.SetSize(sz)

    def _get_tooltip(self):
        if self.children: return None
        if self.parent.WX_CLASS in ("wxSplitterWindow", "wxNotebook"):
            return "Add a sizer here; optionally, delete the panel and add e.g. a notebook, splitter or grid. "
        return "Add a sizer here"

    def add_item(self, child, index=None):
        EditBase.add_item(self, child, index)
        if self.widget: compat.SetToolTip(self.widget, self._get_tooltip_string())

    def remove_item(self, child, level, keep_slot=False):
        EditBase.remove_item(self, child, level, keep_slot)
        if self.widget: compat.SetToolTip(self.widget, self._get_tooltip_string())


class EditPanel(PanelBase, ManagedBase):
    "Class to handle wxPanel objects"
    WX_CLASS = "wxPanel"  # this will be overwritten in properties_changed depending on the "scrolled" property
    WX_CLASSES = ("wxPanel", "wxScrolledWindow")
    PROPERTIES = ManagedBase.PROPERTIES + PanelBase._PROPERTIES + ManagedBase.EXTRA_PROPERTIES
    np.insert_after(PROPERTIES, "name", "class", "custom_base")

    def __init__(self, name, parent, index, style='wxTAB_TRAVERSAL'):
        ManagedBase.__init__(self, name, parent, index)
        PanelBase.__init__(self, style)

    def create_widget(self):
        # to be done: use ScrolledWindow only if scrolling is required
        if self.scrollable:
            self.widget = wx.ScrolledWindow(self.parent_window.widget, wx.ID_ANY, style=self.style)
        else:
            self.widget = wx.Panel(self.parent_window.widget, wx.ID_ANY, style=self.style)
        self.widget.Bind(wx.EVT_ENTER_WINDOW, self.on_enter)
        self.widget.GetBestSize = self.get_widget_best_size
        if not self.parent.IS_SIZER:
            def GetBestSize():
                if self.widget and self.widget.GetSizer():
                    return self.widget.GetSizer().GetMinSize()
                return self.widget.__class__.GetBestSize(self.widget)
            self.widget.GetBestSize = GetBestSize

    def set_sizer(self, sizer):
        # called from sizer.create: self.window.set_sizer(self)
        assert self.children and sizer is self.children[0]
        super(EditPanel, self).set_sizer(sizer)
        if sizer and sizer.widget and self.widget:
            sizer.set_item_best_size(self, size=self.widget.GetBestSize())

    def _create_popup_menu(self, widget=None):
        if widget is None: widget = self.widget
        menu = misc.wxGladePopupMenu(self.name)
        i = misc.append_menu_item(menu, -1, _('Remove Panel\tDel'),  wx.ART_DELETE)
        misc.bind_menu_item_after(widget, i, self.remove)
        i = misc.append_menu_item(menu, -1,   _('Copy\tCtrl+C'), wx.ART_COPY)
        misc.bind_menu_item_after(widget, i, clipboard.copy, self)
        i = misc.append_menu_item(menu, -1,    _('Cut\tCtrl+X'),  wx.ART_CUT)
        misc.bind_menu_item_after(widget, i, clipboard.cut, self)

        i = misc.append_menu_item(menu, -1, _('Paste Sizer\tCtrl+V'), wx.ART_PASTE)
        misc.bind_menu_item_after(widget, i, clipboard.paste, self)
        if self.children or not clipboard.check("sizer"): i.Enable(False)
        menu.AppendSeparator()

        if hasattr(self.parent, "_add_parent_popup_menu_items"):
            self.parent._add_parent_popup_menu_items(menu, self, widget)

        i = misc.append_menu_item(menu, -1, _('Preview'))
        misc.bind_menu_item_after(widget, i, self.preview_parent)

        return menu

    def check_compatibility(self, widget, typename=None, report=True):
        "check whether widget can be pasted"
        if self.children and not self.children[0].IS_SLOT:
            return (False, 'Sizer already set for this panel')
        if typename is not None:
            if typename!="sizer":
                return (False, 'Only sizers can be pasted here')
            return (True, None)

        if not widget.IS_SIZER:
            return (False, 'Only sizers can be pasted here')
        return (True, None)

    def clipboard_paste(self, clipboard_data):
        "Insert a widget from the clipboard to the current destination"
        if self.widget: size = self.widget.GetSize()
        ret = clipboard._paste(self, 0, clipboard_data)
        if self.widget: self.widget.SetSize(size)
        return ret

    def _properties_changed(self, modified, actions):
        if not modified or "scrollable" in modified:
            if self.scrollable:
                self.WX_CLASS = "wxScrolledWindow"
            else:
                self.WX_CLASS = "wxPanel"
        PanelBase._properties_changed(self, modified, actions)
        ManagedBase._properties_changed(self, modified, actions)

    def get_properties(self, without=set()):
        # return list of properties to be written to XML file
        if not self.scrollable: without.add("scroll_rate")
        return ManagedBase.get_properties(self, without)


class EditTopLevelPanel(PanelBase, TopLevelBase):
    IS_TOPLEVEL_WINDOW = False  # avoid to appear in the "Top Window" property of the app
    WX_CLASS = "wxPanel"
    PROPERTIES = TopLevelBase.PROPERTIES + PanelBase._PROPERTIES + TopLevelBase.EXTRA_PROPERTIES

    def __init__(self, name, parent, index, klass, style='wxTAB_TRAVERSAL'):
        TopLevelBase.__init__(self, name, parent, index, klass)
        PanelBase.__init__(self, style)
        self.skip_on_size = False

    def create_widget(self):
        if self.widget:
            # re-creating -> use old frame
            win = self.widget.GetTopLevelParent()
        else:
            style = wx.DEFAULT_FRAME_STYLE
            if common.pin_design_window: style |= wx.STAY_ON_TOP
            win = wx.Frame( common.main, -1, misc.design_title(self.name), size=(400, 300), style=style )
            import os, compat
            icon = compat.wx_EmptyIcon()
            xpm = os.path.join(config.icons_path, 'panel.png')
            icon.CopyFromBitmap(misc.get_xpm_bitmap(xpm))
            win.SetIcon(icon)
            win.Bind(wx.EVT_CLOSE, self.hide_widget)  # CLOSE event of the frame, not the panel
            if wx.Platform == '__WXMSW__':
                win.CentreOnScreen()

        if self.scrollable:
            self.widget = wx.ScrolledWindow(win, wx.ID_ANY, style=0)
        else:
            self.widget = wx.Panel(win, wx.ID_ANY, style=0)
        self.widget.Bind(wx.EVT_ENTER_WINDOW, self.on_enter)
        self.widget.GetBestSize = self.get_widget_best_size
        #self.widget.SetSize = win.SetSize

    def create(self):
        super(EditTopLevelPanel, self).create()
        if not self.check_prop("size") and self.children and self.children[0].IS_SIZER:
            self.children[0].fit_parent()

    def destroy_widget(self, level):
        # handle containing frame
        win = self.widget and self.widget.GetParent() or None
        super(EditTopLevelPanel, self).destroy_widget(level)
        if win is not None: win.Destroy()

    def hide_widget(self, event=None):
        # this is called from the context menu and from the EVT_CLOSE of the Frame
        self.widget.GetParent().Hide()
        common.app_tree.Collapse(self.item)
        self.design.update_label()

    def on_size(self, event):
        # handle containing frame
        w, h = event.GetSize()
        if self.skip_on_size:
            self.skip_on_size = False
            return
        super(EditTopLevelPanel, self).on_size(event)
        self.skip_on_size = True
        if self.widget.GetParent().GetClientSize() != (w, h):
            self.widget.GetParent().SetClientSize((w+2, h+2))

    def _properties_changed(self, modified, actions):
        if not modified or "scrollable" in modified:
            if self.scrollable:
                self.WX_CLASS = "wxScrolledWindow"
            else:
                self.WX_CLASS = "wxPanel"
        if not modified or "name" in modified:
            if self.widget:
                self.widget.GetParent().SetTitle(misc.design_title(self.name))

        PanelBase._properties_changed(self, modified, actions)
        TopLevelBase._properties_changed(self, modified, actions)

    def get_properties(self, without=set()):
        # return list of properties to be written to XML file
        if not self.scrollable: without.add("scroll_rate")
        return TopLevelBase.get_properties(self, without)


def builder(parent, index):
    "factory function for EditPanel objects"
    name = parent.toplevel_parent.get_next_contained_name('panel_%d')
    with parent.frozen():
        editor = EditPanel(name, parent, index, style='')
        editor.properties["proportion"].set(1)
        editor.properties["flag"].set("wxEXPAND")
        if parent.widget: editor.create()
    return editor


def xml_builder(parser, base, name, parent, index):
    "factory to build EditPanel objects from a XML file"
    return EditPanel(name, parent, index, '')


def xml_toplevel_builder(parser, base, name, parent, index):
    return EditTopLevelPanel( name, parent, index, "Panel", '' )


def initialize():
    "initialization function for the module: returns a wxBitmapButton to be added to the main palette"
    common.widget_classes['EditPanel'] = EditPanel
    common.widgets['EditPanel'] = builder
    common.widgets_from_xml['EditPanel'] = xml_builder

    common.widget_classes['EditScrolledWindow'] = EditPanel
    common.widgets_from_xml['EditScrolledWindow'] = xml_builder

    common.widget_classes['EditTopLevelPanel'] = EditTopLevelPanel
    common.widgets_from_xml['EditTopLevelPanel'] = xml_toplevel_builder
    common.widget_classes['EditTopLevelScrolledWindow'] = EditTopLevelPanel
    common.widgets_from_xml['EditTopLevelScrolledWindow'] = xml_toplevel_builder

    # these are for backwards compatibility (may be removed someday...)
    common.widgets_from_xml['SplitterPane'] = xml_builder
    common.widgets_from_xml['NotebookPane'] = xml_builder
    return common.make_object_button('EditPanel', 'panel.png', tip='Add a Panel/ScrolledWindow')

