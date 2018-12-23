"""\
wxPanel objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016-2018 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import logging
import wx
import clipboard
import common, compat, config, misc
import new_properties as np
from edit_windows import ManagedBase, TopLevelBase, EditStylesMixin



class PanelBase(EditStylesMixin):
    "Class PanelBase"
    _custom_base_classes = True

    _PROPERTIES = ["Widget", "no_custom_class", "style", "scrollable", "scroll_rate"]
    _PROPERTY_LABELS = {'no_custom_class':"Don't generate code for this class"}
    _PROPERTY_HELP = {'no_custom_class':'If this is a custom class, setting this property prevents wxGlade\n'
                                        'from generating the class definition code'}

    def __init__(self, style='wxTAB_TRAVERSAL'):
        "Class to handle wxPanel objects"
        # initialise instance logger
        self._logger = logging.getLogger(self.__class__.__name__)

        # initialise instance
        EditStylesMixin.__init__(self, 'wxPanel')
        self.top_sizer = None  # sizer to handle the layout of children

        # initialise properties
        self.no_custom_class = np.CheckBoxProperty(False, default_value=False)
        self.scrollable      = np.CheckBoxProperty(False, default_value=False)
        self.scroll_rate = prop = np.IntPairPropertyD( "10, 10" )
        prop.set_blocked(True)

        if style: self.properties["style"].set(style)

    def finish_widget_creation(self):
        super(PanelBase, self).finish_widget_creation(sel_marker_parent=self.widget)
        if self.scrollable:
            self.widget.SetScrollRate( *self.properties["scroll_rate"].get_tuple() )
        # this must be done here since ManagedBase.finish_widget_creation normally sets EVT_LEFT_DOWN to update_view
        if not self.widget.Disconnect(-1, -1, wx.wxEVT_LEFT_DOWN):
            self._logger.warning( _("EditPanel: Unable to disconnect the event handler") )
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
        if not self.top_sizer and common.adding_sizer:
            self.widget.SetCursor(wx.CROSS_CURSOR)
        else:
            self.widget.SetCursor(wx.STANDARD_CURSOR)

    def set_sizer(self, sizer):
        self.top_sizer = sizer
        if self.top_sizer and self.top_sizer.widget and self.widget:
            self.widget.SetAutoLayout(True)
            self.widget.SetSizer(self.top_sizer.widget)
            self.widget.Layout()
        elif self.top_sizer is None and self.widget:
            self.widget.SetSizer(None)

    def drop_sizer(self, event=None):
        if self.top_sizer or not common.adding_sizer:
            self.on_set_focus(event)  # default behaviour: call show_properties
            return
        if self.widget: self.widget.SetCursor(wx.NullCursor)
        new_widget = common.widgets[common.widget_to_add](self, None)
        if new_widget is None: return
        misc.rebuild_tree(new_widget)
        if event is None or not misc.event_modifier_copy(event):
            common.adding_widget = common.adding_sizer = False
            common.widget_to_add = None

    def check_drop_compatibility(self):
        if self.top_sizer:
            return (False, 'Sizer already set for this panel')
        #if common.adding_sizer:
        return (True, None)
        #return (False, 'Only sizers can be added here')

    def get_widget_best_size(self):
        if self.top_sizer and self.widget.GetSizer():
            self.top_sizer.fit_parent()
            return self.widget.GetSize()
        return self.widget.__class__.GetBestSize(self.widget)

    def properties_changed(self, modified):
        if not modified or "scrollable" in modified:
            if self.scrollable:
                if self.klass == 'wxPanel':
                    self.properties["class"].set('wxScrolledWindow')
                self.properties['scroll_rate'].toggle_active(True)
                self.properties['scroll_rate'].set_blocked(False)
            else:
                if self.klass == 'wxScrolledWindow':
                    self.properties["class"].set('wxPanel')
                self.properties['scroll_rate'].toggle_active(False)
                self.properties['scroll_rate'].set_blocked(True)

        if self.widget and modified:
            if "scrollable" in modified and self.properties["scrollable"].previous_value!=self.scrollable:
                self.recreate_widget()
            elif "scroll_rate" in modified and self.scrollable and isinstance(self.widget, wx.ScrolledWindow):
                self.widget.SetScrollRate( *self.properties["scroll_rate"].get_tuple() )
        EditStylesMixin.properties_changed(self, modified)

    def __getstate__(self):
        state = self.__dict__.copy()
        del state['_logger']
        return state

    def __setstate__(self, state):
        self.__dict__.update(state)

        # re-initialise logger instance deleted from __getstate__
        self._logger = logging.getLogger(self.__class__.__name__)



class EditPanel(PanelBase, ManagedBase):
    "Class to handle wxPanel objects"
    WX_CLASS = "wxPanel"
    PROPERTIES = ManagedBase.PROPERTIES + PanelBase._PROPERTIES + ManagedBase.EXTRA_PROPERTIES

    def __init__(self, name, parent, pos, style='wxTAB_TRAVERSAL'):
        ManagedBase.__init__(self, name, 'wxPanel', parent, pos)
        PanelBase.__init__(self, style)

    def create_widget(self):
        # to be done: use ScrolledWindow only if scrolling is required
        if self.scrollable:
            self.widget = wx.ScrolledWindow(self.parent_window.widget, self.id, style=0)
        else:
            self.widget = wx.Panel(self.parent_window.widget, self.id, style=0)
        self.widget.Bind(wx.EVT_ENTER_WINDOW, self.on_enter)
        self.widget.GetBestSize = self.get_widget_best_size
        #if self.sizer.is_virtual():
        if not self.parent.IS_SIZER:
            def GetBestSize():
                if self.widget and self.widget.GetSizer():
                    return self.widget.GetSizer().GetMinSize()
                return self.widget.__class__.GetBestSize(self)
            self.widget.GetBestSize = GetBestSize

    def set_sizer(self, sizer):
        super(EditPanel, self).set_sizer(sizer)
        if self.top_sizer and self.top_sizer.widget and self.widget:
            #self.sizer.set_item_best_size(self, size=self.widget.GetBestSize())
            self.children[0].set_item_best_size(self, size=self.widget.GetBestSize())

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
        if self.top_sizer is not None or not clipboard.check("sizer"): i.Enable(False)
        menu.AppendSeparator()

        #if self.sizer: self.sizer._add_popup_menu_items(menu, self, widget)
        if hasattr(self.parent, "_add_popup_menu_items"):
            self.parent._add_popup_menu_items(menu, self, widget)

        i = misc.append_menu_item(menu, -1, _('Preview'))
        misc.bind_menu_item_after(widget, i, self.preview_parent)

        return menu

    def check_compatibility(self, widget, typename=None, report=True):
        "check whether widget can be pasted"
        if typename is not None:
            #if typename!="sizer":
                #return (False, 'Only sizers can be pasted here')
            if self.top_sizer:
                return (False, 'Sizer set already')
            return (True, None)

        import edit_sizers
        #if not isinstance(widget, edit_sizers.Sizer):
            #return (False, 'Only sizers can be pasted here')
        #if self.sizer is not None:
        if self.top_sizer:
            return (False, 'Sizer set already')
        return (True, None)

    def clipboard_paste(self, clipboard_data):
        "Insert a widget from the clipboard to the current destination"
        if self.widget: size = self.widget.GetSize()
        ret = clipboard._paste(self, None, 0, clipboard_data)
        if self.widget: self.widget.SetSize(size)
        return ret

    def properties_changed(self, modified):
        if not modified or "scrollable" in modified:
            if self.scrollable:
                # 2003-06-26 ALB: change the "class name", to allow code generation
                # for a wxScrolledWindow (see Node.write and common.class_names usage in xml_parse.py)
                self._classname = 'EditScrolledWindow'
            else:
                self._classname = self.__class__.__name__
        PanelBase.properties_changed(self, modified)
        ManagedBase.properties_changed(self, modified)

    def get_properties(self, without=set()):
        # return list of properties to be written to XML file
        if not self.scrollable: without.add("scroll_rate")
        return ManagedBase.get_properties(self, without)


class EditTopLevelPanel(PanelBase, TopLevelBase):
    IS_TOPLEVEL_WINDOW = False  # avoid to appear in the "Top Window" property of the app
    WX_CLASS = "TopLevelPanel"
    PROPERTIES = TopLevelBase.PROPERTIES + PanelBase._PROPERTIES + TopLevelBase.EXTRA_PROPERTIES

    def __init__(self, name, parent, klass='wxPanel', style='wxTAB_TRAVERSAL'):
        TopLevelBase.__init__(self, name, klass, parent)
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
            xpm = os.path.join(config.icons_path, 'panel.xpm')
            icon.CopyFromBitmap(misc.get_xpm_bitmap(xpm))
            win.SetIcon(icon)
            win.Bind(wx.EVT_CLOSE, self.hide_widget)  # CLOSE event of the frame, not the panel
            if wx.Platform == '__WXMSW__':
                win.CentreOnScreen()

        if self.scrollable:
            self.widget = wx.ScrolledWindow(win, self.id, style=0)
        else:
            self.widget = wx.Panel(win, self.id, style=0)
        self.widget.Bind(wx.EVT_ENTER_WINDOW, self.on_enter)
        self.widget.GetBestSize = self.get_widget_best_size
        #self.widget.SetSize = win.SetSize

    def create(self):
        # XXX refactor this
        size_p = self.properties['size']
        oldval = size_p.get()
        super(EditTopLevelPanel, self).create()
        if self.widget:
            if not size_p.is_active() and self.top_sizer:
                self.top_sizer.fit_parent()
        if size_p.is_active() and oldval!=size_p.get():
            size_p.set(oldval)
            self.set_size()

    def hide_widget(self, event=None):
        # this is called from the context menu and from the EVT_CLOSE of the Frame
        self.widget.GetParent().Hide()
        common.app_tree.expand(self.node, False)
        self.design.update_label()

    def set_name(self, name):
        # handle containing frame
        super(EditTopLevelPanel, self).set_name(name)
        if self.widget:
            self.widget.GetParent().SetTitle(misc.design_title(self.name))

    def destroy_widget(self):
        # handle containing frame
        win = None
        if self.widget:
            win = self.widget.GetParent()
        super(EditTopLevelPanel, self).destroy_widget()
        if win is not None:
            win.Destroy()

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

    def properties_changed(self, modified):
        if not modified or "scrollable" in modified:
            if self.scrollable:
                # 2003-06-26 ALB: change the "class name", to allow code generation
                # for a wxScrolledWindow (see Node.write and common.class_names usage in xml_parse.py)
                self._classname = 'EditTopLevelScrolledWindow'
            else:
                self._classname = self.__class__.__name__
        if not modified or "name" in modified:
            if self.widget:
                self.widget.GetParent().SetTitle(misc.design_title(self.name))

        PanelBase.properties_changed(self, modified)
        TopLevelBase.properties_changed(self, modified)

    def get_properties(self, without=set()):
        # return list of properties to be written to XML file
        if not self.scrollable: without.add("scroll_rate")
        return TopLevelBase.get_properties(self, without)


def builder(parent, pos):
    "factory function for EditPanel objects"
    name = common.root.get_next_name('panel_%d', parent)
    with parent.frozen():
        editor = EditPanel(name, parent, pos, style='')
        editor.properties["proportion"].set(1)
        editor.properties["flag"].set("wxEXPAND")
        if parent.widget: editor.create()
    return editor


def xml_builder(attrs, parent, pos=None):
    "factory to build EditPanel objects from a XML file"
    from xml_parse import XmlParsingError
    try:
        name = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    return EditPanel(name, parent, pos=pos, style='')


def xml_toplevel_builder(attrs, parent, pos=None):
    from xml_parse import XmlParsingError
    try:
        label = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    return EditTopLevelPanel( label, parent, style='' )


def initialize():
    "initialization function for the module: returns a wxBitmapButton to be added to the main palette"
    common.widgets['EditPanel'] = builder
    common.widgets_from_xml['EditPanel'] = xml_builder

    #common.widgets['EditScrolledWindow'] = builder
    common.widgets_from_xml['EditScrolledWindow'] = xml_builder

    common.widgets_from_xml['EditTopLevelPanel'] = xml_toplevel_builder
    common.widgets_from_xml['EditTopLevelScrolledWindow'] = xml_toplevel_builder
    from tree import WidgetTree
    import os.path
    icon = os.path.join(config.icons_path, 'panel.xpm')
    WidgetTree.images['EditTopLevelPanel'] = icon
    WidgetTree.images['EditScrolledWindow'] = icon
    WidgetTree.images['EditTopLevelScrolledWindow'] = icon

    # these are for backwards compatibility (may be removed someday...)
    common.widgets_from_xml['SplitterPane'] = xml_builder
    WidgetTree.images['SplitterPane'] = os.path.join( config.icons_path, 'panel.xpm' )
    common.widgets_from_xml['NotebookPane'] = xml_builder
    WidgetTree.images['NotebookPane'] = os.path.join( config.icons_path, 'panel.xpm' )
    return common.make_object_button('EditPanel', 'panel.xpm', tip='Add a Panel/ScrolledWindow')

