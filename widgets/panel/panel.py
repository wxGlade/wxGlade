"""\
wxPanel objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import logging
import wx
import clipboard
import common, config, misc
from tree import Node
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
        self.scroll_rate     = np.ScrollRatePropertyD( "10, 10" )

        if style: self.properties["style"].set(style)

    def finish_widget_creation(self):
        super(PanelBase, self).finish_widget_creation(sel_marker_parent=self.widget)
        if not self.scrollable:
            self.widget.SetScrollRate(0, 0)
        else:
            self.widget.SetScrollRate( *self.properties["scroll_rate"].get_tuple() )
        # this must be done here since ManagedBase.finish_widget_creation normally sets EVT_LEFT_DOWN to update_view
        if not self.widget.Disconnect(-1, -1, wx.wxEVT_LEFT_DOWN):
            self._logger.warning( _("EditPanel: Unable to disconnect the event handler") )
        wx.EVT_LEFT_DOWN(self.widget, self.drop_sizer)

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
        self.widget.SetCursor(wx.NullCursor)
        common.widgets[common.widget_to_add](self, None, None)
        common.adding_widget = common.adding_sizer = False
        common.widget_to_add = None
        common.app_tree.app.saved = False

    def get_widget_best_size(self):
        if self.top_sizer and self.widget.GetSizer():
            self.top_sizer.fit_parent()
            return self.widget.GetSize()
        return wx.ScrolledWindow.GetBestSize(self.widget)

    def properties_modified(self, modified):
        if not modified or "scrollable" in modified:
            if self.scrollable:
                if self.klass == 'wxPanel':
                    self.klass = 'wxScrolledWindow'
                    self.klass_prop.set_value(self.klass)
                self.properties['scroll_rate'].toggle_active(True)
            else:
                if self.klass == 'wxScrolledWindow':
                    self.klass = 'wxPanel'
                    self.klass_prop.set_value(self.klass)
                self.properties['scroll_rate'].toggle_active(False)

        if not modified or "scroll_rate" in modified or "scrollable" in modified:
            if self.widget:
                if self.scrollable:
                    self.widget.SetScrollRate( *self.properties["scroll_rate"].get_tuple() )
                else:
                    self.widget.SetScrollRate(0, 0)
        EditStylesMixin.properties_modified(self, modified)

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

    PROPERTIES = ManagedBase.PROPERTIES + PanelBase._PROPERTIES + ManagedBase.EXTRA_PROPERTIES

    def __init__(self, name, parent, id, sizer, pos, style='wxTAB_TRAVERSAL'):
        ManagedBase.__init__(self, name, 'wxPanel', parent, id, sizer, pos)
        PanelBase.__init__(self, style)

    def create_widget(self):
        self.widget = wx.ScrolledWindow(self.parent.widget, self.id, style=0)
        wx.EVT_ENTER_WINDOW(self.widget, self.on_enter)
        self.widget.GetBestSize = self.get_widget_best_size
        if self.sizer.is_virtual():
            def GetBestSize():
                if self.widget and self.widget.GetSizer():
                    return self.widget.GetSizer().GetMinSize()
                #return wx.Panel.GetBestSize(self.widget)
                return wx.ScrolledWindow.GetBestSize(self.widget)
            self.widget.GetBestSize = GetBestSize

    def set_sizer(self, sizer):
        super(EditPanel, self).set_sizer(sizer)
        if self.top_sizer and self.top_sizer.widget and self.widget:
            self.sizer.set_item(self.pos, size=self.widget.GetBestSize())

    def _create_popup_menu(self, widget=None):
        COPY_ID, REMOVE_ID, CUT_ID = [wx.NewId() for i in range(3)]
        menu = misc.wxGladePopupMenu(self.name)
        misc.append_menu_item(menu, REMOVE_ID, _('Remove Panel\tDel'),  wx.ART_DELETE)
        misc.append_menu_item(menu, COPY_ID,   _('Copy\tCtrl+C'), wx.ART_COPY)
        misc.append_menu_item(menu, CUT_ID,    _('Cut\tCtrl+X'),  wx.ART_CUT)

        if widget is None: widget = self.widget
        wx.EVT_MENU(widget, REMOVE_ID, misc.exec_after(self.remove))
        wx.EVT_MENU(widget, COPY_ID, misc.exec_after(self.clipboard_copy))
        wx.EVT_MENU(widget, CUT_ID, misc.exec_after(self.clipboard_cut))

        PASTE_ID = wx.NewId()
        i = misc.append_menu_item(menu, PASTE_ID, _('Paste Sizer\tCtrl+V'), wx.ART_PASTE)
        wx.EVT_MENU(widget, PASTE_ID, misc.exec_after(self.clipboard_paste))
        if self.top_sizer is not None: i.Enable(False)

        PREVIEW_ID = wx.NewId()
        menu.AppendSeparator()
        misc.append_menu_item(menu, PREVIEW_ID, _('Preview'))
        wx.EVT_MENU(widget, PREVIEW_ID, misc.exec_after(self.preview_parent))
        
        self._rmenu = (menu, widget) # store for destryoing and unbinding
        return menu

    def check_compatibility(self, widget):
        "check whether widget can be pasted"
        import edit_sizers
        if not isinstance(widget, edit_sizers.Sizer):
            self._logger.warning(_('Only sizers can be pasted here'))
            return False
        #if self.sizer is not None:
        if self.top_sizer:
            self._logger.warning(_('Sizer set already'))
            return False
        return True
        
    def clipboard_paste(self, event=None, clipboard_data=None):
        "Insert a widget from the clipboard to the current destination"
        import xml_parse
        if self.widget is not None:
            size = self.widget.GetSize()
        try:
            if clipboard.paste(self, None, 0, clipboard_data):
                common.app_tree.app.saved = False
                if self.widget is not None:
                    self.widget.SetSize(size)
        except xml_parse.XmlParsingError:
            import os
            if 'WINGDB_ACTIVE' in os.environ: raise
            self._logger.warning(_('Only sizers can be pasted here'))

    def set_scrollable(self, *args, **kwargs):
        raise ValueError("XXX change") # -> properties_modified

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
    _is_toplevel = False  # used to avoid to appear in the "Top Window" property of the app

    PROPERTIES = TopLevelBase.PROPERTIES + PanelBase._PROPERTIES + TopLevelBase.EXTRA_PROPERTIES

    def __init__(self, name, parent, id, klass='wxPanel', style='wxTAB_TRAVERSAL'):
        TopLevelBase.__init__(self, name, klass, parent, id)
        PanelBase.__init__(self, style)
        self.base = 'wxPanel'
        self.skip_on_size = False

    def create_widget(self):
        win = wx.Frame( common.palette, -1, misc.design_title(self.name), size=(400, 300) )
        import os
        icon = wx.EmptyIcon()
        xpm = os.path.join(config.icons_path, 'panel.xpm')
        icon.CopyFromBitmap(misc.get_xpm_bitmap(xpm))
        win.SetIcon(icon)
        #self.widget = wx.Panel(win, self.id, style=0)
        self.widget = wx.ScrolledWindow(win, self.id, style=0)
        wx.EVT_ENTER_WINDOW(self.widget, self.on_enter)
        self.widget.GetBestSize = self.get_widget_best_size
        #self.widget.SetSize = win.SetSize
        wx.EVT_CLOSE(win, self.hide_widget)
        if wx.Platform == '__WXMSW__':
            win.CentreOnScreen()

    def create(self):
        oldval = self.size
        super(EditTopLevelPanel, self).create()
        if self.widget:
            if not self.properties['size'].is_active() and self.top_sizer:
                self.top_sizer.fit_parent()
        self.set_size(oldval)

    def hide_widget(self, *args):
        super(EditTopLevelPanel, self).hide_widget(*args)
        self.widget.GetParent().Hide()

    def set_name(self, name):
        super(EditTopLevelPanel, self).set_name(name)
        if self.widget:
            self.widget.GetParent().SetTitle(misc.design_title(self.name))

    def delete(self):
        win = None
        if self.widget:
            win = self.widget.GetParent()
        super(EditTopLevelPanel, self).delete()
        if win is not None:
            win.Destroy()

    def on_size(self, event):
        w, h = event.GetSize()
        if self.skip_on_size:
            self.skip_on_size = False
            return
        super(EditTopLevelPanel, self).on_size(event)
        self.skip_on_size = True
        if self.widget.GetParent().GetClientSize() != (w, h):
            self.widget.GetParent().SetClientSize((w+2, h+2))

    def set_scrollable(self, value):
        raise ValueError("XXX change") # -> properties_modified

    def properties_modified(self, modified):
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

        PanelBase.properties_modified(self, modified)
        TopLevelBase.properties_modified(self, modified)

    def get_properties(self, without=set()):
        # return list of properties to be written to XML file
        if not self.scrollable: without.add("scroll_rate")
        return TopLevelBase.get_properties(self, without)


def builder(parent, sizer, pos, number=[1]):
    "factory function for EditPanel objects"
    name = 'panel_%d' % number[0]
    while common.app_tree.has_name(name):
        number[0] += 1
        name = 'panel_%d' % number[0]
    panel = EditPanel(name, parent, wx.NewId(), sizer, pos, style='')
    node = Node(panel)
    panel.node = node
    panel.properties["proportion"].set(1)
    panel.properties["flag"].set("wxEXPAND")
    if parent.widget: panel.create()
    common.app_tree.insert(node, sizer.node, pos-1)
    sizer.set_item(panel.pos, 1, wx.EXPAND)


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    "factory to build EditPanel objects from a XML file"
    from xml_parse import XmlParsingError
    try:
        name = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    if not sizer or not sizeritem:
        raise XmlParsingError(_("sizer or sizeritem object cannot be None"))
    panel = EditPanel(name, parent, wx.NewId(), sizer, pos, style='')
    sizer.set_item(panel.pos, proportion=sizeritem.proportion, flag=sizeritem.flag, border=sizeritem.border)
    node = Node(panel)
    panel.node = node
    if pos is None:
        common.app_tree.add(node, sizer.node)
    else:
        common.app_tree.insert(node, sizer.node, pos-1)
    return panel


def xml_toplevel_builder(attrs, parent, sizer, sizeritem, pos=None):
    from xml_parse import XmlParsingError
    try:
        label = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    panel = EditTopLevelPanel( label, parent, wx.NewId(), style='' )
    node = Node(panel)
    panel.node = node
    common.app_tree.add(node)
    return panel


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

