"""\
wxFrame objects (incl. wxMenuBar, wxToolBar and wxStatusBar)

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016-2019 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
import os
import common, config, misc, compat
from tree import Node
import new_properties as np
from edit_windows import TopLevelBase, EditStylesMixin
from gui_mixins import BitmapMixin


class EditFrame(TopLevelBase, EditStylesMixin, BitmapMixin):
    _PROPERTIES =["Widget", "title", "icon", "centered", "sizehints","menubar", "toolbar", "statusbar", "style"]
    PROPERTIES = TopLevelBase.PROPERTIES + _PROPERTIES
    _PROPERTY_HELP   = { 'icon':'Icon for this window.' }
    _PROPERTY_LABELS = { "sizehints":'Set Size Hints', "menubar":'Has MenuBar', "toolbar":'Has ToolBar',
                         "statusbar":'Has StatusBar' }

    def __init__(self, name, parent, id, title, style=wx.DEFAULT_FRAME_STYLE, klass='wxFrame'): #XXX style is not used
        TopLevelBase.__init__(self, name, klass, parent, id, title=title)
        self.base = 'wxFrame'
        EditStylesMixin.__init__(self)
        self.properties["style"].set(style)

        # initialise instance properties
        self.icon      = np.BitmapPropertyD("")
        self.centered  = np.CheckBoxProperty(False, default_value=False)
        self.sizehints = np.CheckBoxProperty(False, default_value=False)
        self.menubar   = np.CheckBoxProperty(False, default_value=False)
        self.toolbar   = np.CheckBoxProperty(False, default_value=False)
        if "statusbar" in self.PROPERTIES:
            self.statusbar = np.CheckBoxProperty(False, default_value=False)
            self._statusbar = None
        else:
            self.statusbar = None
        self._menubar = self._toolbar = None  # these properties will hold the EditMenubar instances etc.

    def create_widget(self):
        if self.parent:
            parent = self.parent.widget
        else:
            #parent = common.palette
            parent = None
        style = self.style
        if common.pin_design_window: style |= wx.STAY_ON_TOP
        self.widget = wx.Frame(parent, self.id, self.title, style=style)
        self._set_widget_icon()

    def finish_widget_creation(self):
        # add menu, status and tool bar
        TopLevelBase.finish_widget_creation(self)
        if not self.properties['size'].is_active():
            self.widget.SetSize((400, 300))
        if wx.Platform == '__WXMSW__':
            self.widget.CenterOnScreen()
        if self.menubar and self._menubar.widget:
            self.widget.SetMenuBar(self._menubar.widget)
        if self.statusbar and self._statusbar.widget:
            self.widget.SetStatusBar(self._statusbar.widget)
        if self.toolbar and self._toolbar.widget:
            self.widget.SetToolBar(self._toolbar.widget)

    def remove(self, *args):
        # remove menu, status and tool bar
        if self.menubar:
            self._menubar = self._menubar.remove(gtk_do_nothing=True)
        if self.statusbar:
            self._statusbar = self._statusbar.remove(do_nothing=True)
        if self.toolbar:
            self._toolbar = self._toolbar.remove(do_nothing=True)
        TopLevelBase.remove(self, *args)

    def _set_widget_icon(self):
        if self.icon:
            bitmap = self.get_preview_obj_bitmap(self.icon.strip())
        else:
            xpm = os.path.join(config.icons_path, 'frame.xpm')
            bitmap = misc.get_xpm_bitmap(xpm)

        icon = compat.wx_EmptyIcon()
        icon.CopyFromBitmap(bitmap)
        self.widget.SetIcon(icon)

    def _set_menu_bar(self):
        if self.menubar:
            # create a MenuBar
            from menubar import EditMenuBar
            self._menubar = EditMenuBar(self.name + '_menubar', 'wxMenuBar', self)
            self._menubar.node = Node(self._menubar)
            common.app_tree.add(self._menubar.node, self.node)
            if self.widget: self._menubar.create()
        else:
            # remove
            if self._menubar is None: return
            self._menubar = self._menubar.remove()

    def _set_status_bar(self):
        if self.statusbar:
            # create a StatusBar
            from statusbar import EditStatusBar
            self._statusbar = EditStatusBar(self.name + '_statusbar', 'wxStatusBar', self)
            if self.widget: self._statusbar.create()
        else:
            # remove
            if self._statusbar is None: return
            self._statusbar = self._statusbar.remove()
        if self.widget:
            # this is needed at least on win32
            wx.PostEvent( self.widget, wx.SizeEvent(self.widget.GetSize(), self.widget.GetId()) )

    def _set_tool_bar(self):
        if self.toolbar:
            # create a ToolBar
            from toolbar import EditToolBar
            self._toolbar = EditToolBar(self.name + '_toolbar', 'wxToolBar', self)
            self._toolbar.node = Node(self._toolbar)
            common.app_tree.add(self._toolbar.node, self.node)

            if self.widget: self._toolbar.create()
        else:
            # remove
            if self._toolbar is None: return
            self._toolbar = self._toolbar.remove()

    def properties_changed(self, modified):
        if not modified or "icon" in modified and self.widget: self._set_widget_icon()

        if not modified or "menubar" in modified:   self._set_menu_bar()
        if not modified or "statusbar" in modified: self._set_status_bar()
        if not modified or "toolbar" in modified:   self._set_tool_bar()

        TopLevelBase.properties_changed(self, modified)
        EditStylesMixin.properties_changed(self, modified)



class EditMDIChildFrame(EditFrame):
    _is_toplevel_window = False  # avoid to appear in the "Top Window" property of the app
    PROPERTIES = [p for p in EditFrame.PROPERTIES if p!="statusbar"]
    #def __init__(self, *args, **kwds):
        #EditFrame.__init__(self, *args, **kwds)
        #self.base = 'wxFrame' # XXX is this correct?



def builder(parent, sizer, pos, klass=None, base=None, name=None):
    "factory function for EditFrame objects"
    if klass is None or base is None:
        import window_dialog
        base_classes = ['wxFrame', 'wxMDIChildFrame']
        klass = 'wxFrame' if common.app_tree.app.language.lower()=='xrc' else 'MyFrame'
        
        dialog = window_dialog.WindowDialog(klass, base_classes, 'Select frame class', True)
        res = dialog.show()
        dialog.Destroy()
        if res is None: return None
        klass, base = res
        name = dialog.get_next_name("frame")

    if base == "wxFrame":
        base_class = EditFrame
    else:
        base_class = EditMDIChildFrame
    frame = base_class(name, parent, wx.NewId(), name, "wxDEFAULT_FRAME_STYLE", klass=klass)
    frame.properties['size'].set( (400,300), activate=True )
    node = Node(frame)
    frame.node = node
    common.app_tree.add(node)
    frame.create()
    frame.widget.Show()
    frame.design.update_label()

    # add a default vertical sizer to the frame
    import edit_sizers
    edit_sizers._builder(frame, None, 0)
    # now select the frame's node in the tree
    common.app_tree.select_item(node)

    import clipboard
    frame.drop_target = clipboard.DropTarget(frame)
    frame.widget.SetDropTarget(frame.drop_target)

    if wx.Platform == '__WXMSW__':
        #frame.widget.CenterOnScreen()
        frame.widget.Raise()


def _make_builder(base_class):
    def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
        from xml_parse import XmlParsingError
        try:
            label = attrs['name']
        except KeyError:
            raise XmlParsingError(_("'name' attribute missing"))
        if attrs.input_file_version and attrs.input_file_version<(0,8):
            # backwards compatibility
            style = "wxDEFAULT_FRAME_STYLE"
        else:
            style = 0
        frame = base_class(label, parent, wx.NewId(), "", style)
        node = Node(frame)
        frame.node = node
        common.app_tree.add(node)
        return frame
    return xml_builder


def initialize():
    "initialization function for the module: returns a wx.BitmapButton to be added to the main palette"
    cwx = common.widgets_from_xml
    cwx['EditFrame']         = _make_builder(EditFrame)
    cwx['EditMDIChildFrame'] = _make_builder(EditMDIChildFrame)

    common.widgets['EditFrame'] = builder

    from tree import WidgetTree
    import os.path
    WidgetTree.images['EditMDIChildFrame'] = os.path.join( config.icons_path, 'frame.xpm' )
    return common.make_object_button('EditFrame', 'frame.xpm', 1)
