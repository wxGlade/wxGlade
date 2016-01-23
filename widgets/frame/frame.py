"""\
wxFrame objects (incl. wxMenuBar, wxToolBar and wxStatusBar)

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import os
import math
import wx

import common
import compat
import config
import misc
from tree import Tree
from widget_properties import *
from edit_windows import TopLevelBase, EditStylesMixin
from gui_mixins import BitmapMixin


class EditFrame(TopLevelBase, EditStylesMixin, BitmapMixin):

    def __init__(self, name, parent, id, title, property_window,
                 style=wx.DEFAULT_FRAME_STYLE, show=True, klass='wxFrame'):
        TopLevelBase.__init__(self, name, klass, parent, id,
                              property_window, show=show, title=title)
        self.base = 'wxFrame'
        EditStylesMixin.__init__(self)

        # initialise instance variables
        self.centered = False
        self.icon = ''
        self.style = style
        self.menubar = None
        self.sizehints = False
        self.statusbar = None
        self.toolbar = None

        # initialise properties remaining staff
        access = self.access_functions
        properties = self.properties

        # style property
        access['style'] = (self.get_style, self.set_style)
        properties['style'] = CheckListProperty(self, 'style', self.widget_writer)

        # menubar property
        access['menubar'] = (self.get_menubar, self.set_menubar)
        properties['menubar'] = CheckBoxProperty(
            self, 'menubar', label=_('Has MenuBar'))

        # statusbar property
        access['statusbar'] = (self.get_statusbar, self.set_statusbar)
        properties['statusbar'] = CheckBoxProperty(
            self, 'statusbar', label=_('Has StatusBar'))

        # toolbar property
        access['toolbar'] = (self.get_toolbar, self.set_toolbar)
        properties['toolbar'] = CheckBoxProperty(
            self, 'toolbar', label=_('Has ToolBar'))

        # icon property
        access['icon'] = (self.get_icon, self.set_icon)
        properties['icon'] = FileDialogProperty(
            self, 'icon', style=wx.FD_OPEN | wx.FD_FILE_MUST_EXIST,
            label=_("icon"))
        properties['icon'].set_tooltip(self.bitmap_tooltip_text)

        # centered property
        access['centered'] = (self.get_centered, self.set_centered)
        properties['centered'] = CheckBoxProperty(
            self, 'centered', label=_("centered"))

        # size hints property
        access['sizehints'] = (self.get_sizehints, self.set_sizehints)
        properties['sizehints'] = CheckBoxProperty(
            self, 'sizehints', label=_('Set Size Hints'))

    def create_widget(self):
        if self.parent:
            parent = self.parent.widget
        else:
            parent = common.palette
        self.widget = wx.Frame(parent, self.id, self.get_title())
        self.set_icon(self.icon)

    def finish_widget_creation(self):
        TopLevelBase.finish_widget_creation(self)
        if not self.properties['size'].is_active():
            self.widget.SetSize((400, 300))
        if wx.Platform == '__WXMSW__':
            self.widget.CenterOnScreen()
        if self.menubar and self.menubar.widget:
            self.widget.SetMenuBar(self.menubar.widget)
        if self.statusbar and self.statusbar.widget:
            self.widget.SetStatusBar(self.statusbar.widget)
        if self.toolbar and self.toolbar.widget:
            self.widget.SetToolBar(self.toolbar.widget)

    def create_properties(self):
        TopLevelBase.create_properties(self)
        prop = self.properties
        panel = wx.ScrolledWindow(self.notebook, -1, style=wx.TAB_TRAVERSAL)
        prop['title'].display(panel)
        prop['icon'].display(panel)
        prop['centered'].display(panel)
        prop['sizehints'].display(panel)
        prop['menubar'].display(panel)
        prop['toolbar'].display(panel)
        try:
            sbprop = prop['statusbar']
            sbprop.display(panel)
        except KeyError:
            sbprop = None
        prop['style'].display(panel)

        szr = wx.BoxSizer(wx.VERTICAL)
        szr.Add(prop['title'].panel, 0, wx.EXPAND)
        szr.Add(prop['icon'].panel, 0, wx.EXPAND)
        szr.Add(prop['centered'].panel, 0, wx.EXPAND)
        szr.Add(prop['sizehints'].panel, 0, wx.EXPAND)
        szr.Add(prop['menubar'].panel, 0, wx.EXPAND)
        szr.Add(prop['toolbar'].panel, 0, wx.EXPAND)
        if sbprop:
            szr.Add(sbprop.panel, 0, wx.EXPAND)
        szr.Add(prop['style'].panel, 0, wx.EXPAND)
        panel.SetAutoLayout(True)
        compat.SizerItem_SetSizer(panel, szr)
        szr.Fit(panel)
        self.notebook.AddPage(panel, 'Widget')
        w, h = panel.GetClientSizeTuple()
        panel.SetScrollbars(5, 5, int(math.ceil(w/5.0)), int(math.ceil(h/5.0)))

    def get_menubar(self):
        return self.menubar is not None

    def set_menubar(self, value):
        if value:
            from menubar import EditMenuBar
            self.menubar = EditMenuBar(self.name + '_menubar', 'wxMenuBar',
                                       self, common.property_panel)
            self.menubar.node = Tree.Node(self.menubar)
            common.app_tree.add(self.menubar.node, self.node)

            if self.widget:
                self.menubar.show_widget(True)
                self.menubar.show_properties()
        else:
            self.menubar = self.menubar.remove()
            self.show_properties(None)

    def get_statusbar(self):
        return self.statusbar is not None

    def set_statusbar(self, value):
        if value:
            from statusbar import EditStatusBar
            self.statusbar = EditStatusBar(self.name + '_statusbar',
                                           'wxStatusBar',
                                           self, common.property_panel)
            if self.widget:
                self.statusbar.show_widget(True)
                self.statusbar.show_properties()
        else:
            self.statusbar = self.statusbar.remove()
            self.show_properties(None)
        if self.widget:
            # this is needed at least on win32
            wx.PostEvent(self.widget, wx.SizeEvent(self.widget.GetSize(),
                                                   self.widget.GetId()))

    def get_toolbar(self):
        return self.toolbar is not None

    def set_toolbar(self, value):
        if value:
            from toolbar import EditToolBar
            self.toolbar = EditToolBar(self.name + '_toolbar', 'wxToolBar',
                                       self, common.property_panel)
            self.toolbar.node = Tree.Node(self.toolbar)
            common.app_tree.add(self.toolbar.node, self.node)

            if self.widget:
                self.toolbar.show_widget(True)
                self.toolbar.show_properties()
        else:
            self.toolbar = self.toolbar.remove()
            self.show_properties(None)

    def remove(self, *args):
        if self.menubar:
            self.menubar = self.menubar.remove(gtk_do_nothing=True)
        if self.statusbar:
            self.statusbar = self.statusbar.remove(do_nothing=True)
        if self.toolbar:
            self.toolbar = self.toolbar.remove(do_nothing=True)
        TopLevelBase.remove(self, *args)

    def get_icon(self):
        return self.icon

    def set_icon(self, value):
        self.icon = value.strip()
        if self.widget:
            if self.icon:
                bitmap = self.get_preview_obj_bitmap(self.icon)
            else:
                xpm = os.path.join(config.icons_path, 'frame.xpm')
                bitmap = misc.get_xpm_bitmap(xpm)

            icon = wx.EmptyIcon()
            icon.CopyFromBitmap(bitmap)
            self.widget.SetIcon(icon)

    def get_centered(self):
        return self.centered

    def set_centered(self, value):
        try: self.centered = bool(int(value))
        except ValueError: pass

    def get_sizehints(self):
        return self.sizehints

    def set_sizehints(self, value):
        try: self.sizehints = bool(int(value))
        except ValueError: pass

# end of class EditFrame


class EditMDIChildFrame(EditFrame):
    _is_toplevel = False  # used to avoid to appear in the "Top Window" property
                         # of the app

    def __init__(self, *args, **kwds):
        EditFrame.__init__(self, *args, **kwds)
        self.base = 'wxFrame'
        del self.properties['statusbar']

# end of class EditMDIChildFrame


def builder(parent, sizer, pos, number=[0]):
    """\
    factory function for EditFrame objects.
    """
    class Dialog(wx.Dialog):
        def __init__(self):
            wx.Dialog.__init__(self, None, -1, _('Select frame class'))
            if common.app_tree.app.get_language().lower() == 'xrc':
                self.klass = 'wxFrame'
            else:
                if not number[0]: self.klass = 'MyFrame'
                else: self.klass = 'MyFrame%s' % number[0]
                number[0] += 1
            self.base = 0
            base_prop = RadioProperty(self, 'base class', self,
                                      ['wxFrame', 'wxMDIChildFrame'], label=_("base class"))
            klass_prop = TextProperty(self, 'class', self, label=_("class"))
            szr = wx.BoxSizer(wx.VERTICAL)
            szr.Add(base_prop.panel, 0, wx.ALL|wx.EXPAND, 5)
            szr.Add(klass_prop.panel, 0, wx.EXPAND)
            btnbox = wx.BoxSizer(wx.HORIZONTAL)
            btnOK = wx.Button(self, wx.ID_OK, _('OK'))
            btnCANCEL = wx.Button(self, wx.ID_CANCEL, _('Cancel'))
            btnbox.Add(btnOK, 0, wx.ALL, 3)
            btnbox.Add(btnCANCEL, 0, wx.ALL, 3)
            btnOK.SetFocus()
            szr.Add(btnbox, 0, wx.ALL|wx.ALIGN_CENTER, 3)
            self.SetAutoLayout(True)
            self.SetSizer(szr)
            szr.Fit(self)
            self.CenterOnScreen()

        def undo(self):
            if number[0] > 0:
                number[0] -= 1

        def __getitem__(self, value):
            if value == 'class':
                def set_klass(c): self.klass = c
                return lambda : self.klass, set_klass
            else:
                def set_base(b): self.base = b
                return lambda : self.base, set_base
    # end of inner class

    dialog = Dialog()
    res = dialog.ShowModal()
    klass = dialog.klass
    base = dialog.base
    dialog.Destroy()
    if res != wx.ID_OK:
        if number[0] > 0:
            number[0] -= 1
        return

    label = 'frame_%d' % (number[0] or 1)
    while common.app_tree.has_name(label):
        number[0] += 1
        label = 'frame_%d' % number[0]
    if base == 0:
        base_class = EditFrame
    else:
        base_class = EditMDIChildFrame
    frame = base_class(label, parent, wx.NewId(), label, common.property_panel,
                       klass=klass)
    node = Tree.Node(frame)
    frame.node = node
    common.app_tree.add(node)
    frame.show_widget(True)

    # add a default vertical sizer to the frame
    import edit_sizers
    edit_sizers._builder(frame, None, 0)
    # now select the frame's node in the tree
    common.app_tree.select_item(node)

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
        frame = base_class(label, parent, wx.NewId(), "",
                           common.property_panel, show=False, style=0)
        node = Tree.Node(frame)
        frame.node = node
        common.app_tree.add(node)
        return frame
    return xml_builder


def initialize():
    """\
    initialization function for the module: returns a wx.BitmapButton to be
    added to the main palette.
    """
    cwx = common.widgets_from_xml
    cwx['EditFrame'] = _make_builder(EditFrame)
    cwx['EditMDIChildFrame'] = _make_builder(EditMDIChildFrame)

    common.widgets['EditFrame'] = builder

    from tree import WidgetTree
    import os.path
    WidgetTree.images['EditMDIChildFrame'] = os.path.join(
        config.icons_path,
        'frame.xpm'
        )
    return common.make_object_button('EditFrame', 'frame.xpm', 1)
