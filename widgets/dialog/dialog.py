"""\
wxDialog objects (incl. wxMenuBar, wxToolBar and wxStatusBar)

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import math
import os
import wx

import common
import compat
import config
import misc
from tree import Tree
from widget_properties import *
from edit_windows import TopLevelBase, EditStylesMixin
from gui_mixins import BitmapMixin


class EditDialog(TopLevelBase, EditStylesMixin, BitmapMixin):

    def __init__(self, name, parent, id, title, property_window,
                 style=wx.DEFAULT_DIALOG_STYLE, show=True, klass='wxDialog'):

        # Initialise parent classes
        TopLevelBase.__init__(self, name, klass, parent, id,
                              property_window, show=show, title=title)
        self.base = 'wxDialog'
        EditStylesMixin.__init__(self)

        # initialise instance variables
        self.style = style
        self.icon = ""
        self.centered = False
        self.sizehints = False

        # initialise properties remaining staff
        properties = self.properties
        access = self.access_functions

        # style property
        access['style'] = (self.get_style, self.set_style)
        properties['style'] = CheckListProperty(
            self, 'style', self.widget_writer)

        # icon property
        access['icon'] = (self.get_icon, self.set_icon)
        properties['icon'] = FileDialogProperty(
            self, 'icon', style=wx.FD_OPEN | wx.FD_FILE_MUST_EXIST,
            label=_("Icon"))
        properties['icon'].set_tooltip(self.bitmap_tooltip_text)

        # centered property
        access['centered'] = (self.get_centered, self.set_centered)
        properties['centered'] = CheckBoxProperty(
            self, 'centered', label=_("Centred"))

        # size hints property
        access['sizehints'] = (self.get_sizehints, self.set_sizehints)
        properties['sizehints'] = CheckBoxProperty(
            self, 'sizehints', label=_('Set Size Hints'))

    def create_widget(self):
        if self.parent:
            parent = self.parent.widget
        else:
            parent = common.palette

        # we set always a default style because this is the best one for
        # editing the dialog (for example, a dialog without a caption would
        # be hard to move, etc.)
        default_style = wx.DEFAULT_DIALOG_STYLE | wx.RESIZE_BORDER

        # change 2002-10-09: now we create a wxFrame instead of a wxDialog,
        # because the latter gives troubles I wasn't able to solve when using
        # wxPython 2.3.3.1 :-/
        self.widget = wx.Frame(parent, self.id, "", style=default_style)
        self.widget.SetBackgroundColour(
            wx.SystemSettings_GetColour(wx.SYS_COLOUR_BTNFACE))
        self.set_icon(self.icon)

    def finish_widget_creation(self):
        TopLevelBase.finish_widget_creation(self)
        if not self.properties['size'].is_active():
            self.widget.SetSize((400, 300))

    def create_properties(self):
        TopLevelBase.create_properties(self)
        panel = wx.ScrolledWindow(self.notebook, -1, style=wx.TAB_TRAVERSAL)
        szr = wx.BoxSizer(wx.VERTICAL)
        self.properties['title'].display(panel)
        self.properties['icon'].display(panel)
        self.properties['centered'].display(panel)
        self.properties['style'].display(panel)
        szr.Add(self.properties['title'].panel, 0, wx.EXPAND)
        szr.Add(self.properties['icon'].panel, 0, wx.EXPAND)
        szr.Add(self.properties['centered'].panel, 0, wx.EXPAND)
        szr.Add(self.properties['style'].panel, 0, wx.EXPAND)
        panel.SetAutoLayout(True)
        compat.SizerItem_SetSizer(panel, szr)
        szr.Fit(panel)
        self.notebook.AddPage(panel, 'Widget')
        w, h = panel.GetClientSizeTuple()
        panel.SetScrollbars(5, 5, int(math.ceil(w/5.0)), int(math.ceil(h/5.0)))

    def get_icon(self):
        return self.icon

    def set_icon(self, value):
        self.icon = value.strip()
        if self.widget:
            if self.icon:
                bitmap = self.get_preview_obj_bitmap(self.icon)
            else:
                xpm = os.path.join(config.icons_path, 'dialog.xpm')
                bitmap = misc.get_xpm_bitmap(xpm)

            icon = wx.EmptyIcon()
            icon.CopyFromBitmap(bitmap)
            self.widget.SetIcon(icon)

    def get_centered(self):
        return self.centered

    def set_centered(self, value):
        try:
            self.centered = bool(int(value))
        except ValueError:
            pass

    def get_sizehints(self):
        return self.sizehints

    def set_sizehints(self, value):
        try:
            self.sizehints = bool(int(value))
        except ValueError:
            pass

# end of class EditDialog


def builder(parent, sizer, pos, number=[0]):
    """\
    factory function for EditDialog objects.
    """
    try:
        import panel
        has_panel = True
    except ImportError:
        has_panel = False

    class Dialog(wx.Dialog):
        def __init__(self):
            if has_panel: title = 'Select widget type'
            else: title = 'Select dialog class'
            wx.Dialog.__init__(self, None, -1, title)
            if common.app_tree.app.get_language().lower() == 'xrc':
                self.klass = 'wxDialog'
            else:
                if not number[0]:
                    self.klass = 'MyDialog'
                else:
                    self.klass = 'MyDialog%s' % number[0]
                number[0] += 1
            self.klass_prop = TextProperty(self, 'class', None)  # self)
            self.widget = 0
            szr = wx.BoxSizer(wx.VERTICAL)
            if has_panel:
                widget_prop = RadioProperty(self, 'widget', self,
                                            ['wxDialog', 'wxPanel'])
                szr.Add(widget_prop.panel, 0, wx.ALL|wx.EXPAND, 5)
            self.klass_prop.display(self)
            szr.Add(self.klass_prop.panel, 0, wx.ALL|wx.EXPAND, 5)
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
            if self.GetSize()[0] < 150:
                self.SetSize((150, -1))
            self.klass_modified = False
            self.CenterOnScreen()

        def set_klass(self, c):
            self.klass = c
            self.klass_modified = True

        def set_widget(self, c):
            self.widget = int(c)
            if not self.klass_modified:
                try:
                    number = str(int(self.klass[-1]))
                except ValueError:
                    number = ''
                if common.app_tree.app.get_language().lower() == 'xrc':
                    if self.widget == 0:
                        self.klass = 'wxDialog'
                    else:
                        self.klass = 'wxPanel'
                else:
                    if self.widget == 0:
                        self.klass = 'MyDialog' + number
                    else:
                        self.klass = 'MyPanel' + number
                self.klass_prop.set_value(self.klass)

        def __getitem__(self, value):
            if value == 'class':
                return lambda : self.klass, self.set_klass
            else:
                return lambda : self.widget, self.set_widget
    # end of inner class

    class_dialog = Dialog()
    res = class_dialog.ShowModal()
    klass = class_dialog.klass
    widget = class_dialog.widget
    class_dialog.Destroy()
    if res != wx.ID_OK:
        if number[0] > 0:
            number[0] -= 1
        return

    if widget == 0:
        name = 'dialog'
    else:
        name = 'panel'
    label = '%s_%d' % (name, (number[0] or 1))
    while common.app_tree.has_name(label):
        number[0] += 1
        label = '%s_%d' % (name, number[0])
    if widget == 0:
        is_panel = False
        dialog = EditDialog(label, parent, wx.NewId(), label,
                            common.property_panel, klass=klass)
    else:
        is_panel = True
        import panel
        dialog = panel.EditTopLevelPanel(label, parent, wx.NewId(),
                                         common.property_panel, klass=klass)
    node = Tree.Node(dialog)
    dialog.node = node
    dialog.show_widget(True)
    common.app_tree.add(node)
    if wx.Platform == '__WXMSW__':
        if not is_panel:
            w = dialog.widget
        else:
            w = dialog.widget.GetParent()
        w.CenterOnScreen()
        w.Raise()


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    """\
    factory to build EditDialog objects from a XML file
    """
    from xml_parse import XmlParsingError
    try:
        label = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    dialog = EditDialog(label, parent, wx.NewId(), "", common.property_panel,
                        show=False, style=0)
    node = Tree.Node(dialog)
    dialog.node = node
    common.app_tree.add(node)
    return dialog


def initialize():
    """\
    initialization function for the module: returns a wxBitmapButton to be
    added to the main palette.
    """
    cwx = common.widgets_from_xml
    cwx['EditDialog'] = xml_builder

    common.widgets['EditDialog'] = builder

    return common.make_object_button('EditDialog', 'dialog.xpm', 1,
                                     tip='Add a Dialog/Panel')
