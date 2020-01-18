"""\
wxDialog objects (incl. wxMenuBar, wxToolBar and wxStatusBar)

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016-2020 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import os
import wx

import common, compat, config, misc
import new_properties as np
from edit_windows import WindowBase, TopLevelBase, EditStylesMixin
from gui_mixins import BitmapMixin


class EditDialog(TopLevelBase, EditStylesMixin, BitmapMixin):
    WX_CLASS = "wxDialog"
    _PROPERTIES =["Widget", "title", "icon", "centered", "sizehints","menubar", "toolbar", "statusbar", "style"]
    PROPERTIES = TopLevelBase.PROPERTIES + _PROPERTIES + TopLevelBase.EXTRA_PROPERTIES
    _PROPERTY_LABELS = { "sizehints":'Set Size Hints'}
    _PROPERTY_HELP = {"size":WindowBase._PROPERTY_HELP["size_sizehints"]}
    
    def __init__(self, name, parent, title, style=wx.DEFAULT_DIALOG_STYLE, klass='wxDialog'):
        TopLevelBase.__init__(self, name, klass, parent, title=title)
        self.base = 'wxDialog'
        EditStylesMixin.__init__(self)
        self.properties["style"].set(style)

        # initialise instance properties
        self.icon      = np.BitmapPropertyD("")
        self.centered  = np.CheckBoxProperty(False, default_value=False)
        self.sizehints = np.CheckBoxProperty(False, default_value=False)

    def create_widget(self):
        if self.parent:
            parent = self.parent.widget
        else:
            parent = common.main

        # we set always a default style because this is the best one for editing the dialog
        # (for example, a dialog without a caption would be hard to move, etc.)
        default_style = wx.DEFAULT_DIALOG_STYLE | wx.RESIZE_BORDER
        if common.pin_design_window: default_style |= wx.STAY_ON_TOP

        # change 2002-10-09: now we create a wxFrame instead of a wxDialog,
        # because the latter gives troubles I wasn't able to solve when using wxPython 2.3.3.1 :-/
        self.widget = wx.Frame(parent, self.id, "", style=default_style)
        self.widget.SetBackgroundColour(compat.wx_SystemSettings_GetColour(wx.SYS_COLOUR_BTNFACE))
        self._set_widget_icon()

    def finish_widget_creation(self):
        TopLevelBase.finish_widget_creation(self)
        if not self.properties['size'].is_active():
            self.widget.SetSize((400, 300))

    def _set_widget_icon(self):
        if self.icon:
            bitmap = self.get_preview_obj_bitmap(self.icon.strip())
        else:
            xpm = os.path.join(config.icons_path, 'dialog.xpm')
            bitmap = misc.get_xpm_bitmap(xpm)

        icon = compat.wx_EmptyIcon()
        icon.CopyFromBitmap(bitmap)
        self.widget.SetIcon(icon)

    def properties_changed(self, modified):
        if not modified or "icon" in modified and self.widget: self._set_widget_icon()
        TopLevelBase.properties_changed(self, modified)
        EditStylesMixin.properties_changed(self, modified)



def builder(parent, pos):
    "factory function for EditDialog objects"
    import window_dialog
    base_classes = ['wxDialog', 'wxPanel']
    klass = 'wxDialog' if common.root.language.lower()=='xrc' else 'MyDialog'

    dialog = window_dialog.WindowDialog(klass, base_classes, 'Select widget type', True)
    res = dialog.show()
    dialog.Destroy()
    if res is None: return
    klass, base = res
    name = 'dialog'  if base == "wxDialog"  else  "panel"
    name = dialog.get_next_name(name)

    if base == "wxDialog":
        is_panel = False
        editor = EditDialog(name, parent, name, "wxDEFAULT_DIALOG_STYLE", klass=klass)
    else:
        is_panel = True
        import panel
        editor = panel.EditTopLevelPanel(name, parent, klass=klass)

    editor.create()
    if base == "wxDialog":
        editor.widget.Show()
    else:
        editor.widget.GetParent().Show()  # the panel is created as child of a Frame
    editor.design.update_label()
    if wx.Platform == '__WXMSW__':
        if not is_panel:
            w = editor.widget
        else:
            w = editor.widget.GetParent()
        w.CenterOnScreen()
        w.Raise()

    import clipboard
    editor.drop_target = clipboard.DropTarget(editor)
    editor.widget.SetDropTarget(editor.drop_target)

    # add a default vertical sizer
    import edit_sizers
    edit_sizers._builder(editor, 0)

    return editor


def xml_builder(attrs, parent, pos=None):
    "factory to build EditDialog objects from a XML file"
    from xml_parse import XmlParsingError
    try:
        label = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    return EditDialog(label, parent, "", style=0)


def initialize():
    "initialization function for the module: returns a wxBitmapButton to be added to the main palette"
    common.widget_classes['EditDialog'] = EditDialog
    common.widgets['EditDialog'] = builder
    common.widgets_from_xml['EditDialog'] = xml_builder

    return common.make_object_button('EditDialog', 'dialog.xpm', 1, tip='Add a Dialog/Panel')
