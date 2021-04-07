"""\
wxDialog objects (incl. wxMenuBar, wxToolBar and wxStatusBar)

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016-2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import os
import wx

import common, compat, config, misc
import new_properties as np
from edit_windows import WindowBase, TopLevelBase, EditStylesMixin
from gui_mixins import BitmapMixin


class AffirmativePropertyD(np.ListBoxPropertyD):

    def track_name(self, old_name=None, new_name=None):
        # if the current value is removed (new_name=None), it will be remembered and re-used if added again
        # this is to keep it over a cut/paste or drag/drop
        if self.value is not None and old_name==self.value:
            # current button is being edited
            if new_name is None:
                # current button has been deleted
                self.previous_value = old_name
                self.value = None
            else:
                self.value = new_name
        elif self.value is None and old_name is None and new_name==self.previous_value:
            buttons = self.owner.find_children(new_name, "wxButton")
            if buttons:
                # the previous button has been pasted again
                self.value = new_name
            self.previous_value = None

    def create_editor(self, panel, sizer):
        buttons = self.owner.find_children(None, "wxButton")
        self.choices = [button.name for button in buttons]
        if self.deactivated:
            # deactivated, but stockitems ID_OK or ID_CANCEL are used inside, display these as defaults
            buttons = [b for b in buttons if b.check_prop_truth("stockitem")]
            buttons = [b.name for b in buttons if b.stockitem==self.default_value]
            self.value = buttons[0]  if buttons else  ""
        super(AffirmativePropertyD, self).create_editor(panel, sizer)


class EditDialog(BitmapMixin, TopLevelBase, EditStylesMixin):
    WX_CLASS = "wxDialog"
    CHILDREN = -1
    _PROPERTIES =["Widget", "title", "icon", "centered", "affirmative", "escape",
                  "sizehints","menubar", "toolbar", "statusbar", "style"]
    PROPERTIES = TopLevelBase.PROPERTIES + _PROPERTIES + TopLevelBase.EXTRA_PROPERTIES
    _PROPERTY_LABELS = { "sizehints":'Set Size Hints'}
    _PROPERTY_HELP = {"size":WindowBase._PROPERTY_HELP["size_sizehints"],
                      "affirmative":"Inside dialogs by default the OK button is the one which is pressed\n"
                                    "when the user hits Enter.\n"
                                    "(The OK button is the button with 'Stockitem' property set to 'OK'.)\n"
                                    "Select a different button to be used instead.\n"
                                    "Usually, you need to activate 'Default' for the button as well.\n"
                                    "See SetAffirmativeId and SetEscapeId in the wx documentation.",
                      "escape":"Inside dialogs by default the CANCEL button is the one which is pressed\n"
                               "when the user hits Escape.\n"
                               "(The CANCEL button is the button with 'Stockitem' property set to 'CANCEL'.)\n"
                               "Select a different button to be used instead.\n"
                               "See SetAffirmativeId and SetEscapeId in the wx documentation."}

    def __init__(self, name, parent, index, klass, title, style=wx.DEFAULT_DIALOG_STYLE):
        TopLevelBase.__init__(self, name, parent, index, klass, title)
        EditStylesMixin.__init__(self)
        self.properties["style"].set(style)

        # initialise instance properties
        self.icon        = np.BitmapPropertyD("")
        self.affirmative = AffirmativePropertyD("", default_value="OK")
        self.escape      = AffirmativePropertyD("", default_value="CANCEL")
        self.centered    = np.CheckBoxProperty(False, default_value=False)
        self.sizehints   = np.CheckBoxProperty(False, default_value=False)

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
        self.widget = wx.Frame(parent, wx.ID_ANY, "", style=default_style)
        self.widget.SetBackgroundColour(compat.wx_SystemSettings_GetColour(wx.SYS_COLOUR_BTNFACE))
        self._set_widget_icon()

    def child_widgets_created(self, level):
        if not self.properties['size'].is_active():
            self.widget.SetSize((400, 300))

    def _set_widget_icon(self):
        if self.icon:
            bitmap = self.get_preview_obj_bitmap(self.icon.strip())
        else:
            xpm = os.path.join(config.icons_path, 'dialog.png')
            bitmap = misc.get_xpm_bitmap(xpm)

        icon = compat.wx_EmptyIcon()
        icon.CopyFromBitmap(bitmap)
        self.widget.SetIcon(icon)

    def _properties_changed(self, modified, actions):
        if not modified or "icon" in modified and self.widget: self._set_widget_icon()
        TopLevelBase._properties_changed(self, modified, actions)
        EditStylesMixin._properties_changed(self, modified, actions)

    def track_contained_name(self, old_name=None, new_name=None):
        TopLevelBase.track_contained_name(self, old_name, new_name)
        self.properties["affirmative"].track_name( old_name, new_name )
        self.properties["escape"].track_name( old_name, new_name )


def builder(parent, index, klass=None, base=None, name=None):
    "factory function for EditDialog objects"
    import window_dialog, edit_base, panel
    if klass is None or base is None:
        klass = 'wxDialog' if common.root.language.lower()=='xrc' else 'MyDialog'

        dialog = window_dialog.DialogOrPanelDialog(klass)
        res = dialog.show()
        add_sizer = dialog.get_options()[0]
        button_names, button_types = dialog.get_selected_buttons()
        dialog.Destroy()
        if res is None: return
        klass, base = res
        name = 'dialog'  if base == "wxDialog"  else  "panel"
        name = dialog.get_next_name(name)
        interactive = True
    else:
        interactive = False

    if base == "wxDialog":
        editor = EditDialog(name, parent, None, klass, name, "wxDEFAULT_DIALOG_STYLE")
    else:
        editor = panel.EditTopLevelPanel(name, parent, None, klass)

    if interactive and add_sizer:
        # add a default panel and vertical sizer to the frame; optionally add buttons if it's a Dialog
        import edit_sizers, widgets.button.button
        slots = 2  if base=="wxDialog" and button_names else  1
        szr = edit_sizers._builder(editor, 0, slots=slots)
        if base=="wxDialog":
            button_szr = edit_sizers._builder(szr, 1, "StdDialogButtonSizer", slots=len(button_names))
            button_szr.properties["border"].set(4)
            button_szr.properties["flag"].set("wxALL|wxALIGN_RIGHT")
            i = 0
            for button_name, button_type in zip(button_names, button_types):
                name = "button_%s"%button_name
                label = "OK" if button_name=="OK" else button_name.capitalize()
                button = widgets.button.button.EditButton(name, button_szr, i, label)
                button.properties["stockitem"].set(button_name, activate=True)
                if button_type=="A":
                    editor.properties["affirmative"].set(name, activate=True)
                    button.properties["default"].set(True)
                if button_type=="C":
                    editor.properties["escape"].set(name, activate=True)
                i += 1
    else:
        # just add a slot
        edit_base.Slot(editor, 0)

    editor.create()
    if base == "wxDialog":
        editor.widget.Show()
    else:
        editor.widget.GetParent().Show()  # the panel is created as child of a Frame
    editor.design.update_label()
    if wx.Platform == '__WXMSW__':
        w = editor.widget.GetTopLevelParent()
        w.CenterOnScreen()
        w.Raise()

    return editor


def xml_builder(parser, base, name, parent, index):
    "factory to build EditDialog objects from a XML file"
    return EditDialog(name, parent, index, "Dialog", "", 0)


def initialize():
    "initialization function for the module: returns a wxBitmapButton to be added to the main palette"
    common.widget_classes['EditDialog'] = EditDialog
    common.widgets['EditDialog'] = builder
    common.widgets_from_xml['EditDialog'] = xml_builder

    return common.make_object_button('EditDialog', 'dialog.png', 1, tip='Add a Dialog/Panel')
