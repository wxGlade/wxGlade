"""\
wxFrame objects (incl. wxMenuBar, wxToolBar and wxStatusBar)

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016-2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
import os
import common, config, misc, compat
import new_properties as np
from edit_windows import WindowBase, TopLevelBase, EditStylesMixin, Slot
from gui_mixins import BitmapMixin



class BarProperty(np.CheckBoxProperty):
    # when checkbox is checked, a MenuBar/ToolBar/StatusBar will be created or destroyed
    def __init__(self, widget_name="MenuBar"):
        self.widget_name = widget_name
        self.widget_index = None
        np.CheckBoxProperty.__init__(self, False, default_value=False)

    def set_owner(self, owner, attributename):
        # set e.g. owner._menubar to None; "_menubar" is the attribute name / index for storing the bar
        np.CheckBoxProperty.set_owner(self, owner, attributename)
        self.widget_index = "_" + attributename
        setattr(owner, self.widget_index, None)

    def on_value_edited(self, new_value, active=None):
        # user has clicked the checkbox
        # the main modification to np.Property.on_value_edited is that it does not store the modification in the history
        if active is not None: self.deactivated = not active

        # create or remove the bar:
        if new_value:
            bar = common.widgets["Edit"+self.widget_name](self.owner, self.widget_index, True)
        else:
            bar = getattr(self.owner, self.widget_index).remove()
        setattr(self.owner, self.widget_index, bar)
        misc.rebuild_tree(widget=self.owner, recursive=False, focus=True)

        self.set(new_value)
        self._notify()


class EditFrame(BitmapMixin, TopLevelBase, EditStylesMixin):
    WX_CLASS = "wxFrame"
    _PROPERTIES =["Widget", "title", "icon", "centered", "sizehints","menubar", "toolbar", "statusbar", "style", "min_size"]
    PROPERTIES = TopLevelBase.PROPERTIES + _PROPERTIES + TopLevelBase.EXTRA_PROPERTIES
    #np.insert_after(PROPERTIES, "class", "custom_base")
    _PROPERTY_HELP   = { 'icon':'Icon for this window.',
                         "size":"Specify the size of the frame.\n\n"
                                "If you don't specify, the Fit() method of the contained sizer will be called\n"
                                "such that the frame will fit the minimum required size of the contained widgets.\n\n"
                                "For a frame that's unusual. Usually you set the size and make the contents expand "
                                "to fill the available space." }
    _PROPERTY_LABELS = { "sizehints":'Set Size Hints', "menubar":'Has MenuBar', "toolbar":'Has ToolBar',
                         "statusbar":'Has StatusBar' }
    ATT_CHILDREN = ["_menubar", "_statusbar", "_toolbar"]

    def __init__(self, name, parent, index, klass, title, style=wx.DEFAULT_FRAME_STYLE):
        TopLevelBase.__init__(self, name, parent, index, klass, title)
        EditStylesMixin.__init__(self)
        self.properties["style"].set(style)

        # initialise instance properties
        self.icon      = np.BitmapPropertyD("")
        self.centered  = np.CheckBoxProperty(False, default_value=False)
        self.sizehints = np.CheckBoxProperty(False, default_value=False)

        self.menubar   = BarProperty("MenuBar")
        self.toolbar   = BarProperty("ToolBar")
        if "statusbar" in self.PROPERTIES:  # not for MDIChildFrame
            self.statusbar = BarProperty("StatusBar")

        self.min_size  = np.SizePropertyD( "-1, -1", default_value="-1, -1" )

    def create_widget(self):
        parent = None
        style = self.style
        if common.pin_design_window: style |= wx.STAY_ON_TOP
        self.widget = wx.Frame(parent, wx.ID_ANY, self.title, style=style)
        self._set_widget_icon()

    def finish_widget_creation(self, level):
        # add menu, status and tool bar
        TopLevelBase.finish_widget_creation(self, level)
        if not self.properties['size'].is_active():
            self.widget.SetSize((400, 300))
        if wx.Platform == '__WXMSW__':
            self.widget.CenterOnScreen()

    def _set_widget_icon(self):
        if self.icon:
            bitmap = self.get_preview_obj_bitmap(prop=self.properties["icon"])
        else:
            xpm = os.path.join(config.icons_path, 'frame.png')
            bitmap = misc.get_xpm_bitmap(xpm)

        icon = compat.wx_EmptyIcon()
        icon.CopyFromBitmap(bitmap)
        self.widget.SetIcon(icon)

    def _properties_changed(self, modified, actions):
        if not modified or "icon" in modified and self.widget: self._set_widget_icon()

        TopLevelBase._properties_changed(self, modified, actions)
        EditStylesMixin._properties_changed(self, modified, actions)



class EditMDIChildFrame(EditFrame):
    WX_CLASS = "wxMDIChildFrame"
    IS_TOPLEVEL_WINDOW = False  # avoid to appear in the "Top Window" property of the app
    PROPERTIES = [p for p in EditFrame.PROPERTIES if p!="statusbar"]
    ATT_CHILDREN = ["_menubar", "_toolbar"]
    TREE_ICON = "EditFrame"


# options for WindowDialog when interactively adding a Frame
options = ["Add panel and sizer"]
last_choices = [True]
_option_help = """\
On some platforms, I think mainly Windows, the panel is responsible for the navigation through the controls.
Without the panel you can't use Tab and Shift+Tab to navigate through the controls.
Also on Windows the background colour of the plain frame will look darker than usual."""

if config.debugging:
    # for testing the error handling
    options.append("Provoke an error")
    last_choices.append(False)


def builder(parent, index, klass=None, base=None, name=None):
    "factory function for EditFrame objects"
    global last_choices
    if klass is None or base is None:
        import window_dialog
        base_classes = ['wxFrame', 'wxMDIChildFrame']
        klass = 'wxFrame' if common.root.language.lower()=='xrc' else 'MyFrame'
        
        dialog = window_dialog.WindowDialog(klass, base_classes, 'Select frame class', True, options, last_choices)
        compat.SetToolTip(dialog.option_controls[0], _option_help)
        res = dialog.show()
        dialog.Destroy()
        if res is None: return None
        klass, base = res
        last_choices[:] = dialog.get_options()  # remember for next time
        if config.debugging and last_choices[1]: XXX  # provoke an error
        name = dialog.get_next_name("frame")
        interactive = True
    else:
        interactive = False  # last_choices not to be obeyed

    if base == "wxFrame":
        base_class = EditFrame
    else:
        base_class = EditMDIChildFrame
    editor = base_class(name, parent, index, klass, name, "wxDEFAULT_FRAME_STYLE")
    editor.properties['size'].set( (400,300), activate=True )
    editor.design.update_label()

    if interactive and last_choices[0]:
        # add a default panel and vertical sizer to the frame
        import edit_sizers, widgets.panel.panel
        panel_editor = widgets.panel.panel.builder(editor, 0)
        edit_sizers._builder(panel_editor, 0)
    else:
        # just add a slot
        Slot(editor, 0)

    editor.create()

    return editor


_base_classes = {'EditFrame':EditFrame, 'EditMDIChildFrame':EditMDIChildFrame}
def xml_builder(parser, base, name, parent, index):
    if parser.input_file_version and parser.check_input_file_version((0,8)):
        # backwards compatibility
        style = "wxDEFAULT_FRAME_STYLE"
    else:
        style = 0
    return _base_classes[base](name, parent, index, "Frame", "", style)


def initialize():
    "initialization function for the module: returns a wx.BitmapButton to be added to the main palette"
    common.widget_classes['EditFrame'] = EditFrame
    common.widgets['EditFrame'] = builder
    common.widgets_from_xml['EditFrame'] = xml_builder

    common.widget_classes['EditMDIChildFrame'] = EditMDIChildFrame
    common.widgets_from_xml['EditMDIChildFrame'] = xml_builder

    return common.make_object_button('EditFrame', 'frame.png', 1)
