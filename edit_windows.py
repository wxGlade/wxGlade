"""
Base classes for windows used by wxGlade

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016-2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import logging
import wx

import new_properties as np
import edit_base
import misc, common, compat, config, clipboard
import decorators, contextlib
from wcodegen.taghandler import BaseXmlBuilderTagHandler

from events_mixin import EventsMixin


class FontHandler(BaseXmlBuilderTagHandler):
    item_attrs = {'size': 0, 'family': 1, 'style': 2, 'weight': 3, 'underlined': 4, 'face': 5}
    strip_char_data = True

    def __init__(self, owner):
        super(FontHandler, self).__init__()
        self.owner = owner
        self.props = [8, 'default', 'normal', 'normal', 0, ''] # size, family, style, weight, underlined, face
        self.index = 0

    def start_elem(self, name, attrs):
        self.index = self.item_attrs.get(name, 5)

    def end_elem(self, name):
        if name in ('font', 'label_font', 'cell_font'):
            self.owner.properties[name].set(tuple(self.props), activate=True)
            return True  # to remove this handler

    def char_data(self, data):
        super(FontHandler, self).char_data(data)
        char_data = self.get_char_data()
        if self.index in (0,4):
            self.props[self.index] = int(char_data)
        else:
            self.props[self.index] = char_data


class ExtraPropertiesPropertyHandler(BaseXmlBuilderTagHandler):
    strip_char_data = True

    def __init__(self, owner):
        super(ExtraPropertiesPropertyHandler, self).__init__()
        self.owner = owner
        self.props = []
        self.prop_name = None

    def start_elem(self, name, attrs):
        super(ExtraPropertiesPropertyHandler, self).__init__()
        if name == 'property':
            self.prop_name = attrs['name']

    def end_elem(self, name):
        if name == 'property':
            if self.prop_name and self._content:
                self.props.append( [self.prop_name, ''.join(self._content)] )
            self.prop_name = None
            self._content = []
        elif name == 'extraproperties':
            self.owner.properties['extraproperties'].load(self.props)
            return True  # to remove this handler


class EditBase(EventsMixin, edit_base.EditBase):
    """Base class of every window available in the builder.

    This class holds the basic properties for this object.
    The properties that control the layout (i.e. the behaviour when inside a sizer) are in ManagedBase."""
    can_preview = False
    # "class" and "custom_base" will be added for TopLevelBase, notebook, panel and splitter window
    _PROPERTIES = ["Common", "name", "instance_class"]
    PROPERTIES = _PROPERTIES

    # the following will be placed on the last tab
    _EXTRA_PROPERTIES = ["Events", "events", "Code", "extracode", "extracode_pre", "extracode_post", "extraproperties"]
    EXTRA_PROPERTIES = _EXTRA_PROPERTIES

    _PROPERTY_HELP={ "class": ("The name of the class to be generated.\n\n"
                               "E.g. for 'ClassName':\n\n"
                               "class ClassName:\n"
                               "    def __init__(self, ...):\n        ...\n\n"
                               "Toplevel windows like frames or dialogs are aways classes.\n"
                               "Notebooks, panels or splitters can be classes."),
                     "name":_("Name of the variable for assigning the reference to the created widget instance.\n\n"
                              "(Valid: alphanumeric characters and underscore, also minus, but not at the beginning)\n"
                              "If the config setting is to have unique names, a non-unique name will be indicated"
                              " by a yellow background.\n\n"
                              "You may edit this in the Tree view as well."),
                     "label": _("You may edit this in the Tree view as well."),
                     "custom_base": _("A comma-separated list of custom base classes. The first will be invoked\n"
                                      "with the same parameters as this class, while for the others the default\n"
                                      "constructor will be used. You should probably not use this if \n"
                                      "overwrite existing sources is not set."),
                     "instance_class":("Instead of e.g. wx.TextCtrl you may specify a compatible class here,\n"
                                "e.g. 'mycontrols.MyTextCtrl'.\n\n"
                                "You need to ensure that the class is available.\n"
                                "Add required import code to 'Extra (import) code for this widget' on the Code tab."),
                     "extracode":"This code will be inserted at the beginning of the file.\n"
                                 "Use this to add e.g. import statements.\n\n"
                                 "The code will be added to the section marked with '# begin wxGlade: extracode'",
                     "extracode_pre":"This code will be inserted right before the widget is created.\n"
                                     "Use this e.g. to create argument values for the widget.",
                     "extracode_post":"This code will be inserted right after the widget is created.\n"
                                      "Use this to set properties that are not added by wxGlade itself."}
    _PROPERTY_LABELS = {"custom_base":'Base class(es)',
                        "extracode":"Extra (import) code for this widget",
                        "extracode_pre":"Code to be inserted before",
                        "extracode_post":"Code to be inserted after"}
    def __init__(self, name, parent, index, klass=None, instance_class=None):
        edit_base.EditBase.__init__(self, name, parent, index)

        # initialise instance properties
        if "class" in self.PROPERTIES:
            if self.IS_TOPLEVEL:
                # always a class
                self.klass = klass_p = np.ClassProperty(klass, name="class")
            else:
                # optionally a class
                self.klass = klass_p = np.ClassPropertyD(klass, name="class")
                if klass: klass_p.deactivated = False

        if "instance_class" in self.PROPERTIES:
            self.instance_class = instance_class_p = np.InstanceClassPropertyD(instance_class, default_value=self.WX_CLASS)
            if instance_class is not None and instance_class!=self.WX_CLASS:
                instance_class_p.deactivated = False

        if "custom_base" in self.PROPERTIES:
            # for TopLevelBase, notebook, panel and splitter window
            self.custom_base = custom_base_p = np.BaseClassesPropertyD(default_value=self.WX_CLASS)
            if klass_p.deactivated: custom_base_p.set_blocked()

        self.extracode       = np.CodeProperty()
        self.extracode_pre   = np.CodeProperty()
        self.extracode_post  = np.CodeProperty()
        self.extraproperties = np.ExtraPropertiesProperty()

        EventsMixin.__init__(self)

    def get_instantiation_class(self, formatter=None, cls_formatter=None, preview=False):
        # e.g. klass = obj.get_instantiation_class(self.cn, self.cn_class)
        if preview:
            if self.IS_TOPLEVEL:
                if cls_formatter is not None: return cls_formatter(self.klass)
                return self.klass
            if formatter is not None:
                return formatter(self.WX_CLASS)
            return self.WX_CLASS

        if self.check_prop("instance_class"):
            return self.instance_class
        if self.check_prop_truth("class"):
            if cls_formatter is not None: return cls_formatter(self.klass)
            return self.klass
        if formatter is not None:
            return formatter(self.WX_CLASS)
        return self.WX_CLASS

    def get_property_handler(self, prop_name):
        """Returns a custom handler function for the property 'prop_name', used when loading this object from a XML file.
        handler must provide three methods: 'start_elem', 'end_elem' and 'char_data'"""
        return EventsMixin.get_property_handler(self, prop_name)

    def _properties_changed(self, modified, actions):
        edit_base.EditBase._properties_changed(self, modified, actions)
        # enable "custom_base" property only if "class" is active
        class_p = self.properties.get("class")
        if class_p and (not modified or "class" in modified) and class_p.deactivated is not None:
            self.properties["custom_base"].set_blocked( block=class_p.deactivated or not class_p.value )

    # context menu #####################################################################################################
    def popup_menu(self, event, pos=None):
        event_widget = event.GetEventObject()
        menu = self._create_popup_menu(widget=event_widget)
        if pos is None:
            # convert relative event position to relative widget position
            event_pos  = event.GetPosition()                     # event position
            screen_pos = event_widget.ClientToScreen(event_pos)  # screen position
            pos        = event_widget.ScreenToClient(screen_pos) # client position
        event_widget.PopupMenu(menu, pos=pos)
        menu.Destroy()

    def _create_popup_menu(self, widget):
        menu = misc.wxGladePopupMenu(self.name)

        # edit: remove/copy/cut
        widgetclass = self.__class__.__name__.lstrip("Edit")
        i = misc.append_menu_item(menu, -1, _('Remove %s\tDel')%widgetclass, wx.ART_DELETE)
        misc.bind_menu_item_after(widget, i, self.remove)

        if hasattr(self, "_add_popup_menu_items"):
            self._add_popup_menu_items(menu, widget)

        i = misc.append_menu_item( menu, -1, _('Copy\tCtrl+C'), wx.ART_COPY )
        misc.bind_menu_item_after(widget, i, clipboard.copy, self)
        i = misc.append_menu_item( menu, -1, _('Cut\tCtrl+X'), wx.ART_CUT )
        misc.bind_menu_item_after(widget, i, clipboard.cut, self)
        menu.AppendSeparator()

        if hasattr(self.parent, "_add_parent_popup_menu_items"):
            self.parent._add_parent_popup_menu_items(menu, self, widget)

        # preview (create or close?)
        p = misc.get_toplevel_widget(self)
        if p is not None and p.preview_is_visible():
            item = _('Close preview (%s)\tF5') % p.name
        else:
            item = _('Preview (%s)\tF5') % p.name
        i = misc.append_menu_item( menu, -1, item )
        #misc.bind_menu_item_after(widget, i, self.preview_parent)
        misc.bind_menu_item(widget, i, self.preview_parent)

        return menu

    def preview_parent(self, *args):
        widget = self.toplevel_parent # misc.get_toplevel_widget(self)
        if widget is not None:
            wx.CallAfter( widget.preview )  # direct call would result in crash

    ####################################################################################################################

    def on_set_focus(self, event):
        """Event handler called when a window receives the focus: this in fact is
        connected to a EVT_LEFT_DOWN and not to an EVT_FOCUS, but the effect is the same"""
        misc.set_focused_widget(self)
        #if wxPlatform != '__WXMSW__': event.Skip()

    # clipboard ########################################################################################################
    def check_compatibility(self, widget, typename=None):
        # only with slots before/after
        if typename=="bitmap":
            return (False, "No bitmaps can be pasted here")
        if (typename is not None and typename=="window") or (widget and widget.IS_TOPLEVEL):
            return (False,"No toplevel objects can be pasted here")
        if self.parent.IS_SIZER and not self.parent._can_add_insert_slots():
            return (False, "Slot is populated already")
        return ("Slot",None)

    def check_drop_compatibility(self):
        # checks whether a widget can be dropped here
        return (False, "Items can only be added to empty slots. Add or free a slot first.")

    ####################################################################################################################

    def is_visible(self):
        if not self.widget: return False
        if not self.widget.IsShown() and not isinstance(self.widget, wx.ScrolledWindow): return False
        if self.IS_TOPLEVEL:
            return self.widget.GetTopLevelParent().IsShown()
        parent = self.parent
        if parent: return parent.is_visible()
        return self.widget.GetParent().IsShown()

    def update_view(self, selected):
        """Updates the widget's view to reflect its state, i.e. shows which
        widget is currently selected; the default implementation does nothing."""
        pass

    def _get_default_or_client_size(self):
        # used by SplitterWindow, Panel to create with parent's client size if parent is not a sizer
        if self.parent.IS_SIZER: return wx.DefaultSize
        return self.parent.widget.GetClientSize()

    @contextlib.contextmanager
    def frozen(self):
        if config.use_freeze_thaw and self.widget:
            toplevel = self.widget.GetTopLevelParent()
            if config.debugging: print("Freezing", toplevel)
            toplevel.Freeze()
        else:
            toplevel = None
        try:
            yield
        finally:
            if toplevel:
                if self.widget:
                    self.widget.Refresh()
                    #self.widget.SendSizeEvent()  # would work most of the time
                    toplevel.SendSizeEvent()  # would work as well
                if config.debugging: print("Thawing", toplevel)
                toplevel.Thaw()


class WindowBase(EditBase):
    """Extends EditBase with the addition of the common properties available to
    almost every window: size, background and foreground colours, and font"""
    _PROPERTIES = ["id", "size",
                   "background", "foreground", "font", "tooltip", "disabled", "focused", "hidden"]
    PROPERTIES = EditBase.PROPERTIES + _PROPERTIES

    _PROPERTY_HELP = {
        "id":   "The 'Id' property could be\n"
                "    1) a constant numeric value\n"
                "    2) a predefined identifier e.g. wxID_ANY\n"
                "    3) a predefined variable like a class member e.g. self.myButtonID\n"
                "    4) a variable assignment e.g. self.myButtonID=?\n\n"
                "The pattern of a variable assignment is always 'variable=value'.\n\n"
                "The value could be again a numeric value, a predefined identifier,\n"
                "another predefined variable or '?' a shortcut for 'wxNewId()'.",

        "size": "Specify the size of the widget.\n\n"
                "E.g. '200,100' for a width of 200 pixels and a height of 100.\n\n"
                "If you want to specify either width or height, you can use -1\n"
                "for the other direction to keep the default for that.\n"
                "E.g. '200,-1' or '-1,200'.\n"
                "(The default value is '-1,-1' .)",

        "size_sizehints": "Specify the size of the widget.\n"
                "This might be overridden if Sizehints property is enabled.\n\n"
                "E.g. '200,100' for a width of 200 pixels and a height of 100.\n\n"
                "If you want to specify either width or height, you can use -1\n"
                "for the other direction to keep the default for that.\n"
                "E.g. '200,-1' or '-1,200'.\n"
                "(The default value is '-1,-1' .)",

        "sizehints":"Call SetSizeHints method of the toplevel sizer.\n"
                    "This resizes the window such that it fits the sizer's minimal size.\n"
                    "Setting this usually overrides the size property."
    }

    _PROPERTY_LABELS = {"attribute":'Store as attribute'}  # used in many derived widget editors

    IS_WINDOW = True
    CHILDREN = None  # sizer or something else

    def __init__(self, name, parent, index, klass, instance_class=None):
        EditBase.__init__(self, name, parent, index, klass, instance_class)

        self.window_id = np.TextPropertyD( "wxID_ANY", name="id", default_value=None )
        self.size      = np.SizePropertyD( "-1, -1", default_value="-1, -1" )

        self.sel_marker = None  # selection markers (a SelectionMarker instance)

        # background, foreground, font properties
        # their actual values will be stored/modified after widget creation in 'finish_widget_creation'
        # before that, the actual values will be stored in this dict from the actual values of the widget:
        self._original = {'font': None}
        # colors
        self.background = np.ColorPropertyD(None)
        self.foreground = np.ColorPropertyD(None)
        # font
        if "font" in self.PROPERTIES:
            if config.use_gui:
                font = self._build_from_font( compat.wx_SystemSettings_GetFont(wx.SYS_DEFAULT_GUI_FONT) )
                font[1] = 'default'
            else:
                font = (9, 'default', 'normal', 'normal', 0, 'Segoe UI')
            self.font = np.FontPropertyD(tuple(font))

        # tooltip, focused, hiden
        self.tooltip    = np.TextPropertyD(multiline="grow")
        self.disabled   = np.CheckBoxProperty(False, default_value=False)
        self.focused    = np.CheckBoxProperty(False, default_value=False)
        self.hidden     = np.CheckBoxProperty(False, default_value=False)

        self.toplevel_parent.parent.check_codegen(self)

    def finish_widget_creation(self, level):
        self.widget.Bind(wx.EVT_SIZE, self.on_size)

        # store the actual values of font as default, if the property is deactivated later
        fnt = self.widget.GetFont()
        if not fnt.IsOk(): fnt = wx.SystemSettings_GetFont(wx.SYS_DEFAULT_GUI_FONT)
        self._original['font'] = fnt.GetNativeFontInfoDesc()

        if self.check_prop_truth("font"): self._set_font()
        if self.check_prop_truth("wrap"): self.widget.Wrap(self.wrap)
        if self.check_prop("size"):       self.set_size("size", "SetSize")
        if self.check_prop("min_size"):   self.set_size("min_size", "SetMinSize")
        if self.check_prop("max_size"):   self.set_size("max_size", "SetMaxSize")
        if self.check_prop("background"): self.widget.SetBackgroundColour(self.properties["background"].get_color())
        if self.check_prop("foreground"): self.widget.SetForegroundColour(self.properties["foreground"].get_color())

        EditBase.finish_widget_creation(self, level)

        self.widget.Bind(wx.EVT_CHAR_HOOK, self.on_char_hook)

    def on_char_hook(self, event):
        misc.handle_key_event(event, "design")

    def _reparent_widget(self, widget):
        "call Reparent(self.widget) for all direct children, including those in sizers"
        if isinstance(widget, wx.Window):
            widget.Reparent(self.widget)
        elif isinstance(widget, wx.Sizer):
            if hasattr(widget, "reparenting"):
                # for (wxGlade)StaticBoxSizer and CustomGridSizer
                widget.reparenting(self.widget)
            # go through all children
            for si in widget.GetChildren():
                if si is None: continue
                child = si.GetWindow() or si.GetSizer()
                if child: self._reparent_widget(child)

    def recreate_widget(self):
        "re-create widget and re-parent children"
        # currently used by panel on modification of 'scrolled' to switch between ScrolledWindow and Panel
        old_widget = self.widget
        with self.frozen():
            self.parent.destroying_child_widget(self, self.index)
            self.create_widget()  # this is not recursive
            if self.IS_TOPLEVEL_WINDOW: self.widget.SetSize(size)   # do this for IS_TOPLEVEL only?
            old_widget.Hide()
            if self.sel_marker:
                self.sel_marker.Destroy()
                self.sel_marker = None

            sizer = old_widget.GetSizer()
            if sizer:
                old_widget.SetSizer(None, False)
                self._reparent_widget(sizer)
                self.widget.SetSizer(sizer)
                sizer.SetContainingWindow(self.widget)
            else:
                for child in old_widget.GetChildren():
                    # no sizer, but a single child or just a slot
                    self._reparent_widget(child)
            compat.DestroyLater(old_widget)
            self.finish_widget_creation(0)
            self.parent.child_widget_created(self, 0)

    def recreate_widget2(self):
        "destroy widget (incl. children) and recursively re-create, typically when a style can't be changed dynamically"
        # restore position and size for toplevels; focus else
        if self.IS_TOPLEVEL:
            rect = self.widget.GetTopLevelParent().GetRect()
        else:
            focused = misc.focused_widget is self

        with self.frozen():
            self.parent.destroying_child_widget(self, self.index)
            self.destroy_widget(0, later=True)
            self.parent.destroyed_child_widget()

            self.create()

            # restore position and size for toplevels; focus else
            if self.IS_TOPLEVEL:
                self.widget.GetTopLevelParent().SetRect(rect)
            elif focused:
                misc.focused_widget = self
                if self.sel_marker: self.sel_marker.Show(True)

    def on_size(self, event):
        "Update the value of the 'size' property"
        if not self.widget: return  # this can happen on destruction
        event.Skip()
        if event.GetEventObject() is None: return
        try:
            prop_size = self.properties['size']

            # try to preserve the user's choice
            size_prop = prop_size.value.strip()

            if prop_size.is_active():
                use_dialog_units = size_prop and size_prop[-1] == 'd'
                if use_dialog_units: size_prop = size_prop[:-1]  # drop the left 'd'

                weidth_prop, height_prop = [int(t) for t in size_prop.split(',')]
            else:
                use_dialog_units = config.preferences.use_dialog_units
                weidth_prop, height_prop = 0, 0

            if use_dialog_units:
                weidth_widget, height_widget = compat.ConvertPixelsToDialog(self.widget, self.widget.GetSize() )
            else:
                weidth_widget, height_widget = self.widget.GetSize()

            if weidth_prop == -1: weidth_widget = -1
            if height_prop == -1: height_widget = -1

            size_widget = "%s, %s" % (weidth_widget, height_widget)
            if use_dialog_units: size_widget += "d"

            # There are an infinite loop of wxSizeEvents. All events have  the same id.
            # It looks currently like a bug in the underlaying wx libraries especially in the GTK part.
            # The bug doesn't occur on Windows.
            # The issue probably occur only within EditGrid.
            # This is workaround prevents the propagation if the size hasn't changed.
            # Related SF bug report: #170
            if size_prop != size_widget:
                # set to the actual value, either because wx forced it or just for displaying
                prop_size.set(size_widget)
        except KeyError:
            logging.exception(_('Internal Error'))

        # for a Panel this is required, while it's not required for a Frame:
        if self.CHILDREN==-1 and self.children and self.children[0].IS_SLOT:
            # make the slot filling the whole window
            self.children[0].widget.SetSize(self.widget.GetClientSize())

    def on_child_pasted(self):
        if not self.widget: return
        if self.parent.WX_CLASS=="wxSplitterWindow":
            self.parent.widget.UpdateSize()

    def _build_from_font(self, font):
        families = np.FontProperty.font_families_from
        styles   = np.FontProperty.font_styles_from
        weights  = np.FontProperty.font_weights_from
        return [font.GetPointSize(), families.get(font.GetFamily(), 'default'), styles.get(font.GetStyle(), 'normal'),
                 weights.get(font.GetWeight(), 'normal'), int(font.GetUnderlined()), font.GetFaceName()]

    def _set_font(self):
        if not self.widget: return
        font_p = self.properties["font"]
        if not font_p.is_active():
            try:
                font = wx.Font()
                font.SetNativeFontInfo( self._original["font"] )
            except TypeError:
                # wxPython 2.8
                font = self.widget.GetFont()
                font.SetNativeFontInfoFromString( self._original["font"] )
        else:
            font = font_p.value
            families = np.FontProperty.font_families_to
            styles = np.FontProperty.font_styles_to
            weights = np.FontProperty.font_weights_to
            font = wx.Font( font[0], families[font[1]], styles[font[2]], weights[font[3]], font[4], font[5])

        self.widget.SetFont(font)

    def set_size(self, prop_name, method="SetSize"):
        size_p = self.properties[prop_name]
        if not self.widget or not size_p.is_active(): return
        size = size_p.get_size(self.widget)
        getattr(self.widget, method)(size)
        if self.parent.IS_SIZER and method=="SetSize":
            self.parent.set_item_best_size(self, size=size)  # actually, this will call SetItemMinSize

    def _properties_changed(self, modified, actions):
        # XXX check whether actions are required
        if modified and "size" in modified and self.widget:
            self.set_size("size", "SetSize")
        if modified and "min_size" in modified and self.widget:  # only used by frame
            self.set_size("min_size", "SetMinSize")
        if modified and "max_size" in modified and self.widget:
            self.set_size("max_size", "SetMaxSize")
        if not modified or "background" in modified and self.widget:
            self.widget.SetBackgroundColour(self.properties["background"].get_color())
            actions.add("refresh")
        if not modified or "foreground" in modified and self.widget:
            self.widget.SetForegroundColour(self.properties["foreground"].get_color())
            actions.add("refresh")
        if "font" in modified and self.widget:
            self._set_font()
            actions.add("layout")

        EditBase._properties_changed(self, modified, actions)

    def get_property_handler(self, name):
        if name in ('font', 'label_font', 'cell_font'):
            return FontHandler(self)
        elif name == 'extraproperties':
            return ExtraPropertiesPropertyHandler(self)
        return EditBase.get_property_handler(self, name)

    def properties_changed(self, modified):
        actions = EditBase.properties_changed(self, modified)
        # widget properties modified; trigger updates
        if self.widget:
            if config.debugging: print("Actions", actions)
            if "recreate2" in actions:
                self.recreate_widget2()
                self.layout()
            elif "recreate" in actions:
                self.recreate_widget()
                self.layout()
            elif "refresh" in actions:
                self.widget.Refresh()
            if "layout" in actions:
                self.layout()
            if "sizeevent" in actions:
                wx.SafeYield()  # required for gtk when e.g. increasing the font size of a label or button
                compat.wxWindow_SendSizeEventToParent(self.widget)
        return actions

    def get_properties(self, without=set()):
        if "foreground" in self.properties:
            if not self.properties["foreground"].is_active(): without.add("foreground")
            if not self.properties["background"].is_active(): without.add("background")
        return EditBase.get_properties(self, without)


class ManagedBase(WindowBase):
    """Base class for every window managed by a sizer.

    Extends WindowBase with the addition of properties relative to the layout of the window:
    proportion/option, flag, and border."""

    _PROPERTIES = ["Layout", "span", "proportion", "border", "flag"]
    SIZER_PROPERTIES = ["proportion","border","flag"]
    PROPERTIES = WindowBase.PROPERTIES + _PROPERTIES
    np.insert_after(PROPERTIES, "size", "max_size")

    _PROPERTY_HELP = { "border": "Border width, if enabled below",
                       "max_size": "Indicate maximum size to the containing sizer."}
                       
    _PROPERTY_LABELS = {"option": "Proportion" }

    #CHILDREN = 0  # most widgets have no children

    ####################################################################################################################

    def __init__(self, name, parent, index, instance_class=None, class_=None):
        WindowBase.__init__(self, name, parent, index, class_, instance_class)
        # if True, the user is able to control the layout of the widget
        # inside the sizer (proportion, borders, alignment...)
        self._has_layout = parent.IS_SIZER
        if self._has_layout:
            self.max_size  = np.SizePropertyD( "-1, -1", default_value="-1, -1" )

        # attributes to keep the values of the sizer properties
        if index is None:
            if self in self.parent.children:
                index = self.parent.children.index(self)
            else:
                index = len(self.parent.children) - 1
        self.span       = np.LayoutSpanProperty((1,1))         # cell spanning for GridBagSizer
        self.proportion = np.LayoutProportionProperty(0)       # item growth in sizer main direction
        self.border     = np.SpinProperty(0, immediate=True)   # border width
        self.flag       = np.ManagedFlags(0)                   # alignment, border; expansion in other dir.

    def check_defaults(self):
        # apply default border if set in preferences; called explicitely from the interactive builder functions
        if not config.preferences.default_border or self.border==config.preferences.default_border_size: return
        self.properties["border"].set( config.preferences.default_border_size )
        flag_p = self.properties["flag"]
        if not flag_p.value_set.intersection(flag_p.FLAG_DESCRIPTION["Border"]):
            flag_p.add("wxALL", notify=False)

    def finish_widget_creation(self, level, sel_marker_parent=None, re_add=True):
        if sel_marker_parent is None: sel_marker_parent = self.parent_window.widget
        self.sel_marker = misc.SelectionMarker(self.widget, sel_marker_parent)
        WindowBase.finish_widget_creation(self, level)
        self.widget.Bind(wx.EVT_LEFT_DOWN, self.on_set_focus)
        self.widget.Bind(wx.EVT_MOUSE_EVENTS, self.on_mouse_events)
        self.widget.Bind(wx.EVT_MOVE, self.on_move)

    def update_view(self, selected):
        if self.sel_marker: self.sel_marker.Show(selected)

    def on_move(self, event):
        if self.sel_marker: self.sel_marker.update()

    def on_size(self, event):
        if not self.widget: return
        old = self.size
        WindowBase.on_size(self, event)
        size_p = self.properties['size']
        if size_p.is_active():
            #if self.proportion!=0 or (self.flag & wx.EXPAND):
            size_p.set(old)
        if self.sel_marker: self.sel_marker.update()

    def _properties_changed(self, modified, actions):
        WindowBase._properties_changed(self, modified, actions)
        p = self.properties["flag"]
        if common.history: common.history.monitor_property( p )
        if modified and "flag" in modified and self.parent.IS_SIZER:
            p._check_value()

        if "flag" in modified and "wxSHAPED" in p.value_set and self.proportion:
            if common.history: common.history.monitor_property( self.properties["proportion"] )
            self.properties["proportion"].set(0, notify=False)
        elif "option" in modified and self.proportion and "wxSHAPED" in p.value_set:
            p.remove("wxSHAPED", notify=False)

        if "border" in modified and self.border and not "flag" in modified:
            # enable border flags if not yet done
            if not p.value_set.intersection(p.FLAG_DESCRIPTION["Border"]):
                p.add("wxALL", notify=False)
                modified.append("flag")

        if not modified or ("option" in modified or "flag" in modified or "border" in modified or
            "size" in modified or "span" in modified):
            if self.parent.WX_CLASS=="wxGridBagSizer" and (not modified or "span" in modified) and self.span!=(1,1):
                # check span range, if pasted item would span more rows/cols than available
                span_p = self.properties["span"]
                max_span = self.sizer.check_span_range(self.index, *span_p.value)
                max_span = ( min(span_p.value[0],max_span[0]), min(span_p.value[1],max_span[1]) )
                if max_span!=span_p.value:
                    if common.history: common.history.monitor_property( span_p )
                    span_p.set(max_span, notify=False)
            if self.parent.IS_SIZER:
                self.sizer.item_properties_modified(self, modified)
                actions.add("layout")

        # if an item inside a flex grid sizer is set to EXPAND, inform the user if row and col are not growable
        if modified and "flag" in modified and self.parent.IS_SIZER and "growable_rows" in self.parent.properties:
            if p.previous_value is not None and "wxEXPAND" in p.value_set and not "wxEXPAND" in p.previous_value:
                row, col = self.parent._get_row_col(self.index)
                if not row in self.parent.growable_rows and not col in self.parent.growable_cols:
                    wx.CallAfter(self.parent.ask_growable, row,col)

        #if modified and self.widget:
            #wx.SafeYield()  # required for gtk when e.g. increasing the font size of a label or button
            #self.widget.SendSizeEventToParent()
        if modified: actions.add("sizeevent")

    #def _set_widget_best_size(self):
        ## called when the widget has been modified and this might affect the automatic size
        #if not self.widget: return
        #size_p = self.properties["size"]
        #if size_p.is_active() and size_p.get() != "-1, -1": return # fixed size
        ## find best size, apply; display if size property is not active
        #self.widget.SetMinSize( (-1,-1) )  # otherwise the size would often not be reduced, e.g. for buttons
        
        #print("skipping SET BESET SIZE")
        ##if hasattr(self.parent, "set_item_best_size"):
            ##best_size = self.widget.GetBestSize()
            ##print("SET BESET SIZE", best_size)
            ##self.parent.set_item_best_size(self, best_size)
            ##if not size_p.is_active():
                ##size_p.set( best_size )

    def destroy_widget(self, level, later=True):
        if self.widget and self.sel_marker:
            self.sel_marker.Destroy()  # destroy the selection markers
            self.sel_marker = None
        WindowBase.destroy_widget(self, level, later)

    def _remove(self):
        "don't set focus"
        return self.parent._free_slot(self.index)

    def remove(self, user=True):
        # entry point from GUI
        #with self.frozen():  # this does not work on mac os: when deleting a panel notebook page, it will remain black
        if user: common.history.widget_removing(self)
        slot = self._remove()
        misc.rebuild_tree(slot, recursive=False)
        if user: common.history.widget_removed(slot)
        return slot

    def on_mouse_events(self, event):
        if event.Dragging():
            # start drag & drop
            window = misc.get_toplevel_parent(self)
            clipboard.begin_drag(window, self)
            return
        event.Skip()


class PreviewMixin(object):
    def __init__(self):
        self.preview = np.ActionButtonProperty(self.on_preview)
        self.preview.set_label( _('Show Preview') )
        self.preview_widget = None
        self._preview_position = None

    def preview_is_visible(self):
        "True if the preview_widget was created"
        return self.preview_widget is not None

    def on_preview(self, refresh=False):
        if self.preview_widget and compat.IS_PHOENIX and config.debugging and wx.KeyboardState().ShiftDown():
            # print structure for debugging
            import utilities
            utilities.StructurePrinter(self.preview_widget)
            return
        new_label = None
        if self.preview_widget is None:
            self.preview_widget = common.root.preview(self, self._preview_position)
            if self.preview_widget:
                new_label = _('Close Preview')
                if wx.Platform == '__WXGTK__':  # otherwise e.g. preview of Test_Editing2 fails sometimes
                    wx.SafeYield()
                    compat.wxWindow_SendSizeEventToParent(self.preview_widget)
        else:
            self._preview_position = self.preview_widget.GetPosition()  # remember position
            self.preview_widget._close_method(self.preview_widget)
            self.preview_widget = None
            new_label = _('Show Preview')
            if refresh: wx.CallAfter(self.on_preview)
        if new_label is not None:
            self.properties["preview"].set_label(new_label)


class DesignButtonProperty(np.ActionButtonProperty):
    def __init__(self, callback):
        np.ActionButtonProperty.__init__(self, callback)
        self.background_color = wx.Colour(150,150,200)
        self.set_label( _('Show Design Window') )

    def update_label(self):
        if not self.owner.widget or not self.owner.is_visible():
            label = _('Show Design Window')
            self.background_color = wx.Colour(150,150,240)  # make button more visible
        else:
            label = _('Hide Design Window')
            self.background_color = compat.wx_SystemSettings_GetColour(wx.SYS_COLOUR_BTNFACE)
        self.set_label(label)


class TopLevelBase(WindowBase, PreviewMixin):
    "Base class for every non-managed window (i.e. Frames, Dialogs and TopLevelPanel)"
    PROPERTIES = WindowBase.PROPERTIES + ["design","preview"]
    np.insert_after(PROPERTIES, "name", "class", "custom_base")

    IS_TOPLEVEL = True
    IS_TOPLEVEL_WINDOW = True  # will be False for TopLevelPanel and MDIChildFrame
    CHILDREN = 1  # a sizer or a widget

    _PROPERTY_HELP={ "extracode_pre": "This code will be inserted at the beginning of the constructor.",
                     "extracode_post":"This code will be inserted at the end of the constructor." }

    def __init__(self, name, parent, index, klass, title=None):
        WindowBase.__init__(self, name, parent, index, klass)
        self._oldname = name
        if "title" in self.PROPERTIES:
            self.title = np.TextProperty(title if title is not None else name)
        PreviewMixin.__init__(self)
        self.design = DesignButtonProperty(self.on_design_button)

    @property
    def window_sizer(self):
        # return the main sizer for this window
        if len(self.children)!=1: return None
        if self.children[0] is None: return None
        if self.children[0].IS_SIZER: return self.children[0]
        return None

    def create(self):
        # creates/shows the widget of the given toplevel node and all its children
        wx.BeginBusyCursor()
        try:
            WindowBase.create(self)
        finally:
            wx.EndBusyCursor()
        # from old code:
        # below, probably check_prop should be False
        ## set the best size for the widget (if no one is given)
        #if not self.check_prop('size'):
            #if self.sizer:  # self.sizer is the containing sizer, i.e. the parent
                #self.sizer.fit_parent()
            #elif self.WX_CLASS=="wxPanel" and self.children:
                #wx.Yield()  # by now, there are probably many EVT_SIZE in the queue
                #self.children[0].fit_parent()

        self.widget.GetTopLevelParent().Show()
        # SafeYield and SetFocus are required for e.g. Ubuntu w. Python 3.8 and wxPython 4.0.7
        # see file SIMPLIFICATIONS\Tests_full.wxg where the first frame will not be layouted
        wx.SafeYield()
        self.widget.Raise()
        self.widget.SetFocus()

    def show_widget(self):
        if not self.widget:
            self.create()
        else:
            self.widget.GetTopLevelParent().Show()  # GetTopLevelParent is required for e.g. Panel
            self.widget.GetTopLevelParent().Raise()

    def finish_widget_creation(self, level):
        WindowBase.finish_widget_creation(self, level)

        if self.CHILDREN:  # not for MenuBar, ToolBar
            self.drop_target = clipboard.DropTarget(self)
            self.widget.SetDropTarget(self.drop_target)

        self.widget.SetMinSize = self.widget.SetSize
        if self.check_prop("title"):
            self.widget.SetTitle( misc.design_title(self.title) )
        elif hasattr(self.widget, 'SetTitle'):
            self.widget.SetTitle(misc.design_title(self.name))
        self.widget.Bind(wx.EVT_LEFT_DOWN, self.drop_sizer)
        self.widget.Bind(wx.EVT_ENTER_WINDOW, self.on_enter)
        self.widget.Bind(wx.EVT_CLOSE, self.hide_widget)
        if wx.Platform == '__WXMSW__':
            # MSW isn't smart enough to avoid overlapping windows, so at least move it away from the 3 wxGlade frames
            self.widget.Center()

    def destroy_widget(self, level, later=True):
        if self.preview_widget is not None:
            self.preview_widget.Unbind(wx.EVT_CHAR_HOOK)
            compat.DestroyLater(self.preview_widget)
            self.preview_widget = None
        WindowBase.destroy_widget(self, level, later)

    def hide_widget(self, event=None):
        self.widget.Hide()  # just hide, don't close
        common.app_tree.Collapse(self.item)
        self.design.update_label()

    def duplicate(self, *args):
        clipboard.copy(self)
        clipboard.paste(common.root)

    def _create_popup_menu(self, widget):
        # remove, hide
        menu = misc.wxGladePopupMenu(self.name)
        widgetclass = self.__class__.__name__.lstrip("Edit")

        if self.widget and self.is_visible():
            # hide window
            i = misc.append_menu_item(menu, -1, _('Hide Design Window'))
            misc.bind_menu_item_after(widget, i, self.hide_widget)
        else:
            i = misc.append_menu_item(menu, -1, _('Show Design Window'))
            misc.bind_menu_item_after(widget, i, common.app_tree.show_toplevel, None, self)

        menu.AppendSeparator()
        i = misc.append_menu_item(menu, -1, _('Remove %s\tDel')%widgetclass, wx.ART_DELETE)
        misc.bind_menu_item_after(widget, i, self.remove)

        i = misc.append_menu_item(menu, -1, _('Duplicate %s')%widgetclass, wx.ART_COPY)
        misc.bind_menu_item_after(widget, i, self.duplicate)

        # paste
        if clipboard.check("menubar") and "menubar" in self.properties:
            i = misc.append_menu_item(menu, -1, _('Paste MenuBar\tCtrl+V'), wx.ART_PASTE)
            misc.bind_menu_item_after(widget, i, clipboard.paste, self)
            if self.menubar: i.Enable(False)
        elif clipboard.check("toolbar") and "toolbar" in self.properties:
            i = misc.append_menu_item(menu, -1, _('Paste ToolBar\tCtrl+V'), wx.ART_PASTE)
            misc.bind_menu_item_after(widget, i, clipboard.paste, self)
            if self.toolbar: i.Enable(False)
        elif clipboard.check("statusbar") and "statusbar" in self.properties:
            i = misc.append_menu_item(menu, -1, _('Paste StatusBar\tCtrl+V'), wx.ART_PASTE)
            misc.bind_menu_item_after(widget, i, clipboard.paste, self)
            if self.statusbar: i.Enable(False)
        else:
            i = misc.append_menu_item(menu, -1, _('Paste Sizer\tCtrl+V'), wx.ART_PASTE)
            misc.bind_menu_item_after(widget, i, clipboard.paste, self)
            # XXX change later on to allow other widgets to be pasted
            if self.children or not clipboard.check("sizer"): i.Enable(False)

        # preview
        menu.AppendSeparator()
        i = misc.append_menu_item(menu, -1, _('Preview %s\tF5'%widgetclass))
        misc.bind_menu_item(widget, i, self.preview_parent)

        return menu

    def on_design_button(self):
        "Button 'Show Design Window' was pressed"
        if self.widget and config.debugging and wx.GetKeyState(wx.WXK_SHIFT):
            import utilities
            utilities.StructurePrinter(self.widget)
            return
        if not self.widget or not self.is_visible():
            common.app_tree.show_toplevel(None, self)
        else:
            self.hide_widget()
        self.design.update_label()

    ####################################################################################################################
    def check_compatibility(self, widget, typename=None):
        "check in advance whether widget can be pasted"

        if widget is not None:
            if widget.WX_CLASS=="wxMenuBar":   typename = "menubar"
            elif widget.WX_CLASS=="wxToolBar": typename = "toolbar"
            elif widget.WX_CLASS=="wxStatusBar": typename = "statusbar"

        if typename in ("menubar", "toolbar", "statusbar"):
            if not typename in self.properties:
                return (False, "Can't set a menu, tool or status bar")
            if self.check_prop(typename) and getattr(self, typename):
                return (False, 'Menu, tool or status bar already set for this window')
            return (True, None)

        if widget and widget.IS_TOPLEVEL:
            # a toplevel dragged internally on another toplevel -> just re-order
            return ("Reorder", None)

        if self.children and not self.children[0].IS_SLOT:
            return (False, 'Sizer or child widget already set for this window')

        if typename is not None:
            #if typename!="sizer":
                #return (False,'Only sizers can be pasted here')
            return (True,None)
        #import edit_sizers
        #if not isinstance(widget, edit_sizers.Sizer):
        #if not widget.IS_SIZER:
            #return (False, 'Only sizers can be pasted here')
        return (True,None)

    def clipboard_paste(self, clipboard_data):
        "Insert a widget from the clipboard to the current destination"
        if self.widget: size = self.widget.GetSize()
        ret = clipboard._paste(self, 0, clipboard_data)
        if self.widget: self.widget.SetSize(size)
        return ret

    ####################################################################################################################
    def set_sizer(self, sizer):
        assert sizer in self.children
        if sizer.widget and self.widget:
            self.widget.SetAutoLayout(True)
            self.widget.SetSizer(sizer.widget)
            self.widget.Layout()

    def on_enter(self, event):
        if not self.children and common.adding_sizer:
            self.widget.SetCursor(wx.CROSS_CURSOR)
        else:
            self.widget.SetCursor(wx.STANDARD_CURSOR)

    def drop_sizer(self, event=None, reset=None):
        if self.children:# or not common.adding_sizer:
            self.on_set_focus(event)  # default behaviour: call show_properties
            return
        if self.widget: self.widget.SetCursor(wx.STANDARD_CURSOR)
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
            return (False, 'Sizer or child widget already set for this window')
        #if common.adding_sizer:
        return (True, None)
        #return (False, 'Only sizers can be added here')

    def on_size(self, event):
        WindowBase.on_size(self, event)
        if len(self.children)!=1 or self.children[0] is None or not self.children[0].widget: return
        child = self.children[0]
        #if child.IS_SLOT or child.WX_CLASS in ("wxSplitterWindow", "wxPanel", "wxNotebook"):
        if child.WX_CLASS in ("wxSplitterWindow", "wxPanel", "wxNotebook"):
            # resize element to fill full space; SendSizeEvent in frozen is not enough
            size = self.widget.GetClientSize()
            child.widget.SetSize( size )

    def _properties_changed(self, modified, actions):
        if "title" in self.properties and (not modified or "title" in modified):
            if self.widget: self.widget.SetTitle( misc.design_title(self.title) )

        if not modified or "name" in modified and (self.name!=self._oldname):
            self.parent.update_top_window_name(self._oldname, self.name)
            self._oldname = self.name

        if not modified or "class" in modified:
            actions.add("label")

        WindowBase._properties_changed(self, modified, actions)

    def _find_widget_by_pos(self, w, x,y, level=1):
        "helper for find_widget_by_pos; w is the parent window/widget"
        if w.HasMultiplePages():
            page = w.GetPage(w.GetSelection())
            x0,y0,width,height = w.GetRect()
            return self._find_widget_by_pos(page, x-x0,y-y0, level+1)
        ret = []
        # check the widget itself; except for the toplevel, this is redundant
        if w.IsTopLevel():  # for a Frame, Rect is the screen position
            x0,y0,width,height = w.GetClientRect()
        else:
            x0,y0,width,height = w.GetRect() # c.GetClientRect()
        if x0 <= x <= x0+width and y0 <= y <= y0+height:
            ret.append(w)
        # check the children; these are relative to this widget, so adjust x/y
        x -= x0
        y -= y0
        for c in w.GetChildren():
            x0,y0,width,height = c.GetRect() # c.GetClientRect()
            if x0 <= x <= x0+width and y0 <= y <= y0+height:
                ret.append(c)
            if isinstance(c, wx.ScrolledWindow):
                ret += self._find_widget_by_pos(c, x-x0,y-y0, level+1)
            else:
                ret += self._find_widget_by_pos(c, x,y, level+1)
        return ret

    def find_editor_by_pos(self, x,y):
        "find the Edit item at a given position"
        if self.widget is None: return None
        found = self._find_widget_by_pos(self.widget, x,y)
        while found:
            w = self._find_editor( found.pop(-1), self )
            if w: return w
        return None

    def _find_editor(self, widget, node):
        # wx widget to editor
        if node.widget is None:
            return
        if widget is node.widget: return node
        button = getattr(node, "_btn", None)
        if button is not None and widget is button: return node
        if hasattr(node.widget, "GetStaticBox"):
            if widget is node.widget.GetStaticBox(): return node
        if node.children:
            for child in node.children:
                found = self._find_editor(widget, child)
                if found is not None:
                    return found
        return None


class EditStylesMixin(np.PropertyOwner):
    """Mixin to handle styles within widget dialogs

    This class needs the wxWidget class to get the proper widget writer.
    Mostly the wxWidget class is stored in self.base. If not you've to set
    manually using constructors 'klass' parameter. The 'klass' parameter
    will preferred used.

    style_set: Set of selected styles (strings)
    style_names: List of style names
    widget_writer: Widget code writer (wcodegen.BaseWidgetWriter)"""
    codegen = None             # Code generator class; see: codegen.BaseLangCodeWriter
    update_widget_style = True # Flag to update the widget style if a style is set using set_style()
    recreate_on_style_change = False

    def __init__(self, style=0, styles=[]):
        """Initialise instance

        klass: Name of the wxWidget klass
        styles: Supported styles, for more details see widget_properties.CheckListProperty; list or OrderedDict"""

        self.style_names = []

        if not self.codegen:
            EditStylesMixin.codegen = common.code_writers['preview']

        try:
            self.widget_writer = self.codegen.obj_builders[self.WX_CLASS]
        except KeyError:
            raise NotImplementedError
        if styles:
            if isinstance(styles, dict):
                for box_label in styles.keys():
                    self.style_names.extend(styles[box_label])
            else:
                self.style_names = styles
        else:
            self.style_names = self.widget_writer.style_list
        self.style = np.WidgetStyleProperty(style)  # this will read it's default value

    @decorators.memoize
    def wxname2attr(self, name):
        """Return the attribute specified by the name. Only wx attributes are supported.

        Example::
            >>> self.wxname2attr('wx.version')
            <function version at 0x2cc6398>
            >>> self.wxname2attr('wx.VERSION')
            (2, 8, 12, 1, '')

        note: Exceptions especially NameError and AttributeError aren't caught."""
        assert name.startswith('wx')

        #cn = self.codegen.get_class(self.codegen.cn(name))
        cn = self.codegen.cn(name)
        namespace, cn = cn.rsplit(".",1)
        if namespace=="wx":
            import wx
            return getattr(wx, cn)
        if namespace=="wx.propgrid":
            import wx.propgrid
            return getattr(wx.propgrid, cn)
        if namespace=="wx.grid":
            import wx.grid
            return getattr(wx.propgrid, cn)
        raise ValueError("namespace %s not implemented"%namespace)

    def _properties_changed(self, modified, actions):
        if not modified or not "style" in modified: return
        if not self.widget or not self.update_widget_style: return
        if "recreate" in actions or "recreate2" in actions: return
        old_style = self.widget.GetWindowStyleFlag()
        new_style = self.style
        if old_style == new_style: return

        if self.recreate_on_style_change:
            actions.add("recreate2")
        if self.WX_CLASS == "wxButton" and (old_style & wx.BU_EXACTFIT != new_style & wx.BU_EXACTFIT):
            actions.add("recreate2")  # workaround

        if not "recreate2" in actions:
            # update style without re-creating the widget
            self.widget.SetWindowStyleFlag(new_style)
            actions.add("refresh")


class Slot(edit_base.Slot):
    pass
