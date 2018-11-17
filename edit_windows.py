"""
Base classes for windows used by wxGlade

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016-2018 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import re
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
        if name == 'font':
            self.owner.properties['font'].set(tuple(self.props), activate=True)
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
    The properties that control the layout (i.e. the behaviour when inside a sizer) are in L{ManagedBase}."""
    can_preview = False
    _PROPERTIES = ["Common", "name","class", "custom_base"] # "custom_base" will be set to None or a property
    PROPERTIES = _PROPERTIES

    # the following will be placed on the last tab
    _EXTRA_PROPERTIES = ["Events", "events", "Code", "extracode", "extracode_pre", "extracode_post", "extraproperties"]
    EXTRA_PROPERTIES = _EXTRA_PROPERTIES

    _PROPERTY_HELP={ "class": _("If you change the default value, it will be interpreted as the name "
                                "of the subclass of the widget.\n\nHow this name affects code generation "
                                "depends on the kind (i.e. language) of output. See the docs for more details."),
                     "name":_("Name of the variable for assigning the reference to the created widget instance.\n\n"
                              "(Valid: alphanumeric characters and underscore, also minus, but not at the beginning)\n"
                              "If the config setting is to have unique names, a non-unique name will be indicated"
                              " by a yellow background"),
                     "custom_base": _("A comma-separated list of custom base classes. The first will be invoked\n"
                                      "with the same parameters as this class, while for the others the default\n"
                                      "constructor will be used. You should probably not use this if \n"
                                      "overwrite existing sources is not set."),
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
    def __init__(self, name, klass, parent, custom_class=True, pos=None):
        edit_base.EditBase.__init__(self, name, parent, pos)
        #np.PropertyOwner.__init__(self)
        ## initialise instance logger
        #self._logger = logging.getLogger(self.__class__.__name__)

        ## initialise instance
        #self.parent = parent
        #self.children = []
        #self.id = wx.NewId()  # id used for internal purpose events

        # initialise instance properties
        self.classname = klass
        self.klass = klass_p = np.ClassProperty(klass, name="class") # Name of the object's class: read/write or read only
        if not custom_class: klass_p.readonly = True  # only used for StatusBar, ToolBar and also non-standalone MenuBar

        # Name of object's wxWidget class; base and klass are mostly the same, except e.g. wxDialog:
        self.base = klass  # not editable; e.g. wxFrame or wxComboBox; used to find the code generator
        # If true, the user can change the value of the 'class' property:
        self.custom_class = custom_class

        if getattr(self, '_custom_base_classes', False):
            self.custom_base = np.TextPropertyD("", multiline=False, default_value=None)
        else:
            self.custom_base = None

        self.extracode       = np.CodeProperty()
        self.extracode_pre   = np.CodeProperty()
        self.extracode_post  = np.CodeProperty()
        self.extraproperties = np.ExtraPropertiesProperty()

        #self.widget = None  # this is the reference to the actual wxWindow widget, created when required
        self._dont_destroy = False
        #self.node = None  # the TreeCtrl node

        EventsMixin.__init__(self)

    def get_property_handler(self, prop_name):
        """Returns a custom handler function for the property 'prop_name', used when loading this object from a XML file.
        handler must provide three methods: 'start_elem', 'end_elem' and 'char_data'"""
        return EventsMixin.get_property_handler(self, prop_name)

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
        i = misc.append_menu_item( menu, -1, _('Copy\tCtrl+C'), wx.ART_COPY )
        misc.bind_menu_item_after(widget, i, clipboard.copy, self)
        i = misc.append_menu_item( menu, -1, _('Cut\tCtrl+X'), wx.ART_CUT )
        misc.bind_menu_item_after(widget, i, clipboard.cut, self)
        menu.AppendSeparator()

        sizer = self.sizer
        if sizer: sizer._add_popup_menu_items(menu, self, widget)

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
        widget = self.toplevel_parent_window # misc.get_toplevel_widget(self)
        if widget is not None:
            wx.CallAfter( widget.preview )  # direct call would result in crash

    ####################################################################################################################

    def on_set_focus(self, event):
        """Event handler called when a window receives the focus: this in fact is
        connected to a EVT_LEFT_DOWN and not to an EVT_FOCUS, but the effect is the same"""
        misc.set_focused_widget(self)
        #if wxPlatform != '__WXMSW__': event.Skip()

    # clipboard ########################################################################################################
    def check_compatibility(self, widget, typename=None, report=False):
        # only with slots before/after
        if typename is not None and typename=="window" or widget.IS_TOPLEVEL:
            return (False,"No toplevel objects can be pasted here")
        return ("Slot",None)

    def check_drop_compatibility(self):
        # checks whether a widget can be dropped here
        return (False, "Items can only be added to empty slots. Add or free a slot first.")

    ####################################################################################################################

    def is_visible(self):
        if not self.widget: return False
        if not self.widget.IsShown() and not isinstance(self.widget, wx.ScrolledWindow): return False
        if self.widget.IsTopLevel():
            return self.widget.IsShown()
        parent = self.parent
        if parent: return parent.is_visible()
        return self.widget.GetParent().IsShown()

    def update_view(self, selected):
        """Updates the widget's view to reflect its state, i.e. shows which
        widget is currently selected; the default implementation does nothing."""
        pass

    @contextlib.contextmanager
    def frozen(self):
        if self.widget:
            toplevel = self.widget.GetTopLevelParent()
            toplevel.Freeze()
        else:
            toplevel = None
        try:
            yield
        finally:
            if toplevel:
                toplevel.Thaw()


class WindowBase(EditBase):
    """Extends EditBase with the addition of the common properties available to
    almost every window: size, background and foreground colours, and font"""
    _PROPERTIES = ["id", "size", "background", "foreground", "font", "tooltip", "disabled", "focused", "hidden"]
    PROPERTIES = EditBase.PROPERTIES + _PROPERTIES

    _PROPERTY_HELP = { "id":"""The "Id" property could be
    1) a constant numeric value
    2) a predefined identifier e.g. wxID_ANY
    3) a predefined variable like a class member e.g. self.myButtonID
    4) a variable assignment e.g. self.myButtonID=?

    The pattern of a variable assignment is always "variable=value". The \
    value could be again a numeric value, a predefined identifier, \
    another predefined variable or "?" a shortcut for "wxNewId()"."""}

    _PROPERTY_LABELS = {"attribute":'Store as attribute'}  # used in many derived widget editors

    IS_WINDOW = True
    CHILDREN = 1  # sizer or something else

    def __init__(self, name, klass, parent, pos=None):
        EditBase.__init__(self, name, klass, parent, pos)

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
            self._font_changed = False # this is True if the user has selected a custom font
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

    def finish_widget_creation(self, *args, **kwds):
        # store the actual values of foreground, background and font as default, if the property is deactivated later
        background_p = self.properties["background"]
        foreground_p = self.properties["foreground"]
 
        fnt = self.widget.GetFont()
        if not fnt.IsOk():
            fnt = wx.SystemSettings_GetFont(wx.SYS_DEFAULT_GUI_FONT)
        self._original['font'] = fnt
 
        size_p = self.properties['size']
        if size_p.is_active():
            self.set_size()
        else:
            # this is a dirty hack: in previous versions <=0.7.2 self.set_size is practically always called
            # set_size then calls self.sizer.set_item(item, pos)
            # without calling this here, e.g. an empty notebook page is not created!
            # XXX this should be made more straightforward
            if "pos" in self.properties:
                #self.sizer.set_item(self.pos)
                self.sizer.item_properties_modified(self)
            size_p.set('%s, %s' % tuple(self.widget.GetSize()))
 
        if background_p.is_active(): self.widget.SetBackgroundColour(self.background)
        if foreground_p.is_active(): self.widget.SetForegroundColour(self.foreground)

        font_p = self.properties.get('font')
        if font_p and font_p.is_active():
            self._set_font()

        EditBase.finish_widget_creation(self)

        self.widget.Bind(wx.EVT_SIZE, self.on_size)
        # after setting various Properties, we must Refresh widget in order to see changes
        self.widget.Refresh()
        #self.widget.Bind(wx.EVT_KEY_DOWN, misc.on_key_down_event)
        self.widget.Bind(wx.EVT_CHAR_HOOK, self.on_char_hook)
    
    def on_char_hook(self, event):
        misc.handle_key_event(event, "design")

    def _reparent_widget(self, widget):
        "call Reparent(self.widget) for all direct children, including those in sizers"
        if isinstance(widget, wx.Window):
            widget.Reparent(self.widget)
        elif isinstance(widget, wx.Sizer):
            if isinstance(widget, wx.StaticBoxSizer):
                # the StaticBox is a widget
                sb = widget.GetStaticBox()
                sb.Reparent(self.widget)
            # go through all children
            for si in widget.GetChildren():
                if si is None: continue
                child = si.GetWindow() or si.GetSizer()
                if child: self._reparent_widget(child)

    def recreate_widget(self):
        "currently used by EditTopLevelPanel to re-create after switch between ScrolledWindow and Panel"
        old_widget = self.widget
        size = self.widget.GetSize()
        with self.frozen():
            self.create_widget()
            self.widget.SetSize(size)
            old_widget.Hide()
            if self.sel_marker:
                self.sel_marker.Destroy()
                self.sel_marker = None

            if self.sizer and not self.sizer.is_virtual():
                self.sizer.widget.Detach(old_widget)
                # finish_widget_creation below will add the new widget to the sizer; alternatively add it here

            sizer = old_widget.GetSizer()
            if sizer:
                self.widget.SetSizer(sizer)
                old_widget.SetSizer(None, False)
                sizer.SetContainingWindow(self.widget)
                self._reparent_widget(sizer)
                #sizer.Layout()
            else:
                for child in self.widget.GetChildren():
                    # actually, for now a panel may only have a sizer as child, so this code is not executed
                    self._reparent_widget(child)
            if old_widget in misc.design_windows:
                misc.design_windows.remove(old_widget)
                misc.design_windows.append(self.widget)
            compat.DestroyLater(old_widget)
            self.finish_widget_creation()  # this will add the new widget to the sizer (default argument re_add=True)

    def on_size(self, event):
        "Update the value of the 'size' property"
        if not self.widget: return  # this can happen on destruction
        if event.GetEventObject() is None: return
        event.Skip()
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
            if size_prop == size_widget: return

            prop_size.set(size_widget)  # set to the actual value, either because wx forced it or just for displaying
        except KeyError:
            logging.exception(_('Internal Error'))

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
            font = self._original["font"]
        else:
            font = font_p.value
            families = np.FontProperty.font_families_to
            styles = np.FontProperty.font_styles_to
            weights = np.FontProperty.font_weights_to
            font = wx.Font( font[0], families[font[1]], styles[font[2]], weights[font[3]], font[4], font[5])

        self.widget.SetFont(font)
        if not self.properties["size"].is_active():
            self.sizer.set_item_best_size(self)

    def set_size(self):
        if not self.widget: return
        size_p = self.properties["size"]
        if not size_p.is_active(): return
        size = size_p.get_size(self.widget)
        self.widget.SetSize(size)
        try:
            self.sizer.set_item_best_size(self, size=size)
        except AttributeError:
            pass

    def get_property_handler(self, name):
        if name == 'font':
            return FontHandler(self)
        elif name == 'extraproperties':
            return ExtraPropertiesPropertyHandler(self)
        return EditBase.get_property_handler(self, name)

    def properties_changed(self, modified=None):
        # XXX check whether actions are required
        refresh = False
        if modified and "size" in modified and self.widget:
            self.set_size()
        if not modified or "background" in modified and self.widget:
            self.widget.SetBackgroundColour(self.properties["background"].get_color())
            refresh = True
        if not modified or "foreground" in modified and self.widget:
            self.widget.SetForegroundColour(self.properties["foreground"].get_color())
            refresh = True
        if "font" in modified and self.widget:
            self._set_font()
        if refresh: self.widget.Refresh()

        EditBase.properties_changed(self, modified)

    def get_properties(self, without=set()):
        if not self.properties["foreground"].is_active(): without.add("foreground")
        if not self.properties["background"].is_active(): without.add("background")
        return EditBase.get_properties(self, without)



class ManagedBase(WindowBase):
    """Base class for every window managed by a sizer.

    Extends WindowBase with the addition of properties relative to the layout of the window:
    proportion/option, flag, and border."""

    _PROPERTIES = ["Layout","pos", "span", "proportion", "border", "flag"]
    SIZER_PROPERTIES = ["pos","proportion","border","flag"]
    PROPERTIES = WindowBase.PROPERTIES + _PROPERTIES

    _PROPERTY_HELP = { "border": _("Border width, if enabled below"),  "pos": _("Sizer slot") }
    _PROPERTY_LABELS = {"option": "Proportion" }

    ####################################################################################################################

    def __init__(self, name, klass, parent, pos):
        WindowBase.__init__(self, name, klass, parent, pos)
        # if True, the user is able to control the layout of the widget
        # inside the sizer (proportion, borders, alignment...)
        self._has_layout = parent.IS_SIZER

        # attributes to keep the values of the sizer properties
        if pos is None: pos = len(self.parent.children) - 1
        self.pos        = np.LayoutPosProperty(pos)            # position within the sizer, 0-based
        self.span       = np.LayoutSpanProperty((1,1))         # cell spanning for GridBagSizer
        self.proportion = np.LayoutProportionProperty(0)       # item growth in sizer main direction
        self.border     = np.SpinProperty(0, immediate=True)   # border width
        self.flag       = np.ManagedFlags(wx.ADJUST_MINSIZE)   # alignment, border; expansion in other dir.

        #parent.add_item(self, pos)

    def check_defaults(self):
        # apply default border if set in preferences; called explicitely from the interactive builder functions
        if not config.preferences.default_border or self.border==config.preferences.default_border_size: return
        self.properties["border"].set( config.preferences.default_border_size )
        flag_p = self.properties["flag"]
        if not flag_p.value_set.intersection(flag_p.FLAG_DESCRIPTION["Border"]):
            flag_p.add("wxALL", notify=False)

    def finish_widget_creation(self, sel_marker_parent=None, re_add=True):
        if sel_marker_parent is None: sel_marker_parent = self.parent.widget
        self.sel_marker = misc.SelectionMarker(self.widget, sel_marker_parent)
        WindowBase.finish_widget_creation(self)
        self.widget.Bind(wx.EVT_LEFT_DOWN, self.on_set_focus)
        self.widget.Bind(wx.EVT_MOUSE_EVENTS, self.on_mouse_events)
        self.widget.Bind(wx.EVT_MOVE, self.on_move)
        if re_add:
            # re-add the item to update it; this is not to be done when a widget is replaced due to style change
            self.parent._add_item( self, self.pos )

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

    def properties_changed(self, modified):
        WindowBase.properties_changed(self, modified)
        p = self.properties["flag"]
        if "flag" in modified and "wxSHAPED" in p.value_set and self.proportion:
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
            if self.sizer._IS_GRIDBAG and (not modified or "span" in modified) and self.span!=(1,1):
                # check span range, if pasted item would span more rows/cols than available
                span_p = self.properties["span"]
                max_span = self.sizer.check_span_range(self.pos, *span_p.value)
                max_span = ( min(span_p.value[0],max_span[0]), min(span_p.value[1],max_span[1]) )
                if max_span!=span_p.value:
                    span_p.set(max_span, notify=False)
            if not self.sizer.is_virtual():
                self.sizer.item_properties_modified(self, modified)

    def _set_widget_best_size(self):
        # called when the widget has been modified and this might affect the automatic size
        if not self.widget: return
        size_p = self.properties["size"]
        if size_p.is_active() and size_p.get() != "-1, -1": return # fixed size
        # find best size, apply; display if size property is not active
        self.widget.SetMinSize( (-1,-1) )  # otherwise the size would often not be reduced, e.g. for buttons
        best_size = self.widget.GetBestSize()
        self.sizer.set_item_best_size(self, best_size)
        if not size_p.is_active():
            size_p.set( best_size )

    def destroy_widget(self):
        if self.sel_marker:
            self.sel_marker.Destroy()  # destroy the selection markers
            self.sel_marker = None
        WindowBase.destroy_widget(self)

    def _remove(self):
        "don't set focus"
        return self.parent.free_slot(self.pos)

    def remove(self):
        with self.frozen():
            slot = self._remove()
        misc.set_focused_widget(slot)

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
        else:
            self._preview_position = self.preview_widget.GetPosition()  # remember position
            self.preview_widget.Close()
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
    _custom_base_classes = True
    PROPERTIES = WindowBase.PROPERTIES + ["design","preview"]

    IS_TOPLEVEL = True
    IS_TOPLEVEL_WINDOW = True  # will be False for TopLevelPanel and MDIChildFrame

    def __init__(self, name, klass, parent, title=None):
        WindowBase.__init__(self, name, klass, parent, pos=None)
        self._oldname = name
        self.has_title = "title" in self.PROPERTIES
        if self.has_title:
            if title is None: title = self.name
            self.title = np.TextProperty(title)
        #self.sizer = None  # sizer that controls the layout of the children of the window
        PreviewMixin.__init__(self)
        self.design = DesignButtonProperty(self.on_design_button)

    @property
    def window_sizer(self):
        # return the main sizer for this window
        if len(self.children)!=1: return None
        if self.children[0].IS_SIZER: return self.children[0]
        return None

    def finish_widget_creation(self, *args, **kwds):
        WindowBase.finish_widget_creation(self)
        self.widget.SetMinSize = self.widget.SetSize
        if self.has_title:
            self.widget.SetTitle( misc.design_title(self.title) )
        elif hasattr(self.widget, 'SetTitle'):
            self.widget.SetTitle(misc.design_title(self.name))
        self.widget.Bind(wx.EVT_LEFT_DOWN, self.drop_sizer)
        self.widget.Bind(wx.EVT_ENTER_WINDOW, self.on_enter)
        self.widget.Bind(wx.EVT_CLOSE, self.hide_widget)
        if wx.Platform == '__WXMSW__':
            # MSW isn't smart enough to avoid overlapping windows, so at least move it away from the 3 wxGlade frames
            self.widget.Center()

    def create(self):
        WindowBase.create(self)
        if wx.Platform == '__WXMSW__':
            # more than ugly, but effective hack to properly layout the window on Win32
            if self.properties['size'].is_active():
                w, h = self.widget.GetSize()
                self.widget.SetSize((-1, h+1))
                self.widget.SetSize((-1, h))
            elif len(self.children)==1 and self.children[0].IS_SIZER:
                self.children[0].fit_parent()
        misc.design_windows.append(self.widget)

    def duplicate(self, *args):
        clipboard.copy(self)
        clipboard.paste(common.root.widget)

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
        i = misc.append_menu_item(menu, -1, _('Paste Sizer\tCtrl+V'), wx.ART_PASTE)
        misc.bind_menu_item_after(widget, i, self.clipboard_paste)
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
    def check_compatibility(self, widget, typename=None, report=True):
        "check in advance whether widget can be pasted"

        if self.children:
            if report:
                return (False, 'Sizer already set for this window')
            return (True,None)

        if typename is not None:
            if typename!="sizer":
                return (False,'Only sizers can be pasted here')
            return (True,None)
        #import edit_sizers
        #if not isinstance(widget, edit_sizers.Sizer):
        if not widget.IS_SIZER:
            return (False, 'Only sizers can be pasted here')
        return (True,None)

    def clipboard_paste(self, clipboard_data):
        "Insert a widget from the clipboard to the current destination"
        if self.widget: size = self.widget.GetSize()
        ret = clipboard._paste(self, None, 0, clipboard_data)
        if self.widget: self.widget.SetSize(size)
        return ret

    ####################################################################################################################
    def set_sizer(self, sizer):
        # XXX replace with add_item
        if self.children:
            self.children[0] = sizer
        else:
            self.children.append(sizer)
        #if sizer and sizer.widget and self.widget:
        if sizer.widget and self.widget:
            self.widget.SetAutoLayout(True)
            self.widget.SetSizer(sizer.widget)
            self.widget.Layout()

    def on_enter(self, event):
        if not self.children and common.adding_sizer:
            self.widget.SetCursor(wx.CROSS_CURSOR)
        else:
            self.widget.SetCursor(wx.STANDARD_CURSOR)

    def drop_sizer(self, event=None):
        if self.children or not common.adding_sizer:
            self.on_set_focus(event)  # default behaviour: call show_properties
            return
        if self.widget: self.widget.SetCursor(wx.STANDARD_CURSOR)
        common.widgets[common.widget_to_add](self, None, None)
        if event is None or not misc.event_modifier_copy(event):
            common.adding_widget = common.adding_sizer = False
            common.widget_to_add = None
        common.root.saved = False

    def check_drop_compatibility(self):
        if self.children:
            return (False, 'Sizer already set for this window')
        if common.adding_sizer:
            return (True, None)
        return (False, 'Only sizers can be added here')

    def hide_widget(self, event=None):
        self.widget.Hide()  # just hide, don't close
        common.app_tree.expand(self, False)
        #misc.set_focused_widget(self.node.parent)
        self.design.update_label()

    def on_size(self, event):
        WindowBase.on_size(self, event)
        sizer = self.window_sizer
        if sizer: sizer.refresh()

    def properties_changed(self, modified):
        if self.has_title and (not modified or "title" in modified):
            if self.widget:
                self.widget.SetTitle(misc.design_title(self.title))
            common.app_tree.refresh(self)

        if not modified or "name" in modified and (self.name!=self._oldname):
            #common.root.update_top_window_name(self._oldname, self.name)
            self.parent.update_top_window_name(self._oldname, self.name)

        WindowBase.properties_changed(self, modified)

    def delete(self, *args):
        if self.preview_widget is not None:
            self.preview_widget.Unbind(wx.EVT_CHAR_HOOK)
            compat.DestroyLater(self.preview_widget)
            self.preview_widget = None
        widget = self.widget
        WindowBase.delete(self)
        if widget is not None and widget in misc.design_windows: misc.design_windows.remove(widget)

    def remove(self):
        if self.IS_TOPLEVEL_WINDOW:
            self.parent.remove_top_window(self.name)
        WindowBase.remove(self)

    def _find_widget_by_pos(self, w, x,y, level=1):
        "helper for find_widget_by_pos; w is the parent window/widget"
        if w.HasMultiplePages():
            page = w.GetPage(w.GetSelection())
            x0,y0,width,height = w.GetRect()
            return self._find_widget_by_pos(page, x-x0,y-y0, level+1)
        ret = []
        # check the widget itself
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

    def find_widget_by_pos(self, x,y):
        "find the widget at a given position"
        if self.widget is None: return None
        x0,y0,width,height = self.widget.ClientRect
        found = self._find_widget_by_pos(self.widget, x-x0,y-y0)
        if not found: return None
        node = None
        while found and node is None:
            node = common.app_tree.find_widget(found.pop(-1))
        if node is None: return None
        return node.widget



class EditStylesMixin(np.PropertyOwner):
    """Mixin to handle styles within widget dialogs

    This class needs the wxWidget class to get the proper widget writer.
    Mostly the wxWidget class is stored in self.base. If not you've to set
    manually using constructors 'klass' parameter. The 'klass' parameter
    will preferred used.

    style_set: Set of selected styles (strings)
    style_names: List of style names
    widget_writer: Widget code writer (wcodegen.BaseWidgetWriter)

    """
    codegen = None             # Code generator class; @see: L{codegen.BaseLangCodeWriter}
    update_widget_style = True # Flag to update the widget style if a style is set using L{set_style()}
    recreate_on_style_change = False

    def __init__(self, klass='', styles=[]):
        """Initialise instance

        klass: Name of the wxWidget klass
        styles: Supported styles, for more details see L{widget_properties.CheckListProperty}; list or OrderedDict"""
        assert klass or hasattr(self, 'base')

        self.style_names = []

        # This class needs the wxWidget class to get the proper widget
        # writer. Mostly the wxWidget class is stored in self.base. If
        # not you've to set manually using constructors 'klass' parameter.
        # The 'klass' parameter will preferred used.
        if klass:
            klass = klass
        elif getattr(self, 'base', None):
            klass = self.base
        else:
            raise TypeError('Can not determinate wxWidgets class')

        # set code generator only once per class
        if not self.codegen:
            self.codegen = common.code_writers['python'].copy()
            self.codegen.for_version = compat.version
            EditStylesMixin.codegen = self.codegen

        try:
            self.widget_writer = self.codegen.obj_builders[klass]
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
        #self.style = np.WidgetStyleProperty(0) # this will use below methods
        self.style = np.WidgetStyleProperty()  # this will read it's default value

    def _set_widget_style(self):
        """Set a new widget style if the style has changed.
        For some widgets style changes are not possible, so they need to be re-created.
        The attribute recreate_on_style_change needs to be True in this case."""
        if not self.widget or not self.update_widget_style: return
        old_style = self.widget.GetWindowStyleFlag()
        new_style = self.style
        if old_style == new_style: return

        recreate = self.recreate_on_style_change
        if self.base == "wxButton" and (old_style & wx.BU_EXACTFIT != new_style & wx.BU_EXACTFIT):
            recreate = True  # workaround

        if not recreate:
            # update style without re-creating the widget
            self.widget.SetWindowStyleFlag(new_style)
            self.widget.Refresh()
            return

        # some widgets can't be updated, e.g. Gauge can't be switched between horizontal and vertical after creation
        # this is for ManagedBase derived classes only
        with self.frozen():
            focused = misc.focused_widget is self
            if self.sel_marker:
                self.sel_marker.Destroy()
                self.sel_marker = None
            old_widget = self.widget
            old_widget.Hide()
            si = self.sizer.widget.GetItem(old_widget)
            self.create_widget()
            compat.SizerItem_SetWindow(si, self.widget)
            compat.DestroyLater(old_widget)
            self.sizer.item_properties_modified(self)  # will call toplevel Refresh as well

            self.finish_widget_creation(re_add=False)
            self.sizer.layout()
            if focused:
                misc.focused_widget = self
                if self.sel_marker: self.sel_marker.Show(True)


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

    def properties_changed(self, modified):
        if "style" in modified:
            self._set_widget_style()


class Slot(edit_base.Slot):
    pass
    #"A window to represent an empty slot, e.g. single slot of a Frame or a page of a Notebook"
    ##PROPERTIES = ["Slot", "pos"]
    #PROPERTIES = []
    #IS_TOPLEVEL = IS_SIZER = IS_WINDOW = False
    #IS_SLOT = True

    #def __init__(self, parent, pos=0, label=None):
        #np.PropertyOwner.__init__(self)
        ## initialise instance logger
        #self._logger = logging.getLogger(self.__class__.__name__)
        #self.klass = self.classname = self.base = "slot"
        #self.label = label

        ## initialise instance properties
        #self.parent = parent

        #self.widget = None       # Reference to the widget resembling the slot (a wx.Window)
        #self.name = "SLOT"
        #self.node = None

    #@property
    #def parent_window(self):
        ## go up to parent until it is no sizer
        #item = self.parent
        #while True:
            #if item.IS_WINDOW: return item
            #item = item.parent

    #@property
    #def toplevel_parent_window(self):
        ## go up to parent until IS_TOPLEVEL is True
        #item = self.parent
        #while True:
            #if item.IS_TOPLEVEL: return item
            #item = item.parent

    #def on_load(self):  # called from XML parser, right after the widget is loaded
        #pass
    #def post_load(self): # called from show_widget
        #pass

    #def update_view(self, selected):
        ## we can ignore selected here, as the repainting only takes place later
        #if self.widget:
            #self.widget.Refresh()

    #def create_widget(self):
        #style = wx.FULL_REPAINT_ON_RESIZE
        #self.widget = wx.Window(self.parent_window.widget, -1, size=(20, 20), style=style)
        #self.widget.SetBackgroundStyle(wx.BG_STYLE_CUSTOM)
        #self.widget.SetAutoLayout(True)
        #self.widget.Bind(wx.EVT_PAINT, self.on_paint)
        #self.widget.Bind(wx.EVT_ERASE_BACKGROUND, self.on_erase_background)
        #self.widget.Bind(wx.EVT_RIGHT_DOWN, self.popup_menu)
        #self.widget.Bind(wx.EVT_LEFT_DOWN, self.on_drop_widget)
        #self.widget.Bind(wx.EVT_MIDDLE_DOWN, misc.exec_after(self.on_select_and_paste))
        #self.widget.Bind(wx.EVT_ENTER_WINDOW, self.on_enter)
        #self.widget.Bind(wx.EVT_LEAVE_WINDOW, self.on_leave)
        ##self.widget.Bind(wx.EVT_CHAR_HOOK, misc.on_key_down_event)  # catch cursor keys   XXX still required?

    #def is_visible(self):
        #return False

    #def create(self):
        #if self.widget: return
        #self.create_widget()

    #def on_enter(self, event):
        ## hack. definitely. but...
        #misc.currently_under_mouse = self.widget
        ## a sizer can be added to sizers or to windows with exactly one child
        #can_add_sizer = self.parent.IS_SIZER or self.parent.CHILDREN is 1
        #if common.adding_widget and (not common.adding_sizer or can_add_sizer):
            #self.widget.SetCursor(wx.CROSS_CURSOR)
        #else:
            #self.widget.SetCursor(wx.STANDARD_CURSOR)
        #event.Skip()

    #def on_leave(self, event):
        ## currently_under_mouse is used to restore the normal cursor, if the
        ## user cancelled the addition of a widget and the cursor is over this slot
        #misc.currently_under_mouse = None
        #event.Skip()

    #def on_paint(self, event):
        #"Handle paint request and draw hatched lines onto the window"
        #if not self.sizer: return  # in deletion
        #dc = wx.PaintDC(self.widget)
        #self._draw_background(dc)

    #def on_erase_background(self, event):
        #dc = event.GetDC()
        #if not dc:
            #dc = wx.ClientDC(self)
            #rect = self.widget.GetUpdateRegion().GetBox()
            #dc.SetClippingRect(rect)
        #self._draw_background(dc, clear=False)

    #def _draw_background(self, dc, clear=True):
        #"draw the hatches on device context dc (red if selected)"
        ## fill background first; propably needed only on MSW and not for on_erase_background
        #size = self.widget.GetSize()
        #small = size[0]<10 or size[1]<10
        #focused = misc.focused_widget is self
        #if clear:
            #if small and focused:
                #dc.SetBackground(wx.Brush(wx.BLUE))
            #else:
                #dc.SetBackground(wx.Brush(wx.LIGHT_GREY))
            #dc.Clear()
        #if small and focused:
            #color = wx.WHITE
        #elif small or not focused:
            #color = wx.BLACK
        #else:
            #color = wx.BLUE

        #if focused:
            #hatch = compat.BRUSHSTYLE_CROSSDIAG_HATCH
        #elif not self.parent.IS_SIZER:
            #hatch = compat.BRUSHSTYLE_FDIAGONAL_HATCH
        #else:
            #if not "cols" in self.parent.PROPERTIES:  # horizontal/vertical sizer or grid sizer?
                #pos = self.pos
            #else:
                #pos = sum( self.sizer._get_row_col(self.pos) )
            #hatch = compat.BRUSHSTYLE_FDIAGONAL_HATCH  if pos%2 else  compat.BRUSHSTYLE_BDIAGONAL_HATCH
        #brush = wx.Brush(color, hatch)
        ## draw hatched lines in foreground
        #dc.SetBrush(brush)
        #size = self.widget.GetClientSize()
        #dc.DrawRectangle(0, 0, size.width, size.height)

    ## context menu #####################################################################################################
    #def popup_menu(self, event, pos=None):
        #event_widget = event.GetEventObject()
        #menu = self._create_popup_menu(widget=event_widget)
        #if pos is None:
            ## convert relative event position to relative widget position
            #event_pos  = event.GetPosition()
            #screen_pos = event_widget.ClientToScreen(event_pos)
            #pos        = event_widget.ScreenToClient(screen_pos)
        #event_widget.PopupMenu(menu, pos)
        #menu.Destroy()

    #def _create_popup_menu(self, widget):
        ## menu title
        #if isinstance(self.parent, GridSizerBase):
            #rows, cols = self.parent._get_actual_rows_cols()
            ## calculate row and pos of our slot
            #row,col = self.parent._get_row_col(self.pos)
            #menu = wx.Menu(_("Slot %d/%d"%(row+1,col+1)))
        #elif "pos" in self.properties:
            #menu = wx.Menu(_("Slot %d"%self.pos))
        #else:
            #menu = wx.Menu(_("Slot"))

        ## edit: paste
        #i = misc.append_menu_item(menu, -1, _('Paste\tCtrl+V'), wx.ART_PASTE)
        #misc.bind_menu_item_after(widget, i, clipboard.paste, self)
        #if not clipboard.check("widget","sizer"): i.Enable(False)
        #menu.AppendSeparator()

        ## slot actions
        #if self.parent.IS_SIZER:
            ## we can add/remove items only from non-virtual sizers
            #if not isinstance(self.parent, EditGridBagSizer):
                #i = misc.append_menu_item(menu, -1, _('Remove Slot\tDel'), wx.ART_DELETE)
                #misc.bind_menu_item_after(widget, i, self.remove)
                #if len(self.parent.children)<=2: i.Enable(False)

            ## if inside a grid sizer: allow removal of empty rows/cols
            #if isinstance(self.parent, GridSizerBase):
                ## check whether all slots in same row/col are empty
                #row_is_empty = col_is_empty = True
                #for i,child in enumerate(self.parent.children):
                    #pos = i+1
                    #if pos==0: continue
                    #child_row, child_col = self.parent._get_row_col(pos)
                    #if child_row==row and not isinstance(child, SizerSlot):
                        #row_is_empty = False
                    #if child_col==col and not isinstance(child, SizerSlot):
                        #col_is_empty = False

                ## allow removal of empty row
                #i = misc.append_menu_item(menu, -1, _('Remove Row %d'%(row+1)) )
                #misc.bind_menu_item_after(widget, i, self.parent.remove_row, self.pos)
                #if not row_is_empty or rows<=1: i.Enable(False)

                ## allow removal of empty col
                #i = misc.append_menu_item(menu, -1, _('Remove Column %d'%(col+1)) )
                #misc.bind_menu_item_after(widget, i, self.parent.remove_col, self.pos)
                #if not col_is_empty or cols<=1: i.Enable(False)
                #menu.AppendSeparator()

            #self.parent._add_popup_menu_items(menu, self, widget)

        #p = self.toplevel_parent_window # misc.get_toplevel_widget(self.sizer)
        ##if p is not None and p.preview_is_visible():
        #if p.preview_is_visible():
            #item = _('Close preview (%s)\tF5') % p.name
        #else:
            #item = _('Preview (%s)\tF5') % p.name

        #i = misc.append_menu_item( menu, -1, item )
        ##misc.bind_menu_item_after(widget, i, self.preview_parent)
        #misc.bind_menu_item_after(widget, i, p.preview)

        #return menu

    ##def preview_parent(self):
        ### context menu callback
        ##p = misc.get_toplevel_widget(self.sizer)
        ##if p is not None:
            ##p.preview()

    #####################################################################################################################
    #def _remove(self):
        ## does not set focus
        ##if self.sizer.is_virtual() or len(self.sizer.children)<=2: return
        #if not self.parent.IS_SIZER: return
        #window = self.toplevel_parent_window
        ##node = self.sizer.children[self.pos].node
        ##with self.sizer.window.frozen():
        #with window.frozen():
            #self.parent.remove_item(self)
            #self.delete()
            #common.app_tree.remove(node)

    #def remove(self):
        ## set focused widget
        #i = self.pos - 1
        #self._remove()
        #if i >= len(self.parent.children):
            #i = len(self.parent.children)-1
        #misc.set_focused_widget( self.parent.children[i] )

    #def on_drop_widget(self, event):
        #"""replaces self with a widget in self.sizer. This method is called
        #to add every non-toplevel widget or sizer, and in turn calls the
        #appropriate builder function (found in the ``common.widgets'' dict)"""
        #if not common.adding_widget:  # widget focused/selecte
            #misc.set_focused_widget(self)
            #if self.widget:
                #self.widget.Refresh()
                #self.widget.SetFocus()
            #return
        #if common.adding_sizer and self.parent.CHILDREN is not 1:
            #return
        #if self.widget:
            #self.widget.SetCursor(wx.NullCursor)
        #common.adding_window = event and event.GetEventObject().GetTopLevelParent() or None
        ## call the appropriate builder
        #common.widgets[common.widget_to_add](self.parent, self.pos)
        #if event is None or not misc.event_modifier_copy(event):
            #common.adding_widget = common.adding_sizer = False
            #common.widget_to_add = None
        #common.root.saved = False

    #def check_drop_compatibility(self):
        #if common.adding_sizer and self.parent.CHILDREN is not 1:
            #return (False, "No sizer can be added here")
        #return (True,None)

    ## clipboard handling ###############################################################################################
    #def check_compatibility(self, widget, typename=None):
        #"check whether widget can be pasted here"
        #if typename is not None:
            #if typename=="sizer" and self.parent.CHILDREN is not 1:
                #return (False, "No sizer can be pasted here")
            #if typename=="window":
                #return (False, "No toplevel object can be pasted here.")
            #return (True,None)

        #if widget.IS_TOPLEVEL:
            #return (False, "No toplevel object can be pasted here.")
        #if self.parent.CHILDREN is not 1 and widget.IS_SIZER:
            ## e.g. a sizer dropped on a splitter window slot; instead, a panel would be required
            #return (False, "No sizer can be pasted here")
        #return (True,None)

    #def clipboard_paste(self, clipboard_data):
        #"Insert a widget from the clipboard to the current destination"
        #return clipboard._paste(self.parent, self.pos, clipboard_data)

    #def on_select_and_paste(self, *args):
        #"Middle-click event handler: selects the slot and, if the clipboard is not empty, pastes its content here"
        #misc.focused_widget = self
        #self.widget.SetFocus()
        #clipboard.paste(self)
    #####################################################################################################################

    #def delete(self):
        ## mainly deletes the widget
        #self.destroy_widget()
        #common.root.saved = False

    #def destroy_widget(self):
        #if self.widget is None: return
        #if misc.currently_under_mouse is self.widget:
            #misc.currently_under_mouse = None

        #self.widget.Hide()

        ## unbind events to prevent new created (and queued) events
        #self.widget.Bind(wx.EVT_PAINT, None)
        #self.widget.Bind(wx.EVT_RIGHT_DOWN, None)
        #self.widget.Bind(wx.EVT_LEFT_DOWN, None)
        #self.widget.Bind(wx.EVT_MIDDLE_DOWN, None)
        #self.widget.Bind(wx.EVT_ENTER_WINDOW, None)
        #self.widget.Bind(wx.EVT_LEAVE_WINDOW, None)
        #self.widget.Bind(wx.EVT_KEY_DOWN, None)
        #if self.parent.IS_SIZER and self.parent.widget:
            #self.parent.widget.Detach(self.widget)  # this will happen during recursive removal only
        #compat.DestroyLater(self.widget)
        #self.widget = None

        #if misc.focused_widget is self:
            #misc.set_focused_widget(None)

    #def _get_tree_label(self):
        #if self.label: return self.label
        #pos = node.widget.pos
        #if node.widget.sizer and isinstance(node.widget.sizer, edit_sizers.GridSizerBase):
            ## row/col
            #sizer = node.widget.sizer
            #rows, cols = sizer._get_actual_rows_cols()
            #row = (pos-1) // cols + 1  # 1 based at the moment
            #col = (pos-1) %  cols + 1
            #return "SLOT  %d/%d"%(row, col)
        #elif isinstance(node.widget.sizer, notebook.notebook.NotebookVirtualSizer):
            #return "Notebook Page %d"%(pos)
        #else:
            #return "SLOT %d"%(pos)


    #def __getstate__(self):
        #state = self.__dict__.copy()
        #del state['_logger']
        #return state

    #def __setstate__(self, state):
        #self.__dict__.update(state)

        ## re-initialise logger instance deleted from __getstate__
        #self._logger = logging.getLogger(self.__class__.__name__)