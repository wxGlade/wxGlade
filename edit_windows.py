"""
Base classes for windows used by wxGlade

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import logging, re
import wx

import new_properties as np
import code_property as cp
import misc, common, compat, config, clipboard
import decorators
from wcodegen.taghandler import BaseXmlBuilderTagHandler

from events_mixin import EventsMixin


class FontHandler(BaseXmlBuilderTagHandler):
    item_attrs = {'size': 0, 'family': 1, 'style': 2, 'weight': 3, 'underlined': 4, 'face': 5}
    strip_char_data = True

    def __init__(self, owner):
        super(FontHandler, self).__init__()
        self.owner = owner
        self.props = [8, 'default', 'normal', 'normal', 0, 'Tahoma'] # size, family, style, weight, underlined, face
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



class EditBase(EventsMixin, np.PropertyOwner):
    """Base class of every window available in the builder.

    This class holds the basic properties for this object.
    The properties that control the layout (i.e. the behaviour when inside a sizer) are in L{ManagedBase}."""
    can_preview = False
    _PROPERTIES = ["Common", "name","class", "custom_base"] # "custom_base" will be set to None or a property
    PROPERTIES = _PROPERTIES

    # the following will be placed on the last tab
    _EXTRA_PROPERTIES = ["Events", "events", "Code", "extracode", "extraproperties"]
    EXTRA_PROPERTIES = _EXTRA_PROPERTIES

    _PROPERTY_HELP={ "class": _("If you change the default value, it will be interpreted as the name "
                                "of the subclass of the widget. How this name affects code generation "
                                "depends on the kind (i.e. language) of output. See the docs for more details."),
                     "name":_("Name of the variable for assigning the reference to the created widget instance.\n"
                              "(Valid: alphanumeric characters and underscore, also minus, but not at the beginning)\n"
                              "If the config setting is to have unique names, a non-unique name will be indicated"
                              " by a yellow background"),
                     "custom_base": _("A comma-separated list of custom base classes. The first will be invoked\n"
                                      "with the same parameters as this class, while for the others the default\n"
                                      "constructor will be used. You should probably not use this if \n"
                                      "overwrite existing sources is not set.") }
    _PROPERTY_LABELS = {"custom_base":'Base class(es)'}

    def __init__(self, name, klass, parent, id, custom_class=True):
        np.PropertyOwner.__init__(self)
        # initialise instance logger
        self._logger = logging.getLogger(self.__class__.__name__)

        # initialise instance
        self.parent = parent
        self.id = id          # id used for internal purpose events

        # initialise instance properties
        self.name  = name_p  = np.NameProperty(name)
        self.klass = klass_p = np.TextProperty(klass, name="class") # Name of the object's class: read/write or read only
        if not custom_class: klass_p.readonly = True
        # validation for class
        klass_p.validation_re = re.compile(r'^[a-zA-Z_]+[\w:.0-9-]*$')

        # Name of object's wxWidget class; base and klass are mostly the same, except e.g. wxDialog:
        self.base = np.TextProperty(klass, "base")
        # If true, the user can change the value of the 'class' property:
        self.custom_class = custom_class

        if getattr(self, '_custom_base_classes', False):
            self.custom_base = np.TextPropertyD("", multiline=False)
        else:
            self.custom_base = None

        self.extracode       = cp.CodePropertyD()           # code property
        self.extraproperties = cp.ExtraPropertiesProperty()

        self.widget = None  # this is the reference to the actual wxWindow widget, created when required
        self._rmenu = None  # popup menu
        self._dont_destroy = False

        EventsMixin.__init__(self)

    def create(self):
        "create the wx widget"
        if self.parent is not None and self.parent.widget is None: return
        if self.widget: return
        self.create_widget()
        self.finish_widget_creation()

    def create_widget(self):
        "Initializes self.widget and shows it"
        raise NotImplementedError

    def finish_widget_creation(self, *args, **kwds):
        "Creates the popup menu and connects some event handlers to self.widgets"
        wx.EVT_RIGHT_DOWN(self.widget, self.popup_menu)

    def delete(self):
        """Destructor. deallocates the popup menu, the notebook and all the properties.
        Why we need explicit deallocation? Well, basically because otherwise we get a lot of memory leaks... :)"""
        # first, destroy the popup menu...
        self._destroy_popup_menu()
        # XXX tell property editor

        # ...finally, destroy our widget (if needed)
        if self.widget and not self._dont_destroy:
            self.widget.Destroy()
            self.widget = None
        if misc.focused_widget is self:
            misc.focused_widget = None

    # context menu #####################################################################################################
    def popup_menu(self, event, pos=None):
        self._destroy_popup_menu()
        event_widget = event.GetEventObject()
        menu = self._create_popup_menu(widget=event_widget)
        if pos is None:
            # convert relative event position to relative widget position
            event_pos  = event.GetPosition()                     # event position
            screen_pos = event_widget.ClientToScreen(event_pos)  # screen position
            pos        = event_widget.ScreenToClient(screen_pos)        # client position
        event_widget.PopupMenu(menu, pos=pos)

    def _create_popup_menu(self, widget):
        self._destroy_popup_menu()
        menu = misc.wxGladePopupMenu(self.name)

        # edit: remove/copy/cut
        widgetclass = self.__class__.__name__.lstrip("Edit")
        i = misc.append_menu_item(menu, -1, _('Remove %s\tDel')%widgetclass, wx.ART_DELETE)
        misc.bind_menu_item_after(widget, i, self.remove)
        i = misc.append_menu_item( menu, -1, _('Copy\tCtrl+C'), wx.ART_COPY )
        misc.bind_menu_item_after(widget, i, self.clipboard_copy)
        i = misc.append_menu_item( menu, -1, _('Cut\tCtrl+X'), wx.ART_CUT )
        misc.bind_menu_item_after(widget, i, self.clipboard_cut)
        menu.AppendSeparator()

        # rows/cols if inside a grid sizer
        if "rows" in self.sizer.PROPERTIES:
            i = misc.append_menu_item(menu, -1, _('Insert Row before') )
            misc.bind_menu_item_after(widget, i, self.sizer.insert_row, self.pos)
            i = misc.append_menu_item(menu, -1, _('Insert Column before') )
            misc.bind_menu_item_after(widget, i, self.sizer.insert_col, self.pos)
            if  (self.pos-1)//self.sizer.cols + 1 >= self.sizer.rows:
                # last row
                i = misc.append_menu_item(menu, -1, _('Add Row') )
                misc.bind_menu_item_after(widget, i, self.sizer.insert_row, -1)
            if self.pos % self.sizer.cols == 0:
                # last col
                i = misc.append_menu_item(menu, -1, _('Add Column') )
                misc.bind_menu_item_after(widget, i, self.sizer.insert_col, -1)
            menu.AppendSeparator()

        if not self.sizer.is_virtual():
            # slots
            i = misc.append_menu_item(menu, -1, _('Insert Slot before\tCtrl+I') )
            misc.bind_menu_item_after(widget, i, self.sizer.insert_slot, self.pos)
            i = misc.append_menu_item(menu, -1, _('Insert Slots before...\tCtrl+Shift+I') )
            misc.bind_menu_item_after(widget, i, self.sizer.insert_slot, self.pos, True)

            if self.pos==len(self.sizer.children)-1: # last slot -> allow to add
                i = misc.append_menu_item(menu, -1, _('Add Slot\tCtrl+A') )
                misc.bind_menu_item_after(widget, i, self.sizer.add_slot)
                i = misc.append_menu_item(menu, -1, _('Add Slots...\tCtrl+Shift+A') )
                misc.bind_menu_item_after(widget, i, self.sizer.add_slot, True)
            menu.AppendSeparator()

        # preview (create or close?)
        p = misc.get_toplevel_widget(self)
        if p is not None and p.preview_is_visible():
            item = _('Close preview (%s)\tF5') % p.name
        else:
            item = _('Preview (%s)\tF5') % p.name
        i = misc.append_menu_item( menu, -1, item )
        #misc.bind_menu_item_after(widget, i, self.preview_parent)
        misc.bind_menu_item(widget, i, self.preview_parent)

        self._rmenu = (menu, widget) # store for destryoing and unbinding
        return menu

    def preview_parent(self, *args):
        self._destroy_popup_menu()
        widget = misc.get_toplevel_widget(self)
        if widget is not None:
            wx.CallAfter( widget.preview )  # direct call would result in crash

    def _destroy_popup_menu(self):
        if self._rmenu is None: return
        menu, widget = self._rmenu
        widget.Unbind(wx.EVT_MENU)
        menu.Destroy()
        self._rmenu = None
    ####################################################################################################################

    def remove(self, *args):
        self._dont_destroy = False  # always destroy when explicitly asked
        common.app_tree.remove(self.node)

    def on_set_focus(self, event):
        """Event handler called when a window receives the focus: this in fact is
        connected to a EVT_LEFT_DOWN and not to an EVT_FOCUS, but the effect is the same"""
        misc.set_focused_widget(self)
        #if wxPlatform != '__WXMSW__': event.Skip()

    def get_property_handler(self, prop_name):
        """Returns a custom handler function for the property 'prop_name', used when loading this object from a XML file.
        handler must provide three methods: 'start_elem', 'end_elem' and 'char_data'"""
        return EventsMixin.get_property_handler(self, prop_name)

    def properties_changed(self, modified):
        if not modified or "class" in modified or "name" in modified:
            common.app_tree.refresh_name(self.node)

    # clipboard ########################################################################################################
    def check_compatibility(self, widget):
        # only with slots before/after
        return "Slot"

    def clipboard_copy(self, event=None):
        "Store a widget copy into the clipboard;  @see: L{clipboard.copy()}"
        self._destroy_popup_menu()
        clipboard.copy(self)

    def clipboard_cut(self, event=None):
        "Store a copy of self into the clipboard and delete the widget;  @see: L{clipboard.cut()}"
        self._destroy_popup_menu()
        clipboard.cut(self)
    ####################################################################################################################

    def is_visible(self):
        if not self.widget: return False
        if not self.widget.IsShown(): return False
        if self.widget.IsTopLevel():
            return self.widget.IsShown()
        parent = self.parent
        if parent: return parent.is_visible()
        return self.widget.IsShown()

    def update_view(self, selected):
        """Updates the widget's view to reflect its state, i.e. shows which
        widget is currently selected; the default implementation does nothing."""
        pass

    def post_load(self):
        """Called after the loading of an app from a XML file, before showing the hierarchy of widget for the first time.
        The default implementation does nothing."""
        pass

    def create_extracode_property(self):
        try:
            self.properties['extracode']._show(self.notebook)
            self.properties['extraproperties']._show(self.notebook)
        except KeyError:
            pass

    def set_property_blocking(self, key, item):
        if key in self.property_blocking:
            self.property_blocking[key].append(item)
        else:
            self.property_blocking[key] = [item]

    def get_property_blocking(self, key):
        if key in self.property_blocking:
            return self.property_blocking[key]
        return None

    def remove_property_blocking(self, key, item):
        if key in self.property_blocking:
            for i in range(self.property_blocking[key].count(item)):
                self.property_blocking[key].remove(item)
            if not len(self.property_blocking[key]):
                del self.property_blocking[key]



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


    def __init__(self, name, klass, parent, id):
        EditBase.__init__(self, name, klass, parent, id)

        self.window_id = np.TextPropertyD( "wxID_ANY", default_value="wxID_ANY", name="id" )
        self.size      = np.SizePropertyD( "-1, -1", default_value="-1, -1" )

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
            font = self._build_from_font( compat.wx_SystemSettings_GetFont(wx.SYS_DEFAULT_GUI_FONT) )
            font[1] = 'default'
            self.font = np.FontPropertyD(tuple(font))

        # tooltip, focused, hiden
        self.tooltip    = np.TextPropertyD()
        self.disabled   = np.CheckBoxProperty(False, default_value=False)
        self.focused    = np.CheckBoxProperty(False, default_value=False)
        self.hidden     = np.CheckBoxProperty(False, default_value=False)

    def finish_widget_creation(self, *args, **kwds):
        # store the actual values of foreground, background and font as default, if the property is deactivated later
        self.properties["background"].set_default( self.widget.GetBackgroundColour() )
        self.properties["foreground"].set_default( self.widget.GetForegroundColour() )

        fnt = self.widget.GetFont()
        if not fnt.IsOk():
            fnt = wx.SystemSettings_GetFont(wx.SYS_DEFAULT_GUI_FONT)
        self._original['font'] = fnt

        prop = self.properties
        size_p = prop['size']
        if size_p.is_active():
            #self.widget.SetSize([int(s) for s in size.split(',')])
            self.set_size(size_p.get())
        else:
            # this is a dirty hack: in previous versions <=0.7.2 self.set_size is practically always called
            # set_size then calls self.sizer.set_item(item, pos)
            # without calling this here, e.g. an empty notebook page is not created!
            # XXX this should be made more straightforward
            if "pos" in self.properties:
                self.sizer.set_item(self.pos)
            size_p.set('%s, %s' % tuple(self.widget.GetSize()))

        self.widget.SetBackgroundColour(self.background)  # active or default
        self.widget.SetForegroundColour(self.foreground)  # active or default

        font_p = prop.get('font')
        if font_p and font_p.is_active():
            self._set_font()

        EditBase.finish_widget_creation(self)

        wx.EVT_SIZE(self.widget, self.on_size)
        # after setting various Properties, we must Refresh widget in order to see changes
        self.widget.Refresh()
        wx.EVT_KEY_DOWN(self.widget, misc.on_key_down_event)
        wx.EVT_KEY_UP(self.widget, misc.on_key_down_event)

    def on_size(self, event):
        "Update the value of the 'size' property"
        event.Skip()  # skip first before doing something else
        if not self.widget: return  # this can happen on destruction
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
                weidth_widget, height_widget = self.widget.ConvertPixelSizeToDialog( self.widget.GetSize() )
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

        old_size = self.widget.GetSize()
        self.widget.SetFont(font)
        if not self.properties["size"].is_active():
            size = self.widget.GetSize()
            if size != old_size:
                self.sizer.set_item(self.pos, size=size)

    def set_width(self, value):
        self.set_size((int(value), -1))

    def set_height(self, value):
        self.set_size((-1, int(value)))

    def set_size(self, value):
        #if not self.widget: return
        if self.properties['size'].is_active():
            v = self.properties['size'].get_value().strip()
            use_dialog_units = v and v[-1] == 'd'
        else:
            use_dialog_units = config.preferences.use_dialog_units  # False
        try: "" + value
        except TypeError: pass
        else:  # value is a string-like object
            if value and value.strip()[-1] == 'd':
                use_dialog_units = True
                value = value[:-1]
        try:
            size = [int(t.strip()) for t in value.split(',', 1)]
        except:
            self.properties['size'].set(self.size)
        else:
            if use_dialog_units and value[-1] != 'd': value += 'd'
            self.properties['size'].set(value)
            if self.widget:
                if use_dialog_units: size = wx.DLG_SZE(self.widget, size)
                self.widget.SetMinSize(size)
                self.widget.SetSize(size)
                try:
                    self.sizer.set_item(self.pos, size=size)
                except AttributeError:
                    pass

    def get_property_handler(self, name):
        if name == 'font':
            return FontHandler(self)
        elif name == 'extraproperties':
            import code_property
            return code_property.ExtraPropertiesPropertyHandler(self)
        return EditBase.get_property_handler(self, name)

    def properties_changed(self, modified=None):
        # XXX check whether actions are required
        refresh = False
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
    _is_toplevel = False  # this will be True for a top level widget like Frame

    _PROPERTIES = ["Layout","pos", "proportion", "border", "flag"]
    SIZER_PROPERTIES = ["pos","proportion","border","flag"]
    PROPERTIES = WindowBase.PROPERTIES + _PROPERTIES

    _PROPERTY_HELP = { "border": _("Border width, if enabled below"),  "pos": _("Sizer slot") }
    _PROPERTY_LABELS = {"option": "Proportion" }

    ####################################################################################################################

    def __init__(self, name, klass, parent, id, sizer, pos):
        WindowBase.__init__(self, name, klass, parent, id)
        # if True, the user is able to control the layout of the widget
        # inside the sizer (proportion, borders, alignment...)
        self._has_layout = not sizer.is_virtual()

        self.sel_marker = None  # selection markers (a SelectionMarker instance)

        # attributes to keep the values of the sizer properties
        self.pos        = np.LayoutPosProperty(pos, sizer)                  # position within the sizer, 1-based
        self.proportion = np.SpinProperty(0, name="option", immediate=True) # item growth in sizer main direction
        self.border     = np.SpinProperty(0, immediate=True)                # border width
        self.flag       = np.ManagedFlags(wx.ADJUST_MINSIZE)                # alignment, border; expansion in other dir.

        self.sizer = sizer
        sizer.add_item(self, pos)

    def finish_widget_creation(self, sel_marker_parent=None):
        if sel_marker_parent is None: sel_marker_parent = self.parent.widget
        self.sel_marker = misc.SelectionMarker(self.widget, sel_marker_parent)
        WindowBase.finish_widget_creation(self)
        wx.EVT_LEFT_DOWN(self.widget, self.on_set_focus)
        wx.EVT_MOVE(self.widget, self.on_move)
        # re-add the item to update it
        self.sizer.add_item( self, self.pos, self.proportion, self.flag, self.border, self.widget.GetSize() )

    def update_view(self, selected):
        if self.sel_marker: self.sel_marker.Show(selected)

    def on_move(self, event):
        if self.sel_marker: self.sel_marker.update()

    def on_size(self, event):
        if not self.widget: return
        old = self.size
        WindowBase.on_size(self, event)
        size_prop = self.properties['size']
        if size_prop.is_active():
            if self.proportion!=0 or (self.flag & wx.EXPAND):
                size_prop.set(old)
        if self.sel_marker: self.sel_marker.update()

    def properties_changed(self, modified):
        WindowBase.properties_changed(self, modified)
        if "border" in modified and self.border:
            # enable border flags if not yet done
            p = self.properties["flag"]
            if not p.value_set.intersection(p.FLAG_DESCRIPTION["Border"]):
                p.add("wxALL")

        if "proportion" in modified or "flag" in modified or "border" in modified or "size" in modified:
            self._update_sizer_item()

    def _update_sizer_item(self):
        # update the widget by calling self.sizer.set_item (again)
        if not self.widget: return

        #border = self.border
        #proportion = self.option
        #flags = self.flag
        # get size from property
        try:
            size_prop = self.properties['size']
            if size_prop.is_active(): # or use get_value, which will now return the default value if disabled
                size = self.size
                if size[-1] == 'd':
                    size = size[:-1]
                    use_dialog_units = True
                else:
                    use_dialog_units = False
                size = [int(v) for v in size.split(',')]
                if use_dialog_units:
                    size = wx.DLG_SZE(self.widget, size)
            else:
                if not (self.flag & wx.EXPAND):
                    size = self.widget.GetBestSize()
                else:
                    size = (-1,-1) # would None or wx.DefaultSize be better`?`

            #self.sizer.set_item(self.pos, border=self.border, option=self.option, flag=self.flag, size=size)
            self.sizer.item_layout_property_changed(self.pos, size=size)
        except AttributeError:
            self._logger.exception(_('Internal Error'))

    def _set_widget_best_size(self):
        # called when the widget has been modified and this might affect the automatic size
        if not self.widget: return
        size_p = self.properties["size"]
        if size_p.get() != "-1, -1": return # fixed size
        # find best size, apply; display if size property is not active
        best_size = self.widget.GetBestSize()
        self.sizer.set_item(self.pos, size=best_size)
        if not size_p.is_active():
            size_p.set( best_size )

    def delete(self):
        if self.sel_marker:
            self.sel_marker.Destroy()  # destroy the selection markers
            self.sel_marker = None
        WindowBase.delete(self)

    def remove(self, *args):
        self.sizer.free_slot(self.pos)
        if self.sizer.is_virtual() and not self.sizer.is_fixed():
            WindowBase.remove(self)
        else:
            # focus the freed slot
            misc.set_focused_widget(self.sizer.children[self.pos].item)

    def set_pos(self, value):
        "setter for the 'pos' property: calls self.sizer.change_item_pos"
        self.sizer.change_item_pos( self, min( value+1, len(self.sizer.children)-1 ) )

    def update_pos(self, pos):
        "called by self.sizer.change_item_pos to update the item's position when another widget is moved"
        # XXX currently not used; make np.LayoutPosProperty editable again
        self.properties['pos'].set_value(pos)



class PreviewMixin(object):
    def __init__(self):
        self.preview = np.ActionButtonProperty(self.on_preview)
        self.preview_widget = None

    def preview_is_visible(self):
        "True if the preview_widget was created"
        return self.preview_widget is not None

    def on_preview(self, refresh=False):
        if self.preview_widget is None:
            self.preview_widget = common.app_tree.app.preview(self)
            label = _('Close Preview')
        else:
            self.preview_widget.Close()
            self.preview_widget = None
            label = _('Preview')
            if refresh: wx.CallAfter(self.on_preview)
        self.properties["preview"].set_label(label)



class TopLevelBase(WindowBase, PreviewMixin):
    "Base class for every non-managed window (i.e. Frames and Dialogs)"
    _is_toplevel = True
    _custom_base_classes = True
    PROPERTIES = WindowBase.PROPERTIES + ["preview"]

    def __init__(self, name, klass, parent, id, title=None):
        WindowBase.__init__(self, name, klass, parent, id)
        self._oldname = name
        self.has_title = "title" in self.PROPERTIES
        if self.has_title:
            self.title = np.TextProperty(title or self.name)
        self.sizer = None  # sizer that controls the layout of the children of the window
        PreviewMixin.__init__(self)

    def finish_widget_creation(self, *args, **kwds):
        WindowBase.finish_widget_creation(self)
        self.widget.SetMinSize = self.widget.SetSize
        if self.has_title:
            self.widget.SetTitle( misc.design_title(self.title) )
        elif hasattr(self.widget, 'SetTitle'):
            self.widget.SetTitle(misc.design_title(self.name))
        wx.EVT_LEFT_DOWN(self.widget, self.drop_sizer)
        wx.EVT_ENTER_WINDOW(self.widget, self.on_enter)
        wx.EVT_CLOSE(self.widget, self.hide_widget)
        if wx.Platform == '__WXMSW__':
            # MSW isn't smart enough to avoid overlapping windows, so at least move it away from the 3 wxGlade frames
            self.widget.Center()
        # ALB 2004-10-15
        self.widget.SetAcceleratorTable(common.palette.accel_table)

    def create(self):
        WindowBase.create(self)
        if wx.Platform == '__WXMSW__':
            # more than ugly, but effective hack to properly layout the window on Win32
            if self.properties['size'].is_active():
                w, h = self.widget.GetSize()
                self.widget.SetSize((-1, h+1))
                self.widget.SetSize((-1, h))
            elif self.sizer:
                self.sizer.fit_parent()

    def _create_popup_menu(self, widget):
        # remove, hide
        menu = misc.wxGladePopupMenu(self.name)
        widgetclass = self.__class__.__name__.lstrip("Edit")
        i = misc.append_menu_item(menu, -1, _('Remove %s\tDel')%widgetclass, wx.ART_DELETE)
        misc.bind_menu_item_after(widget, i, self.remove)
        
        
        if self.widget and self.is_visible():
            # hide window
            i = misc.append_menu_item(menu, -1, _('Hide'))
            misc.bind_menu_item_after(widget, i, self.hide_widget)
        else:
            i = misc.append_menu_item(menu, -1, _('Show'))
            misc.bind_menu_item_after(widget, i, common.app_tree.show_toplevel, None, self)

        # paste
        i = misc.append_menu_item(menu, -1, _('Paste Sizer\tCtrl+V'), wx.ART_PASTE)
        misc.bind_menu_item_after(widget, i, self.clipboard_paste)
        if self.sizer is not None: i.Enable(False)
        
        # preview
        menu.AppendSeparator()
        i = misc.append_menu_item(menu, -1, _('Preview %s\tF5'%widgetclass))
        misc.bind_menu_item(widget, i, self.preview_parent)

        self._rmenu = (menu, widget)
        return menu

    ####################################################################################################################
    def check_compatibility(self, widget):
        "check in advance whether widget can be pasted"

        if self.sizer is not None:
            self._logger.warning( _('WARNING: Sizer already set for this window') )
            return False

        import edit_sizers
        if not isinstance(widget, edit_sizers.Sizer):
            self._logger.warning(_('Only sizers can be pasted here'))
            return False
        return True

    def clipboard_paste(self, event=None, clipboard_data=None):
        "Insert a widget from the clipboard to the current destination"
        self._destroy_popup_menu()
        if self.sizer is not None:
            self._logger.warning( _('WARNING: Sizer already set for this window') )
            return
        import xml_parse
        size = self.widget.GetSize()
        try:
            if clipboard.paste(self, None, 0, clipboard_data):
                common.app_tree.app.saved = False
                self.widget.SetSize(size)
        except xml_parse.XmlParsingError:
            import os
            if 'WINGDB_ACTIVE' in os.environ: raise
            self._logger.warning( _('WARNING: Only sizers can be pasted here') )
    ####################################################################################################################

    def set_sizer(self, sizer):
        self.sizer = sizer
        if self.sizer and self.sizer.widget and self.widget:
            self.widget.SetAutoLayout(True)
            self.widget.SetSizer(self.sizer.widget)
            self.widget.Layout()

    def on_enter(self, event):
        if not self.sizer and common.adding_sizer:
            self.widget.SetCursor(wx.CROSS_CURSOR)
        else:
            self.widget.SetCursor(wx.STANDARD_CURSOR)

    def drop_sizer(self, event):
        if self.sizer or not common.adding_sizer:
            self.on_set_focus(event)  # default behaviour: call show_properties
            return
        common.adding_widget = common.adding_sizer = False
        self.widget.SetCursor(wx.STANDARD_CURSOR)
        common.widgets[common.widget_to_add](self, None, None)
        common.widget_to_add = None

    def hide_widget(self, *args):
        self._destroy_popup_menu()
        self.widget.Hide()
        common.app_tree.expand(self.node, False)
        #misc.set_focused_widget(self.node.parent)

    def on_size(self, event):
        WindowBase.on_size(self, event)
        if self.sizer and self.widget:
            self.sizer.refresh()

    def properties_changed(self, modified):
        if self.has_title and (not modified or "title" in modified):
            if self.widget:
                self.widget.SetTitle(misc.design_title(self.title))
            common.app_tree.refresh(self.node)

        if not modified or "name" in modified and (self.name!=self._oldname):
            common.app_tree.app.update_top_window_name(self._oldname, self.name)

        WindowBase.properties_changed(self, modified)

    def delete(self, *args):
        if self.preview_widget is not None:
            self.preview_widget.Destroy()
            self.preview_widget = None
        WindowBase.delete(self)



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
            self.codegen.for_version = wx.VERSION[0:2]
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
        """\
        Set a new widget style if the style has changed

        @note:
            Quote from wxWidgets documentation about changing styles
            dynamically:

            Note that alignment styles (wxTE_LEFT, wxTE_CENTRE and
            wxTE_RIGHT) can be changed dynamically after control creation
            on wxMSW and wxGTK. wxTE_READONLY, wxTE_PASSWORD and
            wrapping styles can be dynamically changed under wxGTK but
            not wxMSW. The other styles can be only set during
            control creation.

        @see: L{EditBase.widget}
        """
        widget = getattr(self, 'widget', None)
        if widget and self.update_widget_style:
            old_style = widget.GetWindowStyleFlag()

            new_style = self.get_int_style()

            if old_style != new_style:
                widget.SetWindowStyleFlag(new_style)
                widget.Refresh()

    @decorators.memoize
    def wxname2attr(self, name):
        """Return the attribute specified by the name. Only wx attributes are supported.

        Example::
            >>> self.wxname2attr('wx.version')
            <function version at 0x2cc6398>
            >>> self.wxname2attr('wx.VERSION')
            (2, 8, 12, 1, '')

        @note: Exceptions especially NameError and AttributeError aren't caught.
        """
        assert name.startswith('wx')

        cn = self.codegen.get_class(self.codegen.cn(name))
        attr = getattr(wx, cn)
        return attr

    def properties_changed(self, modified):
        # XXX add style modfication handling
        pass

