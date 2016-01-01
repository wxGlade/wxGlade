"""
Base classes for windows used by wxGlade

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

# import general python modules
from ordereddict import OrderedDict
import logging
import math
import re
import types
import wx

# import project modules
from widget_properties import *
import misc
import clipboard
import common
import compat
import config
import decorators
from wcodegen.taghandler import BaseXmlBuilderTagHandler

# event handling support
from events_mixin import EventsMixin


class FontHandler(BaseXmlBuilderTagHandler):

    item_attrs = {'size': 0, 'family': 1, 'style': 2, 'weight': 3,
                  'underlined': 4, 'face': 5}

    strip_char_data = True

    def __init__(self, owner):
        super(FontHandler, self).__init__()
        self.owner = owner
        self.props = ['' for i in range(6)]
        self.index = 0

    def start_elem(self, name, attrs):
        self.index = self.item_attrs.get(name, 5)

    def end_elem(self, name):
        if name == 'font':
            self.owner.properties['font'].set_value(
                repr(self.props))
            self.owner.properties['font'].toggle_active(True)
            self.owner.set_font(repr(self.props))
            return True  # to remove this handler

    def char_data(self, data):
        super(FontHandler, self).char_data(data)
        char_data = self.get_char_data()
        self.props[self.index] = char_data

# end of class FontHandler


class EditBase(EventsMixin):
    """\
    Base class of every window available in the builder.

    @ivar custom_class: If true, the user can change the value of the
                        'class' property
    @type custom_class: bool

    @ivar base: Name of object's wxWidget class; base and klass are mostly
                the same, except e.g. wxDialog
    @type base: str

    @ivar klass: Name of the object's class
    @type klass: str

    @ivar name:  Name of the object
    @type name:  str

    @ivar property_window: Widget inside which Properties of this object are
                           displayed

    @ivar widget: This is the reference to the actual wxWindow widget; it is
                  created only if needed, i.e. when it should become visible

    @ivar access_functions: Getter and setter for each property
    @type access_functions: dict

    @ivar properties: Property instance for each property
    @type properties: dict

    @ivar _logger: Instance specific logger

    @ivar _rmenu: Popup menu
    """

    def __init__(self, name, klass, parent, id, property_window, show=True,
                 custom_class=True):
        """\
        Dictionary of properties relative to this object; the properties that
        control the layout (i.e. the behaviour when inside a sizer) are not
        contained here, but in a separate list (see L{ManagedBase})
        the keys of the dict are the names of the properties

        @param property_window: Widget inside which Properties of this object
                                are displayed

        @param name: Name of the object
        @type name:  str

        @param klass: Name of the object's class
        @type klass:  str

        @param show: Show this widget
        @type show:  bool

        @param custom_class: If true, the user can change the value of the
                            'class' property
        """
        # initialise instance logger
        self._logger = logging.getLogger(self.__class__.__name__)

        # initialise instance
        self.properties = {}
        self.property_blocking = {}
        self.parent = parent
        # id used for internal purpose events
        self.id = id
        self.name = name
        self.klass = klass
        self.base = klass
        self.custom_class = custom_class

        self._dont_destroy = False

        self.access_functions = {
            'name': (lambda: self.name, self.set_name),
            'class': (lambda: self.klass, self.set_klass)
            }

        # these two properties are special and are not listed in
        # 'self.properties'
        self.name_prop = TextProperty(self, 'name', None, label=_("name"))
        self.name_prop.tooltip = _("Name of the variable for assigning "
                                   "the reference to the created widget "
                                   "instance.")
        self.klass_prop = TextProperty(self, 'class', None,
                                       readonly=not custom_class, label=_("class"))
        if custom_class:
            self.klass_prop.tooltip = _("If you change the default value, "
                                        "it will be interpreted as the name "
                                        "of the subclass of the widget. "
                                        "How this name affects code generation "
                                        "depends on the kind (i.e. language) "
                                        "of output. See the docs for "
                                        "more details.")

        if getattr(self, '_custom_base_classes', False):
            self.custom_base = ""

            def get_custom_base(): return self.custom_base

            def set_custom_base(val): self.custom_base = val
            self.access_functions['custom_base'] = (get_custom_base,
                                                    set_custom_base)
            p = self.properties['custom_base'] = TextProperty(
                self, 'custom_base', can_disable=True, enabled=False)
            p.label = _('Base class(es)')
            p.tooltip = _("""\
A comma-separated list of custom base classes. The first will be invoked \
with the same parameters as this class, while for the others the default \
constructor will be used. You should probably not use this if \
"overwrite existing sources" is not set.""")

        self.notebook = None
        self.property_window = property_window

        # popup menu
        self._rmenu = None

        # this is the reference to the actual wxWindow widget; it is created
        # only if needed, i.e. when it should become visible
        self.widget = None

        if show:
            self.show_widget(True)
            property_window.SetSize((250, 340))
            property_window.Show(True)

        EventsMixin.__init__(self)

        # code property
        import code_property
        self.properties['extracode'] = code_property.CodeProperty(self)
        self.properties['extraproperties'] = code_property.ExtraPropertiesProperty(self)

    def show_widget(self, yes):
        if yes and self.widget is None:
            self.create_widget()
            self.finish_widget_creation()
        if self.widget: self.widget.Show(yes)

    def create_widget(self):
        """\
        Initializes self.widget and shows it
        """
        raise NotImplementedError

    def finish_widget_creation(self, *args, **kwds):
        """\
        Creates the popup menu and connects some event handlers to
        self.widgets
        """
        wx.EVT_RIGHT_DOWN(self.widget, self.popup_menu)

    def delete(self):
        """\
        Destructor. deallocates the popup menu, the notebook and all the
        properties. Why we need explicit deallocation? Well, basically because
        otherwise we get a lot of memory leaks... :)
        """
        # first, destroy the popup menu...
        if wx.Platform != '__WXMAC__':
            if self._rmenu: self._rmenu.Destroy()
        # ...then, destroy the property notebook...
        if self.notebook:
            nb_szr = self.notebook.sizer
            self.notebook.DeleteAllPages()
            self.notebook.Destroy()
            if nb_szr is not None: nb_szr.Destroy()
        # ...finally, destroy our widget (if needed)
        if self.widget and not self._dont_destroy:
            self.widget.Destroy()
        if misc.focused_widget is self: misc.focused_widget = None

    def create_properties(self):
        """\
        Creates the notebook with the properties of self
        """
        self.notebook = wx.Notebook(self.property_window, -1)

        self.notebook.sizer = None
        self.notebook.SetAutoLayout(True)
        self.notebook.Hide()

        self._common_panel = panel = wx.ScrolledWindow(
            self.notebook, -1, style=wx.TAB_TRAVERSAL|wx.FULL_REPAINT_ON_RESIZE)

        self.name_prop.display(panel)
        self.klass_prop.display(panel)
        if getattr(self, '_custom_base_classes', False):
            self.properties['custom_base'].display(panel)

    def __getitem__(self, value):
        return self.access_functions[value]

    def set_name(self, value):
        value = "%s" % value
        if not config.preferences.allow_duplicate_names and \
               (self.widget and common.app_tree.has_name(value, self.node)):
            wx.CallAfter(
                wx.MessageBox, _('Name "%s" is already in use.\n'
                'Please enter a different one.') % value, _("Error"),
                wx.OK|wx.ICON_ERROR)
            self.name_prop.set_value(self.name)
            return
        if not re.match(self.set_name_pattern, value):
            wx.CallAfter(
                wx.MessageBox, _(
                'The new name "%s" contains invalid characters. The\n'
                'old name "%s" will be retain.\n'
                '\n'
                'Valid characters are alphanumeric characters, minus sign\n'
                'and the underscore. Names start always with an alphabetic\n'
                'character or an underscore.\n'
                '\n'
                'Please enter a different name.') % (value, self.name),
                _("Error"),
                wx.OK|wx.ICON_ERROR)
            self.name_prop.set_value(self.name)
        else:
            oldname = self.name
            self.name = value
            if self._rmenu:
                self._rmenu.SetTitle(self.name)
            try:
                common.app_tree.refresh_name(self.node, oldname)
            except AttributeError: pass
            self.property_window.SetTitle(_('Properties - <%s>') % self.name)
    set_name_pattern = re.compile(r'^[a-zA-Z_]+[\w-]*(\[\w*\])*$')

    def set_klass(self, value):
        value = "%s" % value
        if not re.match(self.set_klass_pattern, value):
            self.klass_prop.set_value(self.klass)
        else:
            self.klass = value
            try:
                common.app_tree.refresh_name(self.node)
            except AttributeError:
                pass
    set_klass_pattern = re.compile(r'^[a-zA-Z_]+[\w:.0-9-]*$')

    def popup_menu(self, event):
        if not self.widget:
            return
        if not self._rmenu:
            self._create_popup_menu()
        self.setup_preview_menu()
        # convert relative event position to relative widget position
        event_widget = event.GetEventObject()
        event_pos = event.GetPosition()
        screen_pos = event_widget.ClientToScreen(event_pos)
        client_pos = self.widget.ScreenToClient(screen_pos)
        self.widget.PopupMenu(self._rmenu, client_pos)

    def _create_popup_menu(self):
        COPY_ID, REMOVE_ID, CUT_ID = [wx.NewId() for i in range(3)]
        self._rmenu = misc.wxGladePopupMenu(self.name)
        misc.append_item(self._rmenu, REMOVE_ID, _('Remove\tDel'),
                         wx.ART_DELETE)
        misc.append_item(self._rmenu, COPY_ID, _('Copy\tCtrl+C'),
                         wx.ART_COPY)
        misc.append_item(self._rmenu, CUT_ID, _('Cut\tCtrl+X'),
                         wx.ART_CUT)
        self._rmenu.AppendSeparator()
        PREVIEW_ID = wx.NewId()
        misc.append_item(self._rmenu, PREVIEW_ID, _('Preview'))

        def bind(method):
            return lambda e: wx.CallAfter(method)

        wx.EVT_MENU(self.widget, REMOVE_ID, bind(self.remove))
        wx.EVT_MENU(self.widget, COPY_ID, bind(self.clipboard_copy))
        wx.EVT_MENU(self.widget, CUT_ID, bind(self.clipboard_cut))
        wx.EVT_MENU(self.widget, PREVIEW_ID, bind(self.preview_parent))

    def remove(self, *args):
        self._dont_destroy = False  # always destroy when explicitly asked
        common.app_tree.remove(self.node)

    def setup_preview_menu(self):
        p = misc.get_toplevel_widget(self)
        if p is not None:
            item = list(self._rmenu.GetMenuItems())[-1]
            if p.preview_is_visible():
                item.SetText(_('Close preview') + ' (%s)\tCtrl+P' % p.name)
            else:
                item.SetText(_('Preview') + ' (%s)\tCtrl+P' % p.name)

    def preview_parent(self):
        widget = misc.get_toplevel_widget(self)
        if widget is not None:
            widget.preview(None)

    def show_properties(self, *args):
        """\
        Updates property_window to display the properties of self
        """
        if self.klass == 'wxPanel':  # am I a wxPanel under a wxNotebook?
            if self.parent and self.parent.klass == 'wxNotebook':
                nb = self.parent
                if nb.widget:
                    i = 0
                    for tn, ep in nb.tabs:  # tn=tabname, ep = editpanel
                        try:
                            if ep and self.name == ep.name:
                                # If I am under this tab...
                                nb.widget.SetSelection(i)  # ...Show that tab.
                        except AttributeError:
                            pass
                        i += 1
        if self.parent and self.parent.klass == 'wxPanel':
            # am I a widget under a wxPanel under a wxNotebook?
            if self.parent.parent and self.parent.parent.klass == 'wxNotebook':
                nb = self.parent.parent
                if nb.widget:
                    i = 0
                    for tn, ep in nb.tabs:  # tn=tabname, ep = editpanel
                        try:
                            if ep and self.parent.name == ep.name:
                                nb.widget.SetSelection(i)
                        except AttributeError:
                            pass
                        i += 1

        if not self.is_visible():
            return  # don't do anything if self is hidden
        # create the notebook the first time the function is called: this
        # allows us to create only the notebooks we really need
        if self.notebook is None:
            self.create_properties()
            self.create_events_property()
            self.create_extracode_property()
        sizer_tmp = self.property_window.GetSizer()
        child = sizer_tmp.GetChildren()[0]
        w = child.GetWindow()
        if w is self.notebook:
            return

        try:
            index = -1
            title = w.GetPageText(w.GetSelection())
            for i in range(self.notebook.GetPageCount()):
                if self.notebook.GetPageText(i) == title:
                    index = i
                    break
        except AttributeError:
            #self._logger.exception(_('Internal Error:'))
            index = -1
        w.Hide()
        if 0 <= index < self.notebook.GetPageCount():
            self.notebook.SetSelection(index)
        self.notebook.Reparent(self.property_window)
        compat.SizerItem_SetWindow(child, self.notebook)

        self.notebook.Show()
        self.notebook.SetSize(self.property_window.GetClientSize())

        self.property_window.Layout()
        self.property_window.SetTitle(_('Properties - <%s>') % self.name)
        try:
            common.app_tree.select_item(self.node)
        except AttributeError:
            pass
        self.widget.SetFocus()

    def on_set_focus(self, event):
        """\
        Event handler called when a window receives the focus: this in fact is
        connected to a EVT_LEFT_DOWN and not to an EVT_FOCUS, but the effect
        is the same
        """
        self.show_properties()
        misc.focused_widget = self
        #if wxPlatform != '__WXMSW__': event.Skip()

    def get_property_handler(self, prop_name):
        """\
        Returns a custom handler function for the property 'prop_name', used
        when loading this object from a XML file. handler must provide
        three methods: 'start_elem', 'end_elem' and 'char_data'
        """
        return EventsMixin.get_property_handler(self, prop_name)

    def clipboard_copy(self, event=None):
        """\
        Store a widget copy into the clipboard

        @see: L{clipboard.copy()}
        """
        clipboard.copy(self)

    def clipboard_cut(self, event=None):
        """\
        Store a copy of self into the clipboard and delete the widget.

        @see: L{clipboard.cut()}
        """
        clipboard.cut(self)

    def is_visible(self):
        if not self.widget: return False
        if not self.widget.IsShown(): return False
        if self.widget.IsTopLevel():
            return self.widget.IsShown()
        parent = self.parent
        if parent: return parent.is_visible()
        return self.widget.IsShown()

    def update_view(self, selected):
        """\
        Updates the widget's view to reflect its state, i.e. shows which
        widget is currently selected; the default implementation does nothing.
        """
        pass

    def post_load(self):
        """\
        Called after the loading of an app from a XML file, before showing
        the hierarchy of widget for the first time. The default implementation
        does nothing.
        """
        pass

    def create_extracode_property(self):
        try:
            self.properties['extracode']._show(self.notebook)
            self.properties['extraproperties']._show(self.notebook)
        except KeyError:
            pass

    def set_property_blocking(self, key, item):
        if self.property_blocking.has_key(key):
            self.property_blocking[key].append(item)
        else:
            self.property_blocking[key] = [item]

    def get_property_blocking(self, key):
        if self.property_blocking.has_key(key):
            return self.property_blocking[key]
        return None

    def remove_property_blocking(self, key, item):
        if self.property_blocking.has_key(key):
            for i in range(self.property_blocking[key].count(item)):
                self.property_blocking[key].remove(item)
            if not len(self.property_blocking[key]):
                del self.property_blocking[key]

# end of class EditBase


class WindowBase(EditBase):
    """\
    Extends EditBase with the addition of the common properties available to
    almost every window: size, background and foreground colours, and font
    """

    def __init__(self, name, klass, parent, id, property_window, show=True):
        EditBase.__init__(self, name, klass, parent, id, property_window,
                          show=False)
        # 'property' id (editable by the user)
        self.window_id = "wxID_ANY"

        def set_id(value):
            self.window_id = value
        self.access_functions['id'] = (lambda s=self: s.window_id, set_id)
        self.size = '-1, -1'
        self.access_functions['size'] = (self.get_size, self.set_size)
        self.background = ''
        self.access_functions['background'] = (self.get_background,
                                               self.set_background)
        self.foreground = ''
        self.access_functions['foreground'] = (self.get_foreground,
                                               self.set_foreground)
        # this is True if the user has selected a custom font
        self._font_changed = False
        self.font = self._build_from_font(wx.SystemSettings_GetFont(
            wx.SYS_DEFAULT_GUI_FONT))
        self.font[1] = 'default'

        self.access_functions['font'] = (self.get_font, self.set_font)

        self.tooltip = ''
        self.access_functions['tooltip'] = (self.get_tooltip,
                                            self.set_tooltip)

        self._original = {'background': None, 'foreground': None,
                          'font': None}

        prop = self.properties
        prop['id'] = TextProperty(self, 'id', None, can_disable=True)
        prop['id'].tooltip = _("""\
The "Id" property could be 
1) a constant numeric value
2) a predefined identifier e.g. wxID_ANY
3) a predefined variable like a class member e.g. self.myButtonID
4) a variable assignment e.g. self.myButtonID=?

The pattern of a variable assignment is always "variable=value". The \
value could be again a numeric value, a predefined identifier, \
another predefined variable or "?" a shortcut for "wxNewId()". \
""")
        prop['size'] = TextProperty(self, 'size', None, can_disable=True, label=_("size"))
        prop['background'] = ColorDialogProperty(self, "background", None, label=_("background"))
        prop['foreground'] = ColorDialogProperty(self, "foreground", None, label=_("foreground"))
        prop['font'] = FontDialogProperty(self, "font", None, label=_("font"))

        prop['tooltip'] = TextProperty(self, 'tooltip', None, can_disable=True, label=_('tooltip'))

        self.disabled_p = False
        self.access_functions['disabled'] = (self.get_disabled,
                                             self.set_disabled)
        prop['disabled'] = CheckBoxProperty(self, 'disabled', None, _('disabled'))

        self.focused_p = False
        self.access_functions['focused'] = (self.get_focused, self.set_focused)
        prop['focused'] = CheckBoxProperty(self, 'focused', None, _('focused'))

        self.hidden_p = False
        self.access_functions['hidden'] = (self.get_hidden, self.set_hidden)
        prop['hidden'] = CheckBoxProperty(self, 'hidden', None, _('hidden'))

    def finish_widget_creation(self, *args, **kwds):
        self._original['background'] = self.widget.GetBackgroundColour()
        self._original['foreground'] = self.widget.GetForegroundColour()
        fnt = self.widget.GetFont()
        if not fnt.Ok():
            fnt = wx.SystemSettings_GetFont(wx.SYS_DEFAULT_GUI_FONT)
        self._original['font'] = fnt

        prop = self.properties
        size = prop['size'].get_value()
        if size:
            #self.widget.SetSize([int(s) for s in size.split(',')])
            self.set_size(size)
        else:
            prop['size'].set_value('%s, %s' % tuple(self.widget.GetSize()))
        if prop['background'].is_active():
            self.set_background(prop['background'].get_value())
        else:
            color = misc.color_to_string(self.widget.GetBackgroundColour())
            self.background = color
            prop['background'].set_value(color)
        if prop['foreground'].is_active():
            self.set_foreground(prop['foreground'].get_value())
        else:
            color = misc.color_to_string(self.widget.GetForegroundColour())
            self.foreground = color
            prop['foreground'].set_value(color)
        if prop['font'].is_active():
            self.set_font(prop['font'].get_value())
        EditBase.finish_widget_creation(self)
        wx.EVT_SIZE(self.widget, self.on_size)
        # after setting various Properties, we must Refresh widget in order to
        # see changes
        self.widget.Refresh()

        def on_key_down(event):
            evt_flags = 0
            if event.ControlDown(): evt_flags = wx.ACCEL_CTRL
            evt_key = event.GetKeyCode()
            done = False
            for flags, key, function in misc.accel_table:
                if evt_flags == flags and evt_key == key:
                    wx.CallAfter(function)
                    done = True
                    break
            if not done:
                event.Skip()
        wx.EVT_KEY_DOWN(self.widget, on_key_down)

    def create_properties(self):
        EditBase.create_properties(self)

        panel = self._common_panel

        prop = self.properties
        prop['id'].display(panel)
        prop['size'].display(panel)

        prop['background'].display(panel)
        prop['foreground'].display(panel)
        try:
            prop['font'].display(panel)
        except KeyError:
            pass
        prop['tooltip'].display(panel)
        prop['disabled'].display(panel)
        prop['focused'].display(panel)
        prop['hidden'].display(panel)

        sizer_tmp = wx.BoxSizer(wx.VERTICAL)
        sizer_tmp.Add(self.name_prop.panel, 0, wx.EXPAND)
        sizer_tmp.Add(self.klass_prop.panel, 0, wx.EXPAND)
        if getattr(self, '_custom_base_classes', False):
            sizer_tmp.Add(prop['custom_base'].panel, 0, wx.EXPAND)
        sizer_tmp.Add(prop['id'].panel, 0, wx.EXPAND)
        sizer_tmp.Add(prop['size'].panel, 0, wx.EXPAND)
        sizer_tmp.Add(prop['background'].panel, 0, wx.EXPAND)
        sizer_tmp.Add(prop['foreground'].panel, 0, wx.EXPAND)
        try:
            sizer_tmp.Add(prop['font'].panel, 0, wx.EXPAND)
        except KeyError:
            pass
        sizer_tmp.Add(prop['tooltip'].panel, 0, wx.EXPAND)
        sizer_tmp.Add(prop['disabled'].panel, 0, wx.EXPAND)
        sizer_tmp.Add(prop['focused'].panel, 0, wx.EXPAND)
        sizer_tmp.Add(prop['hidden'].panel, 0, wx.EXPAND)

#        # add a note if the widget don't support all supported wx versions
#        all_versions = set(common.app_tree.app.all_supported_versions)
#        supported = set(
#            [misc.format_supported_by(version) \
#             for version in getattr(self, 'supported_by', [])]
#            )
#        not_supported = all_versions.difference(supported)
#        # hint text only if we have support information (supported_by) and
#        # not all versions are supported (not_supported)
#        if supported and not_supported:
#            text_supported = ', '.join(supported)
#            text_not_supported = ', '.join(not_supported)
#            note = wx.StaticText(
#                panel,
#                -1,
#                _("This widget is only supported for wx %s") % \
#                text_supported
#                )
#            note.SetToolTip(wx.ToolTip(
#                _("This widgets is supported for wx versions %(supported)s "
#                  "and not at %(not_supported)s." ) % {
#                    'supported': text_supported,
#                    'not_supported': text_not_supported,
#                    }
#                ))
#            sizer_tmp.Add(note, 0, wx.ALL | wx.EXPAND, 3)

        panel.SetAutoLayout(1)
        compat.SizerItem_SetSizer(panel, sizer_tmp)
        sizer_tmp.Layout()
        sizer_tmp.Fit(panel)

        w, h = panel.GetClientSize()
        self.notebook.AddPage(panel, _("Common"))
        self.property_window.Layout()
        panel.SetScrollbars(1, 5, 1, int(math.ceil(h/5.0)))

    def on_size(self, event):
        """\
        Update the value of the 'size' property
        """
        try:
            prop_size = self.properties['size']

            # try to preserve the user's choice
            value_prop = prop_size.get_value().strip()
            if prop_size.is_active():
                try:
                    use_dialog_units = value_prop and value_prop[-1] == 'd'

                except IndexError:
                    use_dialog_units = False

                if use_dialog_units:
                    value_prop = value_prop[:-1]

                weidth_prop, height_prop = [int(t) for t in value_prop.split(',')]
            else:
                use_dialog_units = config.preferences.use_dialog_units
                weidth_prop, height_prop = 0, 0

            if use_dialog_units:
                weidth_widget, height_widget = \
                    self.widget.ConvertPixelSizeToDialog(
                        self.widget.GetSize())
            else:
                weidth_widget, height_widget = self.widget.GetSize()

            if weidth_prop == -1:
                weidth_widget = -1
            if height_prop == -1:
                height_widget = -1

            size_widget = "%s, %s" % (weidth_widget, height_widget)
            if use_dialog_units:
                size_widget += "d"

            # There are an infinite loop of wxSizeEvents. All events have
            # the same id. It looks currently like a bug in the underlaying
            # wx libraries especially in the GTK part. The bug doesn't occur
            # on Windows.
            #
            # The issue probably occur only within EditGrid.
            #
            # This is workaround prevents the propagation if the size hasn't
            # changed.
            #
            # Related SF bug report: #170
            if self.size == size_widget:
                return

            self.size = size_widget
            prop_size.set_value(size_widget)
        except KeyError:
            logging.exception(_('Internal Error'))

        event.Skip()

    def get_tooltip(self):
        return self.tooltip

    def set_tooltip(self, value):
        self.tooltip = misc.wxstr(value)

    def get_background(self):
        return self.background

    def get_foreground(self):
        return self.foreground

    def set_background(self, value):
        oldval = self.background
        self.background = value
        if not self.widget: return
        value = value.strip()
        if value in ColorDialogProperty.str_to_colors:
            self.widget.SetBackgroundColour(wx.SystemSettings_GetColour(
                ColorDialogProperty.str_to_colors[value]))
        else:
            try:
                color = misc.string_to_color(value)
                self.widget.SetBackgroundColour(color)
            except:
                self.background = oldval
                self.properties['background'].set_value(self.get_background())
                return
        self.widget.Refresh()

    def set_foreground(self, value):
        oldval = self.foreground
        self.foreground = value
        if not self.widget: return
        value = value.strip()
        if value in ColorDialogProperty.str_to_colors:
            self.widget.SetForegroundColour(wx.SystemSettings_GetColour(
                ColorDialogProperty.str_to_colors[value]))
        else:
            try:
                color = misc.string_to_color(value)
                self.widget.SetForegroundColour(color)
            except:
                self.foreground = oldval
                self.properties['foreground'].set_value(self.get_foreground())
                return
        self.foreground = value
        self.widget.Refresh()

    def get_font(self):
        return str(self.font)

    def _build_from_font(self, font):
        families = FontDialogProperty.font_families_from
        styles = FontDialogProperty.font_styles_from
        weights = FontDialogProperty.font_weights_from
        return [ str(font.GetPointSize()),
                 families.get(font.GetFamily(), 'default'),
                 styles.get(font.GetStyle(), 'normal'),
                 weights.get(font.GetWeight(), 'normal'),
                 str(int(font.GetUnderlined())), font.GetFaceName() ]

    def set_font(self, value):
        #if not self.widget: return
        families = FontDialogProperty.font_families_to
        styles = FontDialogProperty.font_styles_to
        weights = FontDialogProperty.font_weights_to
        try:
            value = eval(value)
            f = wx.Font(int(value[0]), families[value[1]], styles[value[2]],
                       weights[value[3]], int(value[4]), value[5])
        except:
            #self._logger.exception(_('Internal Error'))
            self.properties['font'].set_value(self.get_font())
        else:
            self.font = value
            if self.widget:
                old_size = self.widget.GetSize()
                self.widget.SetFont(f)
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
            self.properties['size'].set_value(self.size)
        else:
            if use_dialog_units and value[-1] != 'd': value += 'd'
            self.size = value
            if self.widget:
                if use_dialog_units: size = wx.DLG_SZE(self.widget, size)
                self.widget.SetMinSize(size)
                self.widget.SetSize(size)
                try:
                    #self.sizer.set_item(self.pos, size=self.widget.GetSize())
                    self.sizer.set_item(self.pos, size=size)
                except AttributeError:
                    pass

    def get_size(self):
        return self.size

    def get_property_handler(self, name):
        if name == 'font':
            return FontHandler(self)
        elif name == 'extraproperties':
            import code_property
            return code_property.ExtraPropertiesPropertyHandler(self)
        return EditBase.get_property_handler(self, name)

    def get_disabled(self):
        return self.disabled_p

    def set_disabled(self, value):
        try: self.disabled_p = bool(int(value))
        except ValueError: pass

    def get_focused(self):
        return self.focused_p

    def set_focused(self, value):
        try: self.focused_p = bool(int(value))
        except ValueError: pass

    def get_hidden(self):
        return self.hidden_p

    def set_hidden(self, value):
        try: self.hidden_p = bool(int(value))
        except ValueError: pass

# end of class WindowBase


class ManagedBase(WindowBase):
    """\
    Base class for every managed window used by the builder: extends WindowBase
    with the addition of properties relative to the layout of the window:
    option, flag, and border

    @ivar sel_marker: Selection markers
    @type sel_marker: SelectionMarker

    @ivar self.sizer_properties: Properties relative to the sizer which
                                 controls this window
    @type self.sizer_properties: dict
    """

    def __init__(self, name, klass, parent, id, sizer, pos, property_window,
                 show=True):
        WindowBase.__init__(self, name, klass, parent, id, property_window,
                            show=show)
        # if True, the user is able to control the layout of the widget
        # inside the sizer (proportion, borders, alignment...)
        self._has_layout = not sizer.is_virtual()

        # selection markers
        self.sel_marker = None

        # dictionary of properties relative to the sizer which
        # controls this window
        self.sizer_properties = {}

        # attributes to keep the values of the sizer_properties
        self.option = 0
        self.flag = 0
        self.border = 0

        border_styles = OrderedDict()
        border_styles[_('Border')] = ['wxALL', 'wxLEFT', 'wxRIGHT', 'wxTOP',
                                      'wxBOTTOM']
        border_styles[_('Alignment')] = [
            'wxEXPAND', 'wxALIGN_RIGHT', 'wxALIGN_BOTTOM', 'wxALIGN_CENTER',
            'wxALIGN_CENTER_HORIZONTAL', 'wxALIGN_CENTER_VERTICAL',
            'wxSHAPED', 'wxADJUST_MINSIZE', 'wxFIXED_MINSIZE']

        # use 'wxDialog' as a functional dummy
        self.esm_border = EditStylesMixin('wxDialog', border_styles)
        self.esm_border.update_widget_style = True
        self.esm_border._set_widget_style = self.set_int_flag

        self.sizer = sizer
        self.pos = pos
        self.access_functions['option'] = (self.get_option, self.set_option)
        self.access_functions['flag'] = (self.esm_border.get_style,
                                         self.esm_border.set_style)
        self.access_functions['border'] = (self.get_border, self.set_border)
        self.access_functions['pos'] = (self.get_pos, self.set_pos)

        self.flag = wx.ADJUST_MINSIZE
        sizer.add_item(self, pos)

        szprop = self.sizer_properties
        from layout_option_property import LayoutOptionProperty, \
             LayoutPosProperty
        szprop['option'] = LayoutOptionProperty(self, sizer)
        szprop['flag'] = CheckListProperty(self, 'flag', styles=border_styles)
        szprop['border'] = SpinProperty(self, 'border', None, 0, (0, 1000), label=_('border'))
        szprop['pos'] = LayoutPosProperty(self, sizer)

    def finish_widget_creation(self, sel_marker_parent=None):
        if sel_marker_parent is None: sel_marker_parent = self.parent.widget
        self.sel_marker = misc.SelectionMarker(self.widget, sel_marker_parent)
        WindowBase.finish_widget_creation(self)
        wx.EVT_LEFT_DOWN(self.widget, self.on_set_focus)
        wx.EVT_MOVE(self.widget, self.on_move)
        # re-add the item to update it
        self.sizer.add_item(self, self.pos, self.option, self.flag,
                            self.border, self.widget.GetSize())
        # set the value of the properties
        szp = self.sizer_properties
        szp['option'].set_value(self.get_option())
        szp['flag'].set_value(self.esm_border.get_style())
        szp['border'].set_value(self.get_border())
        szp['pos'].set_value(self.pos - 1)

    def create_properties(self):
        WindowBase.create_properties(self)
        if not self._has_layout: return
        panel = wx.ScrolledWindow(
            self.notebook, -1, style=wx.TAB_TRAVERSAL|wx.FULL_REPAINT_ON_RESIZE)

        szprop = self.sizer_properties
        szprop['pos'].display(panel)
        szprop['option'].display(panel)
        szprop['border'].display(panel)
        szprop['flag'].display(panel)

        sizer_tmp = wx.BoxSizer(wx.VERTICAL)
        sizer_tmp.Add(szprop['pos'].panel, 0, wx.EXPAND)
        sizer_tmp.Add(szprop['option'].panel, 0, wx.EXPAND)
        sizer_tmp.Add(szprop['border'].panel, 0, wx.EXPAND)
        sizer_tmp.Add(szprop['flag'].panel, 0, wx.EXPAND, 5)
        panel.SetAutoLayout(True)
        compat.SizerItem_SetSizer(panel, sizer_tmp)
        sizer_tmp.Layout()
        sizer_tmp.Fit(panel)

        w, h = panel.GetClientSize()
        self.notebook.AddPage(panel, _("Layout"))
        panel.SetScrollbars(1, 5, 1, int(math.ceil(h/5.0)))

    def update_view(self, selected):
        if self.sel_marker: self.sel_marker.Show(selected)

    def on_move(self, event):
        self.sel_marker.update()

    def on_size(self, event):
        old = self.size
        WindowBase.on_size(self, event)
        size_prop = self.properties['size']
        if (size_prop.is_active() and (int(self.get_option()) != 0 or
                                self.get_int_flag() & wx.EXPAND)):
            size_prop.set_value(old)
            self.size = old
        self.sel_marker.update()

    def set_option(self, value):
        self.option = value = int(value)
        if not self.widget: return
        try:
            sz = self.properties['size']
            if value or sz.is_active():
                size = sz.get_value().strip()
                if size[-1] == 'd':
                    size = size[:-1]
                    use_dialog_units = True
                else: use_dialog_units = False
                w, h = [ int(v) for v in size.split(',') ]
                if use_dialog_units:
                    w, h = wx.DLG_SZE(self.widget, (w, h))
                if value:
                    w, h = 1, 1
            else:
                w, h = self.widget.GetBestSize()
            self.sizer.set_item(self.pos, option=value, size=(w, h))
        except AttributeError:
            self._logger.exception(_('Internal Error'))

    def set_int_flag(self, flags=None):
        """\
        Set the widget flag

        @param flags: Widget flag
        @type flags: int
        """
        assert isinstance(flags, (types.IntType, types.NoneType))
        if isinstance(flags, types.NoneType):
            flags = self.esm_border.get_int_style()
        self.flag = flags
        if not self.widget:
            return
        try:
            try:
                sp = self.properties['size']
                size = sp.get_value().strip()
                if size[-1] == 'd':
                    size = size[:-1]
                    use_dialog_units = True
                else:
                    use_dialog_units = False
                w, h = [int(v) for v in size.split(',')]
                if use_dialog_units:
                    w, h = wx.DLG_SZE(self.widget, (w, h))
                size = [w, h]
            except ValueError:
                size = None
            if not (flags & wx.EXPAND) and \
               not self.properties['size'].is_active():
                size = list(self.widget.GetBestSize())
            self.sizer.set_item(self.pos, flag=flags, size=size)
        except AttributeError:
            self._logger.exception(_('Internal Error'))

    def set_border(self, value):
        self.border = int(value)
        if not self.widget:
            return
        try:
            sp = self.properties['size']
            size = sp.get_value().strip()
            if size[-1] == 'd':
                size = size[:-1]
                use_dialog_units = True
            else:
                use_dialog_units = False
            w, h = [int(v) for v in size.split(',')]
            if use_dialog_units:
                w, h = wx.DLG_SZE(self.widget, (w, h))
            if w == -1:
                w = self.widget.GetSize()[0]
            if h == -1:
                h = self.widget.GetSize()[1]
            self.sizer.set_item(self.pos, border=int(value), size=(w, h))
        except AttributeError:
            self._logger.exception(_('Internal Error'))

    def get_option(self):
        return self.option

    def get_int_flag(self):
        return self.esm_border.get_int_style()

    def get_border(self):
        return self.border

    def delete(self):
        if self.sel_marker:
            self.sel_marker.Destroy()  # destroy the selection markers
        WindowBase.delete(self)

    def remove(self, *args):
        self.sizer.free_slot(self.pos)
        WindowBase.remove(self)

    def get_pos(self):
        return self.pos - 1

    def set_pos(self, value):
        """\
        setter for the 'pos' property: calls self.sizer.change_item_pos
        """
        self.sizer.change_item_pos(self, min(value + 1,
                                             len(self.sizer.children) - 1))

    def update_pos(self, value):
        """\
        called by self.sizer.change_item_pos to update the item's position
        when another widget is moved
        """
        self.sizer_properties['pos'].set_value(value-1)
        self.pos = value

# end of class ManagedBase


class PreviewMixin(object):
    """\
    Mixin class used to add preview to a widget

    @ivar preview_button: Button to show or close the preview window
    @ivar preview_widget: Widget to be represented

    @ivar _logger: Class specific logging instance
    """

    def __init__(self):
        # initialise instance logger
        self._logger = logging.getLogger(self.__class__.__name__)

        # initialise instance
        self.preview_button = None
        self.preview_widget = None

    def create_properties(self):
        panel = self.notebook.GetPage(0)
        sizer_tmp = panel.GetSizer()
        # add a preview button to the Common panel for top-levels
        self.preview_button = btn = wx.Button(panel, -1, _('Preview'))
        wx.EVT_BUTTON(btn, -1, self.preview)
        sizer_tmp.Add(btn, 0, wx.ALL|wx.EXPAND, 5)
        sizer_tmp.Layout()
        sizer_tmp.Fit(panel)
        w, h = panel.GetClientSize()
        self.property_window.Layout()
        panel.SetScrollbars(1, 5, 1, int(math.ceil(h/5.0)))

    def preview(self, event):
        """\
        Create a preview of the selected widget
        """
        #self._logger.debug('frame class _> %s', self.klass)
        if self.preview_widget is None:
            # The preview widget is None in case of code generation errors
            self.preview_widget = common.app_tree.app.preview(self)
            if self.preview_widget:
                self.preview_button.SetLabel(_('Close Preview'))
        else:
            # Close triggers the EVT_CLOSE that does the real work
            # (see application.py -> preview)
            self.preview_widget.Close()

    def preview_is_visible(self):
        """\
        True if the L{preview_button} is created

        @rtype: bool
        """
        return self.preview_widget is not None

# end of class PreviewMixin


class TopLevelBase(WindowBase, PreviewMixin):
    """\
    Base class for every non-managed window (i.e. Frames and Dialogs).
    """
    _is_toplevel = True
    _custom_base_classes = True

    def __init__(self, name, klass, parent, id, property_window, show=True,
                 has_title=True, title=None):
        WindowBase.__init__(self, name, klass, parent, id, property_window,
                            show=show)
        self.has_title = has_title
        if self.has_title:
            if title is None: title = self.name
            self.title = title
            self.access_functions['title'] = (self.get_title, self.set_title)
            self.properties['title'] = TextProperty(self, 'title', None, label=_("title"))
        self.sizer = None  # sizer that controls the layout of the children
                          # of the window
        PreviewMixin.__init__(self)

    def finish_widget_creation(self, *args, **kwds):
        WindowBase.finish_widget_creation(self)
        self.widget.SetMinSize = self.widget.SetSize
        if self.has_title:
            self.widget.SetTitle(misc.design_title(
                self.properties['title'].get_value()))
        elif hasattr(self.widget, 'SetTitle'):
            self.widget.SetTitle(misc.design_title(self.name))
        wx.EVT_LEFT_DOWN(self.widget, self.drop_sizer)
        wx.EVT_ENTER_WINDOW(self.widget, self.on_enter)
        wx.EVT_CLOSE(self.widget, self.hide_widget)
        if wx.Platform == '__WXMSW__':
            # MSW isn't smart enough to avoid overlapping windows, so
            # at least move it away from the 3 wxGlade frames
            self.widget.Center()
        # ALB 2004-10-15
        self.widget.SetAcceleratorTable(common.palette.accel_table)

    def show_widget(self, yes):
        WindowBase.show_widget(self, yes)
        if yes and wx.Platform == '__WXMSW__':
            # more than ugly, but effective hack to properly layout the window
            # on Win32
            if self.properties['size'].is_active():
                w, h = self.widget.GetSize()
                self.widget.SetSize((-1, h+1))
                self.widget.SetSize((-1, h))
            elif self.sizer:
                self.sizer.fit_parent()

    def _create_popup_menu(self):
        REMOVE_ID, HIDE_ID = [wx.NewId() for i in range(2)]
        self._rmenu = misc.wxGladePopupMenu(self.name)
        misc.append_item(self._rmenu, REMOVE_ID, _('Remove\tDel'),
                         wx.ART_DELETE)
        misc.append_item(self._rmenu, HIDE_ID, _('Hide'))

        def bind(method):
            return lambda e: wx.CallAfter(method)

        wx.EVT_MENU(self.widget, REMOVE_ID, bind(self.remove))
        wx.EVT_MENU(self.widget, HIDE_ID, bind(self.hide_widget))
        # paste
        PASTE_ID = wx.NewId()
        misc.append_item(self._rmenu, PASTE_ID, _('Paste\tCtrl+V'),
                         wx.ART_PASTE)
        wx.EVT_MENU(self.widget, PASTE_ID, bind(self.clipboard_paste))
        PREVIEW_ID = wx.NewId()
        self._rmenu.AppendSeparator()
        misc.append_item(self._rmenu, PREVIEW_ID, _('Preview'))
        wx.EVT_MENU(self.widget, PREVIEW_ID, bind(self.preview_parent))

    def clipboard_paste(self, event=None):
        """\
        Insert a widget from the clipboard to the current destination.

        @see: L{clipboard.paste()}
        """
        if self.sizer is not None:
            self._logger.warning(
                _('WARNING: Sizer already set for this window')
                )
            return
        import xml_parse
        size = self.widget.GetSize()
        try:
            if clipboard.paste(self, None, 0):
                common.app_tree.app.saved = False
                self.widget.SetSize(size)
        except xml_parse.XmlParsingError:
            self._logger.warning(
                _('WARNING: Only sizers can be pasted here')
                )

    def create_properties(self):
        WindowBase.create_properties(self)
        # don't display the title ourselves anymore, now it's a
        # duty of the subclass!
##         if self.has_title:
##             panel = self.notebook.GetPage(0)
##             sizer_tmp = panel.GetSizer()
##             self.properties['title'].display(panel)
##             sizer_tmp.Add(self.properties['title'].panel, 0, wxEXPAND)
        PreviewMixin.create_properties(self)

    def get_title(self):
        return self.title

    def set_title(self, value):
        self.title = misc.wxstr(value)
        if self.widget:
            self.widget.SetTitle(misc.design_title(value))

    def set_sizer(self, sizer):
        self.sizer = sizer
        if self.sizer and self.sizer.widget and self.widget:
            self.widget.SetAutoLayout(True)
            compat.SizerItem_SetSizer(self.widget, self.sizer.widget)
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
        self.widget.Hide()
        common.app_tree.expand(self.node, False)
        common.app_tree.select_item(self.node.parent)
        common.app_tree.app.show_properties()

    def on_size(self, event):
        WindowBase.on_size(self, event)
        if self.sizer and self.widget:
            self.sizer.refresh()

    def set_name(self, name):
        oldname = self.name

        # check and set name
        WindowBase.set_name(self, name)

        # update top window name
        if not misc.streq(oldname, self.name):
            common.app_tree.app.update_top_window_name(oldname, self.name)

    def delete(self, *args):
        if self.preview_widget is not None:
            self.preview_widget.Destroy()
            self.preview_widget = None
        WindowBase.delete(self)

# end of class TopLevelBase


class EditStylesMixin(object):
    """\
    Mixin to handle styles within widget dialogs

    This class needs the wxWidget class to get the proper widget writer.
    Mostly the wxWidget class is stored in self.base. If not you've to set
    manually using constructors 'klass' parameter. The 'klass' parameter
    will preferred used.

    @ivar style_set: Set of selected styles
    @type style_set: set[str]

    @ivar style_names: List of style names
    @type style_names: list[str]

    @ivar widget_writer: Widget code writer
    @type widget_writer: wcodegen.BaseWidgetWriter

    """
    codegen = None
    """\
    Code generator class

    @see: L{codegen.BaseLangCodeWriter}
    """

    update_widget_style = True
    """\
    Flag to update the widget style if a style is set using L{set_style()}

    @type: bool
    """

    def __init__(self, klass='', styles=[]):
        """\
        Initialise instance

        @param klass: Name of the wxWidget klass
        @type klass:  str

        @param styles: Supported styles, for more details
                       see L{widget_properties.CheckListProperty}
        @type styles: list[str] | OrderedDict
        """
        assert klass or hasattr(self, 'base')
        self.style_names = []
        self.style_set = set()

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
            if isinstance(styles, types.DictionaryType):
                for box_label in styles.keys():
                    self.style_names.extend(styles[box_label])
            else:
                self.style_names = styles
        else:
            self.style_names = self.widget_writer.style_list

    def get_style(self):
        """\
        Convert the current style in a list of boolean values. Each value
        indicated if the style  at the same position in style_pos is set.

        Example::
            >>> pprint.pprint(self.style_names)
            ['wxHL_ALIGN_LEFT',
             'wxHL_ALIGN_RIGHT',
             'wxHL_ALIGN_CENTRE',
             'wxHL_CONTEXTMENU',
             'wxHL_DEFAULT_STYLE']
            >>> self.style_set
            set(['wxHL_ALIGN_LEFT'])
            >>> self.get_style()
            [True, False, False, False, False]

        @see: L{style_set}

        @return: List of style flags
        @rtype: list[bool]
        """
        assert self.style_names

        style_list = [False] * len(self.style_names)

        if not self.style_set:
            return style_list

        for pos in xrange(len(self.style_names)):
            name = self.style_names[pos]
            if name in self.style_set:
                style_list[pos] = True

        return style_list

    def get_string_style(self):
        """\
        Return the selected styles joined with '|'.

        @rtype: str
        """
        styles = list(self.style_set)
        styles.sort()
        return '|'.join(styles)

    def get_int_style(self):
        """\
        Convert styles in style_set into integer value

        @return: Integer representation of the selected styles
        @rtype: int

        @see: L{wcodegen.BaseLanguageMixin.cn_f()}
        """
        new_style = 0

        if not self.style_set:
            return new_style

        styles = list(self.style_set)
        styles.sort()
        for style_name in styles:
            try:
                style_name = self.widget_writer.cn_f(style_name)

                # cn_f() returns an empty string if the given styles are not
                # supported
                if not style_name:
                    continue

                style_value = self.wxname2attr(style_name)
                if not isinstance(style_value, types.IntType):
                    self._logger.warning(
                        _('''Can't convert style "%s" to an integer. Got
                            "%s" instead.'''), style_name, style_value
                    )
                    continue
                new_style |= style_value
            except (AttributeError, NameError):
                pass
        return new_style

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

    def set_style(self, value):
        """\
        Set styles. The style value could be a string with styles
        concatenated with '|' or a list of boolean flags to mark the styles
        via the postion as set.

        Example 1::
            >>> self.set_style([True, False, False, False, False])
            >>> self.style_set
            set(['wxHL_ALIGN_LEFT'])

        Example 2::
            >>> self.set_style('wxHL_ALIGN_LEFT')
            >>> self.style_set
            set(['wxHL_ALIGN_LEFT'])

        @param value: Styles to set
        @type value:  str | list[bool] | None

        @see: L{style_set}
        @see: L{style_names}
        @see: L{update_widget_style}
        @see: L{_set_widget_style}
        """
        assert isinstance(value, (types.ListType, types.StringType,
                                  types.UnicodeType, types.NoneType))

        if isinstance(value, types.NoneType):
            new_styles = set()
        elif isinstance(value, types.StringTypes):
            if not value or value == '0':
                new_styles = set()
            else:
                new_styles = set(value.split('|'))
        else:
            new_styles = set()
            for pos in range(len(self.style_names)):
                if value[pos]:
                    new_styles.add(self.style_names[pos])

        self.style_set = self.widget_writer.combine_styles(new_styles)
        self._set_widget_style()

    @decorators.memoize
    def wxname2attr(self, name):
        """\
        Return the attribute specified by the name. Only wx attributes are
        supported.

        Example::
            >>> self.wxname2attr('wx.version')
            <function version at 0x2cc6398>
            >>> self.wxname2attr('wx.VERSION')
            (2, 8, 12, 1, '')

        @param name: Attribute name
        @type name:  String

        @note: Exceptions especially NameError and AttributeError aren't
        caught.
        """
        assert name.startswith('wx')

        cn = self.codegen.without_package(self.codegen.cn(name))
        attr = getattr(wx, cn)
        return attr

# end of class EditStylesMixin
