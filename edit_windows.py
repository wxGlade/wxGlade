# edit_windows.py: base classes for windows used by wxGlade
# 
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: GPL (see license.txt)

from wxPython.wx import *
from widget_properties import *
from tree import Tree, WidgetTree
import math, misc, common, sys


class EditBase:
    """\
    Base class of every window available in the builder.
    """
    def __init__(self, name, klass, parent, id, property_window, show=True,
                 custom_class=True):
        # property_window: widget inside which Properties of this object
        #                  are displayed
        # name: name of the object
        # klass: name of the object's class
        # custom_class: if true, the user can chage the value of the 'class'
        #               property
        
        # dictionary of properties relative to this object; the properties that
        # control the layout (i.e. the behaviour when inside a sizer) are not
        # contained here, but in a separate list (see ManagedBase)
        # the keys of the dict are the names of the properties
        self.properties = {}
        self.parent = parent
        # id used for internal purpose events
        self.id = id
        self.name = name
        self.klass = klass
        self.base = klass
        self.custom_class = custom_class

        self.access_functions = {
            'name' : (lambda : self.name, self.set_name),
            'class' : (lambda : self.klass, self.set_klass)
            }

        # these two properties are special and are not listed in
        # 'self.properties'
        self.name_prop = TextProperty(self, 'name', None)
        self.klass_prop = TextProperty(self, 'class', None,
                                       readonly=not custom_class)
        self.notebook = None
        self.property_window = property_window

        # popup menu
        self._rmenu = None

        # this is the reference to the actual wxWindow widget; it is created
        # only if needed, i.e. when it should become visible
        self.widget = None

        if show:
            self.show_widget(True)
            #self.SetFocus()
            property_window.SetSize((250, 340))
            property_window.Show(True)

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

    def finish_widget_creation(self):
        """\
        Creates the popup menu and connects some event handlers to self.widgets
        """
        COPY_ID, REMOVE_ID, CUT_ID = [ wxNewId() for i in range(3) ]
        self._rmenu = misc.wxGladePopupMenu(self.name)
        self._rmenu.Append(REMOVE_ID, 'Remove')
        self._rmenu.Append(COPY_ID, 'Copy')
        self._rmenu.Append(CUT_ID, 'Cut')
        EVT_RIGHT_DOWN(self.widget, self.popup_menu)
        EVT_MENU(self.widget, REMOVE_ID, self.remove)
        EVT_MENU(self.widget, COPY_ID, self.clipboard_copy)
        EVT_MENU(self.widget, CUT_ID, self.clipboard_cut)

    def delete(self):
        """\
        Destructor. Deallocates the popup menu, the notebook and all the
        properties. Why we need explicit deallocation? Well, basically because
        otherwise we get a lot of memory leaks... :)
        """
        # first, destroy the popup menu...
        if self._rmenu: self._rmenu.Destroy()
        # ...then, destroy the property notebook...
        if self.notebook:
            for p in self.properties.itervalues():
                if p.panel: p.panel.Destroy()
            if self.name_prop.panel: self.name_prop.panel.Destroy()
            if self.klass_prop.panel: self.klass_prop.panel.Destroy()
            if hasattr(self, 'sizer_properties'):
                for p in self.sizer_properties.itervalues():
                    if p.panel: p.panel.Destroy()
            self.notebook.Destroy()
            self.notebook.sizer.Destroy()
        # ...finally, destroy our widget
        if self.widget: self.widget.Destroy()
            
    def create_properties(self):
        """\
        Creates the notebook with the properties of self
        """
        self.notebook = wxNotebook(self.property_window, -1)
        nb_sizer = wxNotebookSizer(self.notebook)
        self.notebook.SetAutoLayout(True)
        self.notebook.sizer = nb_sizer
        self.notebook.Hide()

        self._common_panel = panel = wxScrolledWindow(self.notebook, -1)

        self.name_prop.display(panel)
        self.klass_prop.display(panel)

    def __getitem__(self, value):
        return self.access_functions[value]

    def set_name(self, value):
        self.name = str(value)
        self._rmenu.SetTitle(self.name)
        try: common.app_tree.set_name(self.node, self.name)
        except AttributeError: pass

    def set_klass(self, value):
        self.klass = str(value)

    def popup_menu(self, event):
        if self.widget: self.widget.PopupMenu(self._rmenu, event.GetPosition())

    def remove(self, *args):
        common.app_tree.remove(self.node)

    def show_properties(self, *args):
        """\
        Updates property_window to display the properties of self
        """
        if not self.is_visible(): return # don't do anything if self is hidden
        # create the notebook the first time the function is called: this
        # allows us to create only the notebooks we really need
        if self.notebook is None: self.create_properties()
        sizer_tmp = self.property_window.GetSizer()
        sizer_tmp = wxPyTypeCast(sizer_tmp, "wxBoxSizer")
        child = wxPyTypeCast(sizer_tmp.GetChildren()[0], "wxSizerItem")
        w = wxPyTypeCast(child.GetWindow(), "wxWindow")
        if w is self.notebook: return
        w.Hide()
        child.SetWindow(self.notebook)
        self.property_window.Layout()
        self.property_window.SetTitle('Properties - <%s>' % self.name)
        try: common.app_tree.select_item(self.node)
        except AttributeError: pass
        self.notebook.Show()
        
    def on_set_focus(self, event):
        """\
        Event handler called when a window receives the focus: this in fact is
        connected to a EVT_LEFT_DOWN and not to an EVT_FOCUS, but the effect
        is the same
        """
        self.show_properties()
        if wxPlatform != '__WXMSW__': event.Skip()

    def get_property_handler(self, prop_name):
        """\
        returns a custom handler function for the property 'prop_name', used
        when loading this object from an xml file. handler must provide
        three methods: 'start_elem', 'end_elem' and 'char_data'
        """
        return None

    def clipboard_copy(self, event):
        """\
        returns a copy of self to be inserted in the clipboard
        """
        import clipboard
        clipboard.copy(self)

    def clipboard_cut(self, event):
        import clipboard
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
        updates the widget's view to reflect its state, i.e. shows which widget
        is currently selected; the default implementation does nothing
        """
        pass

# end of class EditBase


class WindowBase(EditBase):
    """\
    Extends EditBase with the addition of the common properties available to
    almost every window: size, background and foreground colors, and font
    """
    def __init__(self, name, klass, parent, id, property_window, show=True):
        EditBase.__init__(self, name, klass, parent, id, property_window,
                          show=False)
        # 'property' id (editable by the user) 
        self.window_id = -1

        def set_id(value):
            self.window_id = value
        self.access_functions['id'] = (lambda s=self: s.window_id, set_id)
        self.access_functions['size'] = (self.get_size, self.set_size)
        self.access_functions['background'] = (self.get_background,
                                               self.set_background)
        self.access_functions['foreground'] = (self.get_foreground,
                                               self.set_foreground)
        self.access_functions['font'] = (self.get_font, self.set_font)

        min_x = wxSystemSettings_GetSystemMetric(wxSYS_WINDOWMIN_X)
        min_y = wxSystemSettings_GetSystemMetric(wxSYS_WINDOWMIN_Y)
        max_x = wxSystemSettings_GetSystemMetric(wxSYS_SCREEN_X)
        max_y = wxSystemSettings_GetSystemMetric(wxSYS_SCREEN_Y)

        prop = self.properties
        prop['id'] = TextProperty(self, 'id', None, can_disable=True)
        prop['size'] = TextProperty(self, 'size', None, can_disable=True)
        prop['background'] = ColorDialogProperty(self, "background", None)
        prop['foreground'] = ColorDialogProperty(self, "foreground", None)
        prop['font'] = FontDialogProperty(self, "font", None)

    def finish_widget_creation(self):
        prop = self.properties
        size = prop['size'].get_value()
        if size: self.widget.SetSize([int(s) for s in size.split(',')])
        else: prop['size'].set_value('%s, %s' % tuple(self.widget.GetSize()))
        background = prop['background'].get_value()
        if background:
            self.widget.SetBackgroundColour(misc.string_to_color(background))
        foreground = prop['foreground'].get_value()
        if foreground:
            self.widget.SetForegroundColour(misc.string_to_color(foreground))
        font = prop['font'].get_value()
        if font: self.set_font(font)
        EditBase.finish_widget_creation(self)
        EVT_SIZE(self.widget, self.on_size)
        # after setting various Properties, we must Refresh widget in order to
        # see changes
        self.widget.Refresh()

    def create_properties(self):
        EditBase.create_properties(self)
        min_x = wxSystemSettings_GetSystemMetric(wxSYS_WINDOWMIN_X)
        min_y = wxSystemSettings_GetSystemMetric(wxSYS_WINDOWMIN_Y)
        max_x = wxSystemSettings_GetSystemMetric(wxSYS_SCREEN_X)
        max_y = wxSystemSettings_GetSystemMetric(wxSYS_SCREEN_Y)

        panel = self._common_panel
            
        prop = self.properties
        prop['id'].display(panel)
        prop['size'].display(panel)
        prop['background'].display(panel) 
        prop['foreground'].display(panel)
        try: prop['font'].display(panel) 
        except KeyError: pass

        sizer_tmp = wxBoxSizer(wxVERTICAL)
        sizer_tmp.Add(self.name_prop.panel, 0, wxEXPAND)
        sizer_tmp.Add(self.klass_prop.panel, 0, wxEXPAND)
        sizer_tmp.Add(prop['id'].panel, 0, wxEXPAND)
        sizer_tmp.Add(prop['size'].panel, 0, wxEXPAND)
        sizer_tmp.Add(prop['background'].panel, 0, wxEXPAND)
        sizer_tmp.Add(prop['foreground'].panel, 0, wxEXPAND)
        try: sizer_tmp.Add(prop['font'].panel, 0, wxEXPAND)
        except KeyError: pass
        panel.SetAutoLayout(1)
        panel.SetSizer(sizer_tmp)
        sizer_tmp.Layout()
        sizer_tmp.Fit(panel)

        self.notebook.AddPage(panel, "Common")

        self.property_window.Layout()
        w, h = panel.GetClientSize()
        panel.SetScrollbars(1, 5, 1, math.ceil(h/5.0))        

    def on_size(self, event):
        """\
        update the value of the 'size' property
        """
        try: self.properties['size'].set_value("%s, %s" % \
                                               self.widget.GetSizeTuple())
        except KeyError: pass
        event.Skip()

    def get_background(self):
        if not self.widget: return '' # this is an invalid color
        return misc.color_to_string(self.widget.GetBackgroundColour())

    def get_foreground(self):
        if not self.widget: return '' # this is an invalid color
        return misc.color_to_string(self.widget.GetForegroundColour())

    def set_background(self, value):
        if not self.widget: return
        try: color = misc.string_to_color(value)
        except: self.properties['background'].set_value(self.get_background())
        else:
            self.widget.SetBackgroundColour(color)
            self.widget.Refresh()
            
    def set_foreground(self, value):
        if not self.widget: return
        try: color = misc.string_to_color(value)
        except: self.properties['background'].set_value(self.get_background())
        else:
            self.widget.SetForegroundColour(color)
            self.widget.Refresh()

    def get_font(self):
        if not self.widget: return '' # this is an invalid font
        font = self.widget.GetFont()
        families = FontDialogProperty.font_families_from
        styles = FontDialogProperty.font_styles_from
        weights = FontDialogProperty.font_weights_from
        return "['%s', '%s', '%s', '%s', '%s', '%s']" % \
               (font.GetPointSize(), families[font.GetFamily()],
                styles[font.GetStyle()], weights[font.GetWeight()],
                font.GetUnderlined(), font.GetFaceName())

    def set_font(self, value):
        if not self.widget: return
        families = FontDialogProperty.font_families_to
        styles = FontDialogProperty.font_styles_to
        weights = FontDialogProperty.font_weights_to
        try:
            value = eval(value)
            f = wxFont(int(value[0]), families[value[1]], styles[value[2]],
                       weights[value[3]], int(value[4]), str(value[5]))
        except: self.properties['font'].set_value(self.get_font())
        else: self.widget.SetFont(f)

    def set_width(self, value):
        self.set_size((int(value), -1))

    def set_height(self, value):
        self.set_size((-1, int(value)))

    def set_size(self, value):
        if not self.widget: return
        try:
            size = [int(t.strip()) for t in value.split(',')]
        except:
            self.properties['size'].set_value('%s, %s' % \
                                              tuple(self.widget.GetSize()))
        else:
            self.widget.SetSize(size)
            try: self.sizer.set_item(self.pos, size=self.widget.GetSize())
            except AttributeError: pass

    def get_size(self):
        if not self.widget: return '' # invalid size
        return "%s, %s" % tuple(self.widget.GetSize())

    def get_property_handler(self, name):
        if name == 'font':
            class FontHandler:
                def __init__(self, owner):
                    self.owner = owner
                    self.props = range(6)
                    self.index = 0
                def start_elem(self, name, attrs):
                    index = { 'size': 0, 'family': 1, 'style': 2, 'weight': 3,
                              'underlined': 4, 'face': 5 }
                    self.index = index.get(name, 5)
                def end_elem(self, name):
                    if name == 'font':
                        self.owner.properties['font'].set_value(
                            repr(self.props))
                        self.owner.properties['font'].toggle_active(True)
                        self.owner.set_font(repr(self.props))
                        return True # to remove this handler
                def char_data(self, data):
                    self.props[self.index] = str(data.strip())
            # end of class FontHandler
            return FontHandler(self)
        return None

# end of class WindowBase


class ManagedBase(WindowBase):
    """\
    Base class for every managed window used by the builder: extends WindowBase
    with the addition of properties relative to the layout of the window:
    option, flag, and border
    """
    def __init__(self, name, klass, parent, id, sizer, pos, property_window,
                 show=True):
        WindowBase.__init__(self, name, klass, parent, id, property_window,
                            show=show)
        # selection markers
        self.sel_marker = None
        # dictionary of properties relative to the sizer which
        # controls this window
        self.sizer_properties = {}
        # attributes to keep the values of the sizer_properties
        self.option = 0
        self.flag = 0
        self.border = 0
        
        self.sizer = sizer
        self.pos = pos
        self.access_functions['option'] = (self.get_option, self.set_option)
        self.access_functions['flag'] = (self.get_flag, self.set_flag)
        self.access_functions['border'] = (self.get_border, self.set_border)
        self.flags_pos = (wxEXPAND, wxALIGN_RIGHT, wxALIGN_BOTTOM,
                          wxALIGN_CENTER_HORIZONTAL, wxALIGN_CENTER_VERTICAL,
                          wxLEFT, wxRIGHT, wxTOP, wxBOTTOM)
        sizer.add_item(self, pos)

        szprop = self.sizer_properties
        szprop['option'] = SpinProperty(self, "option", None, 0, (0, 1000))
        flag_labels = ('#section#Alignment', 'wxEXPAND', 'wxALIGN_RIGHT',
                       'wxALIGN_BOTTOM', 'wxALIGN_CENTER_HORIZONTAL',
                       'wxALIGN_CENTER_VERTICAL', '#section#Border',
                       'wxLEFT', 'wxRIGHT', 'wxTOP', 'wxBOTTOM')
        szprop['flag'] = CheckListProperty(self, 'flag', None, flag_labels)
        szprop['border'] = SpinProperty(self, 'border', None, 0, (0, 1000))

    def finish_widget_creation(self):
        self.sel_marker = misc.SelectionMarker(self.widget, self.parent.widget)
        WindowBase.finish_widget_creation(self)
        EVT_LEFT_DOWN(self.widget, self.on_set_focus)
        EVT_MOVE(self.widget, self.on_move)
        # re-add the item to update it
        self.sizer.add_item(self, self.pos, self.option, self.flag,
                            self.border, self.widget.GetSize())
        # set the value of the properties
        szp = self.sizer_properties
        szp['option'].set_value(self.get_option())
        szp['flag'].set_value(self.get_flag())
        szp['border'].set_value(self.get_border())

    def create_properties(self):
        WindowBase.create_properties(self)
        panel = wxScrolledWindow(self.notebook, -1)

        min_x = wxSystemSettings_GetSystemMetric(wxSYS_WINDOWMIN_X)
        min_y = wxSystemSettings_GetSystemMetric(wxSYS_WINDOWMIN_Y)
        max_x = wxSystemSettings_GetSystemMetric(wxSYS_SCREEN_X)
        max_y = wxSystemSettings_GetSystemMetric(wxSYS_SCREEN_Y)

        szprop = self.sizer_properties
        szprop['option'].display(panel)
        szprop['flag'].display(panel)
        szprop['border'].display(panel)

        sizer_tmp = wxBoxSizer(wxVERTICAL)
        sizer_tmp.Add(szprop['option'].panel, 0, wxEXPAND)
        sizer_tmp.Add(szprop['border'].panel, 0, wxEXPAND)
        sizer_tmp.Add(szprop['flag'].panel, 0, wxEXPAND, 5)
        panel.SetAutoLayout(True)
        panel.SetSizer(sizer_tmp)
        sizer_tmp.Layout()
        sizer_tmp.Fit(panel)

        self.notebook.AddPage(panel, "Layout")
        w, h = panel.GetClientSize()
        panel.SetScrollbars(1, 5, 1, math.ceil(h/5.0))
        
    def update_view(self, selected):
        if self.sel_marker: self.sel_marker.Show(selected)

    def on_move(self, event): self.sel_marker.update()
    def on_size(self, event):
        WindowBase.on_size(self, event)
        self.sel_marker.update()

    def set_option(self, value):
        self.option = value = int(value)
        if not self.widget: return
        try:
            sz = self.properties['size']
            if value or sz.is_active():
                size = [int(s.strip()) for s in sz.get_value().split(',')]
            else: size = self.widget.GetBestSize()
            self.sizer.set_item(self.pos, option=value, size=size)
        except AttributeError, e:
            print e

    def set_flag(self, value):
        value = self.sizer_properties['flag'].prepare_value(value)
        flags = 0
        for v in range(len(value)):
            if value[v]:
                flags |= self.flags_pos[v]
        self.flag = flags
        if not self.widget: return
        try:
            try:
                size = [ int(s) for s in
                         self.properties['size'].get_value().split(',') ]
            except ValueError:
                size = None
            if not (flags & wxEXPAND) and \
               not self.properties['size'].is_active():
                size = self.widget.GetBestSize()
            self.sizer.set_item(self.pos, flag=flags, size=size)
        except AttributeError, e:
            import traceback; traceback.print_exc()

    def set_border(self, value):
        self.border = int(value)
        if not self.widget: return
        try:
            size = [ int(s) for s in
                     self.properties['size'].get_value().split(',') ]
            self.sizer.set_item(self.pos, border=int(value), size=size)
        except AttributeError, e:
            import traceback; traceback.print_exc()

    def get_option(self):
##         try: return self.sizer.GetChildren()[self.pos].GetOption()
##         except AttributeError: return 0
        return self.option

    def get_flag(self):
        retval = [0] * len(self.flags_pos)
        try:
            #flag = self.sizer.GetChildren()[self.pos].GetFlag()
            for i in range(len(self.flags_pos)):
                if self.flag & self.flags_pos[i]:
                    retval[i] = 1
        except AttributeError: pass
        return retval

    def get_int_flag(self):
        #return self.sizer.GetChildren()[self.pos].GetFlag()
        return self.flag

    def get_border(self):
##         try: return self.sizer.GetChildren()[self.pos].GetBorder()
##         except AttributeError: return 0
        return self.border

    def delete(self):
        if self.sel_marker:
            self.sel_marker.Destroy() # destroy the selection markers
        WindowBase.delete(self)

    def remove(self, *args):
##         from edit_sizers import SizerSlot
##         elem = self.sizer.GetChildren()[self.pos]
##         w = SizerSlot(self.parent, self.sizer, self.pos)
##         try:
##             self.sizer.elements[self.sizer.elements.index(self)] = w
##         except (IndexError, KeyError):
##             pass        
##         elem.SetWindow(w)
##         elem.SetOption(1)
##         elem.SetBorder(0)
##         elem.SetFlag(wxEXPAND)
##         self.sizer.Layout()
        self.sizer.free_slot(self.pos)        
        WindowBase.remove(self)

# end of class ManagedBase


class TopLevelBase(WindowBase):
    """\
    Base class for every non-managed window (i.e. Frames and Dialogs).
    """
    def __init__(self, name, klass, parent, id, property_window, show=1):
        WindowBase.__init__(self, name, klass, parent, id, property_window,
                            show=show)
        self.access_functions['title'] = (self.get_title, self.set_title)
        self.properties['title'] = TextProperty(self, 'title', None)
        self.sizer = None # sizer that controls the layout of the children
                          # of the window

    def finish_widget_creation(self):
        WindowBase.finish_widget_creation(self)
        for label in ("Copy", "Cut"):
            item_id = self._rmenu.FindItem(label)
            self._rmenu.Delete(item_id)
        HIDE_ID = wxNewId()
        self._rmenu.Append(HIDE_ID, 'Hide')
        self.widget.SetTitle(self.properties['title'].get_value())
        EVT_MENU(self.widget, HIDE_ID, self.hide_widget)
        EVT_LEFT_DOWN(self.widget, self.drop_sizer)
        EVT_ENTER_WINDOW(self.widget, self.on_enter)
        EVT_CLOSE(self.widget, self.hide_widget)
        if wxPlatform == '__WXMSW__':
            # MSW isn't smart enough to avoid overlapping windows, so
            # at least move it away from the 3 wxGlade frames
            self.widget.CenterOnScreen()

    def create_properties(self):
        WindowBase.create_properties(self)
        panel = self.notebook.GetPage(0)
        self.properties['title'].display(panel)
        sizer_tmp = panel.GetSizer()
        sizer_tmp.Add(self.properties['title'].panel, 0, wxEXPAND)
        sizer_tmp.Layout()
        sizer_tmp.Fit(panel)

    def get_title(self):
        if not self.widget: return self.name
        return self.widget.GetTitle()

    def set_title(self, value):
        if not self.widget: return
        self.widget.SetTitle(value)

    def set_sizer(self, sizer):
        self.sizer = sizer
        if self.sizer and self.sizer.widget and self.widget:
            self.widget.SetAutoLayout(True)
            self.widget.SetSizer(self.sizer.widget)
            self.widget.Layout()

    def on_enter(self, event):
        if not self.sizer and common.adding_sizer:
            self.widget.SetCursor(wxCROSS_CURSOR)
        else:
            self.widget.SetCursor(wxNullCursor)

    def drop_sizer(self, event):
        if self.sizer or not common.adding_sizer:
            self.on_set_focus(event) # default behaviour: call show_properties
            return
        common.adding_widget = common.adding_sizer = False
        self.widget.SetCursor(wxNullCursor)
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

# end of class TopLevelBase
