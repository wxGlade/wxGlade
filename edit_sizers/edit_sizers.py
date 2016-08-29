"""
Hierarchy of Sizers supported by wxGlade

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

from collections import OrderedDict
import logging, math, re
import wx
from wx.lib.buttons import GenButton

import new_properties as np
from edit_windows import EditStylesMixin
from tree import Tree, WidgetTree, Node, SlotNode
import clipboard
import common, compat, config, misc


class BaseSizerBuilder(object):
    "Language independent base class for all sizer builders"

    tmpl = []                 # Statements to generate the sizer from, the stmt has to end with a newline character

    klass = ''                # klass: Sizer class name
    language = None           # Language to generate the code for

    tmpl_SetSizer = ''        # Template to call SetSizer()
    tmpl_Fit = ''             # Template to call Fit()
    tmpl_SetSizeHints = ''    # Template to set the size hints
    tmpl_AddGrowableRow = ''  # Template for wxFlexGridSizer to set growable rows
    tmpl_AddGrowableCol = ''  # Template for wxFlexGridSizer to set growable columns

    def __init__(self):
        "Initialise sizer builder"
        self.props_get_code = {}                          # properties to replace in L{tmpl}
        self.codegen = common.code_writers[self.language] #language specific code generator (codegen.BaseLangCodeWriter)

    def _get_wparent(self, obj):
        "Return the parent widget or a reference to it as string"
        raise NotImplementedError

    def _get_code(self, obj):
        "Generates the language specific code for sizer specified in L{klass}"
        if not self.tmpl:
            return [], [], []

        init = []
        layout = []

        # append default properties
        self.props_get_code['sizer_name'] = obj.name
        self.props_get_code['klass'] = self.codegen.cn(self.klass)
        self.props_get_code['wxIDANY'] = self.codegen.cn('wxID_ANY')
        self.props_get_code['parent_widget'] = self._get_wparent(obj)

        self.props_get_code['sizer_name'] = self.codegen._format_classattr(obj)

        # generate init lines from tmpl filled with props_get_code
        init.append(self.tmpl % self.props_get_code)

        # generate layout lines
        if obj.is_toplevel:
            layout.append(self.tmpl_SetSizer % self.props_get_code)
            if not 'size' in obj.parent.properties and obj.parent.is_toplevel:
                layout.append(self.tmpl_Fit % self.props_get_code)
            if obj.parent.properties.get('sizehints', False):
                layout.append(self.tmpl_SetSizeHints % self.props_get_code)

        return init, [], layout

    def get_code(self, obj):
        "Generates the language specific code for sizer specified in L{klass}"
        if self.klass == 'wxBoxSizer':       return self.get_code_wxBoxSizer(obj)
        if self.klass == 'wxStaticBoxSizer': return self.get_code_wxStaticBoxSizer(obj)
        if self.klass == 'wxGridSizer':      return self.get_code_wxGridSizer(obj)
        if self.klass == 'wxFlexGridSizer':  return self.get_code_wxFlexGridSizer(obj)
        return self._get_code(obj)

    def get_code_wxStaticBoxSizer(self, obj):
        "Set sizer specific properties and generate the code"
        self.props_get_code.clear()
        self.props_get_code['orient'] = self.codegen.cn( obj.properties.get('orient', 'wxHORIZONTAL') )
        self.props_get_code['label'] = self.codegen.quote_str( obj.properties.get('label', '') )
        return self._get_code(obj)

    def get_code_wxBoxSizer(self, obj):
        "Set sizer specific properties and generate the code"
        self.props_get_code.clear()
        self.props_get_code['orient'] = self.codegen.cn( obj.properties.get('orient', 'wxHORIZONTAL') )
        return self._get_code(obj)

    def get_code_wxGridSizer(self, obj):
        "Set sizer specific properties and generate the code"
        self.props_get_code.clear()
        self.props_get_code['rows'] = obj.properties.get('rows', '0')
        self.props_get_code['cols'] = obj.properties.get('cols', '0')
        self.props_get_code['vgap'] = obj.properties.get('vgap', '0')
        self.props_get_code['hgap'] = obj.properties.get('hgap', '0')
        return self._get_code(obj)

    def get_code_wxFlexGridSizer(self, obj):
        "Set sizer specific properties and generate the code"
        result = self.get_code_wxGridSizer(obj)

        # convert tuple to list
        result = list(result)

        # extract layout lines
        layout = result[-1]
        del result[-1]

        props = obj.properties
        if 'growable_rows' in props:
            for row in props['growable_rows'].split(','):
                self.props_get_code['row'] = row.strip()
                layout.append(self.tmpl_AddGrowableRow % self.props_get_code)
        if 'growable_cols' in props:
            for col in props['growable_cols'].split(','):
                self.props_get_code['col'] = col.strip()
                layout.append(self.tmpl_AddGrowableCol % self.props_get_code)

        # reappend layout lines
        result.append(layout)
        return result



class SizerSlot(np.PropertyOwner):
    "A window to represent a slot in a sizer"
    PROPERTIES = ["Slot", "pos"]
    def __init__(self, parent, sizer, pos=0):
        np.PropertyOwner.__init__(self)
        # initialise instance logger
        self._logger = logging.getLogger(self.__class__.__name__)
        self.klass = "SLOT"

        self.sizer = sizer       # Sizer object (SizerBase instance)

        # initialise instance properties
        self.parent = parent
        self.pos =p = np.LayoutPosProperty(pos, sizer)  # position within the sizer, 1-based
        p.readonly = True

        self.widget = None       # Reference to the widget resembling the slot (a wx.Window)
        self._rmenu = None       # context menu and widget, when active
        self.name = "SLOT"

    def post_load(self): # called from show_widget
        pass

    def update_view(self, selected):
        # we can ignore selected here, as the repainting only takes place later
        if self.widget:
            self.widget.Refresh()

    def create_widget(self):
        style = wx.FULL_REPAINT_ON_RESIZE
        self.widget = wx.Window(self.parent.widget, -1, size=(20, 20), style=style)
        self.widget.SetBackgroundColour(wx.LIGHT_GREY)
        self.widget.SetAutoLayout(True)
        self.widget.Bind(wx.EVT_PAINT, self.on_paint)
        self.widget.Bind(wx.EVT_RIGHT_DOWN, self.popup_menu)
        self.widget.Bind(wx.EVT_LEFT_DOWN, self.on_drop_widget)
        self.widget.Bind(wx.EVT_MIDDLE_DOWN, misc.exec_after(self.on_select_and_paste))
        self.widget.Bind(wx.EVT_ENTER_WINDOW, self.on_enter)
        self.widget.Bind(wx.EVT_LEAVE_WINDOW, self.on_leave)
        self.widget.Bind(wx.EVT_KEY_DOWN, misc.on_key_down_event)

    def is_visible(self):
        return False

    def show_widget(self, yes):
        if yes and not self.widget:
            self.create_widget()
        if self.widget:
            self.widget.Show(yes)
            self.widget.Refresh()

    def on_enter(self, event):
        # hack. definitely. but...
        misc.currently_under_mouse = self.widget
        if common.adding_widget and (not common.adding_sizer or not self.sizer.is_virtual()):
            self.widget.SetCursor(wx.CROSS_CURSOR)
        else:
            self.widget.SetCursor(wx.STANDARD_CURSOR)
        event.Skip()

    def on_leave(self, event):
        # currently_under_mouse is used to restore the normal cursor, if the
        # user cancelled the addition of a widget and the cursor is over this slot
        misc.currently_under_mouse = None
        event.Skip()

    def on_paint(self, event):
        "Handle paint request and draw hatched lines onto the window"
        dc = wx.PaintDC(self.widget)
        # fill background first
        dc.SetBackground(wx.Brush(wx.LIGHT_GREY))
        dc.Clear()
        # draw the hatches (red if selected)
        color = wx.RED  if misc.focused_widget is self  else  wx.BLACK
        if self.pos % 2:
            brush = wx.Brush(color, wx.FDIAGONAL_HATCH)
        else:
            brush = wx.Brush(color, wx.BDIAGONAL_HATCH)
        # don't draw hatched lines in background on MSW due to wxWidget bug #17326:
        # "SetBackground() with hatched brushes cases black background on MSW"
        if wx.Platform == '__WXMSW__':
            # draw hatched lines in foreground
            dc.SetBrush(brush)
            size = self.widget.GetClientSize()
            dc.DrawRectangle(0, 0, size.width, size.height)
        else:
            # draw hatched lines in background
            dc.SetBackground(brush)
            dc.Clear()

    # context menu #####################################################################################################
    def popup_menu(self, event):
        event_widget = event.GetEventObject()
        menu = self._create_popup_menu(widget=event_widget)
        # convert relative event position to relative widget position
        event_pos  = event.GetPosition()
        screen_pos = event_widget.ClientToScreen(event_pos)
        client_pos = event_widget.ScreenToClient(screen_pos)
        event_widget.PopupMenu(menu, client_pos)

    def _create_popup_menu(self, widget):
        self._destroy_popup_menu()
        menu = wx.Menu(_("Slot %d"%self.pos))
        
        if not self.sizer.is_virtual():
            # we cannot remove items from virtual sizers
            i = misc.append_menu_item(menu, -1, _('Remove\tDel'), wx.ART_DELETE)
            misc.bind_menu_item_after(widget, i, self.remove)
        i = misc.append_menu_item(menu, -1, _('Paste\tCtrl+V'), wx.ART_PASTE)
        misc.bind_menu_item_after(widget, i, self.clipboard_paste)
        menu.AppendSeparator()

        p = misc.get_toplevel_widget(self.sizer)
        if p is not None and p.preview_is_visible():
            item = _('Close preview (%s)\tF5') % p.name
        else:
            item = _('Preview (%s)\tF5') % p.name

        i = misc.append_menu_item( menu, -1, item )
        misc.bind_menu_item_after(widget, i, self.preview_parent)

        self._rmenu = (menu, widget) # store for destryoing and unbinding
        return menu

    def _destroy_popup_menu(self):
        if self._rmenu is None: return
        menu, widget = self._rmenu
        widget.Unbind(wx.EVT_MENU)
        menu.Destroy()
        self._rmenu = None

    def preview_parent(self):
        # context menu callback
        self._destroy_popup_menu()
        p = misc.get_toplevel_widget(self.sizer)
        if p is not None:
            p.preview(None)

    ####################################################################################################################

    def remove(self, *args):
        self._destroy_popup_menu()
        if not self.sizer.is_virtual():
            node = self.sizer.children[self.pos].item.node
            self.sizer.remove_item(self)
            self.delete()
            common.app_tree.remove(node)

    def on_drop_widget(self, event):
        """replaces self with a widget in self.sizer. This method is called
        to add every non-toplevel widget or sizer, and in turn calls the
        appropriate builder function (found in the ``common.widgets'' dict)"""
        if not common.adding_widget:  # widget focused/selecte
            misc.set_focused_widget(self)
            if self.widget: self.widget.Refresh()
            self.widget.SetFocus()
            return
        if common.adding_sizer and self.sizer.is_virtual():
            return
        common.adding_widget = False
        common.adding_sizer = False
        if self.widget:
            self.widget.SetCursor(wx.NullCursor)
            self.widget.Hide()
        # call the appropriate builder
        common.widgets[common.widget_to_add](self.parent, self.sizer, self.pos)
        common.widget_to_add = None
        common.app_tree.app.saved = False

    # clipboard handling ###############################################################################################
    def check_compatibility(self, widget):
        "check whether widget can be pasted here"
        if getattr(widget, "_is_toplevel", False):
            return False
        return True

    def clipboard_paste(self, event=None, clipboard_data=None):
        "Insert a widget from the clipboard to the current destination"
        self._destroy_popup_menu()
        if self.widget:
            self.widget.Hide()
        if clipboard.paste(self.parent, self.sizer, self.pos, clipboard_data):
            common.app_tree.app.saved = False
        else:
            self.widget.Show()

    def on_select_and_paste(self, *args):
        "Middle-click event handler: selects the slot and, if the clipboard is not empty, pastes its content here"
        misc.focused_widget = self
        self.widget.SetFocus()
        self.clipboard_paste()
    ####################################################################################################################

    def delete(self):
        # mainly deletes the widget
        if misc.currently_under_mouse is self.widget:
            misc.currently_under_mouse = None

        if self.widget:
            self.widget.Hide()

            # unbind events to prevent new created (and queued) events
            self.widget.Bind(wx.EVT_PAINT, None)
            self.widget.Bind(wx.EVT_RIGHT_DOWN, None)
            self.widget.Bind(wx.EVT_LEFT_DOWN, None)
            self.widget.Bind(wx.EVT_MIDDLE_DOWN, None)
            self.widget.Bind(wx.EVT_ENTER_WINDOW, None)
            self.widget.Bind(wx.EVT_LEAVE_WINDOW, None)
            self.widget.Bind(wx.EVT_KEY_DOWN, None)

            self._destroy_popup_menu()
            self.widget = None

        if misc.focused_widget is self:
            misc.set_focused_widget(None)
        common.app_tree.app.saved = False


    def __getstate__(self):
        state = self.__dict__.copy()
        del state['_logger']
        return state

    def __setstate__(self, state):
        self.__dict__.update(state)

        # re-initialise logger instance deleted from __getstate__
        self._logger = logging.getLogger(self.__class__.__name__)



class SizerHandleButton(GenButton):
    'Provides a "handle" to activate a Sizer and to access its popup menu'
    def __init__(self, parent, id, sizer):
        GenButton.__init__(self, parent.widget, id, '', size=(5, 5))
        self.sizer = sizer
        self.SetUseFocusIndicator(False)
        wx.EVT_RIGHT_DOWN(self, self.sizer.popup_menu )
        wx.EVT_KEY_DOWN(self, misc.on_key_down_event)



class SizerItem(object):
    "Represents a child of a sizer"
    def __init__(self, item, pos, option=0, flag=0, border=0, size=None):
        self.item = item
        self.item.pos = pos
        self.proportion = option
        self.flag = flag
        self.border = border
        self.size = size


_change_sizer_panel = None  # reference to a hidden frame persistently shared between different calls of change_sizer

def change_sizer(old, new):
    """Replaces sizer instance 'old' with a new one; 'new' is the name of the new one.
    (which_page: index of the property windows notebook page; used only by set_growable_(rows|cols)"""
    constructors = {
        'wxBoxSizer (wxVERTICAL)':         lambda: EditBoxSizer(old.name, old.window, wx.VERTICAL, 0, old.toplevel),
        'wxBoxSizer (wxHORIZONTAL)':       lambda: EditBoxSizer(old.name, old.window, wx.HORIZONTAL, 0, old.toplevel),
        'wxStaticBoxSizer (wxVERTICAL)':   lambda: EditStaticBoxSizer(old.name, old.window, wx.VERTICAL,
                                                                      getattr(old, 'label', old.name), 0, old.toplevel),
        'wxStaticBoxSizer (wxHORIZONTAL)': lambda: EditStaticBoxSizer(old.name, old.window, wx.HORIZONTAL,
                                                                      getattr(old, 'label', old.name), 0, old.toplevel),
        'wxGridSizer':     lambda: EditGridSizer(old.name, old.window, rows=0, cols=0, toplevel=old.toplevel),
        'wxFlexGridSizer': lambda: EditFlexGridSizer(old.name, old.window, rows=0, cols=0, toplevel=old.toplevel)
    }

    szr = constructors[new]()
    szr.children.extend(old.children[1:])
    szr.node = old.node

    if isinstance(szr, GridSizerBase):
        szr.set_rows(getattr(old, 'rows', 1))
        szr.set_cols(getattr(old, 'cols', len(szr.children) - 1))
        szr.set_hgap(getattr(old, 'hgap', 0))
        szr.set_vgap(getattr(old, 'vgap', 0))
        if isinstance(szr, EditFlexGridSizer):
            try:
                grow_r = old.grow_rows
                grow_c = old.grow_cols
                if grow_r:
                    szr.grow_rows = grow_r
                    szr.properties['growable_rows'].toggle_active(True)
                    szr.properties['growable_rows'].set_value(szr.get_growable_rows())
                if grow_c:
                    szr.grow_cols = grow_c
                    szr.properties['growable_cols'].toggle_active(True)
                    szr.properties['growable_cols'].set_value(szr.get_growable_cols())
            except (AttributeError, KeyError):
                pass
    # XXX replace old sizer in parent.children;
    #     also check for tree
    global _change_sizer_panel
    if old.widget is not None:
        szr.show_widget(True, dont_set=True)
        if _change_sizer_panel is None:
            _change_sizer_panel = wx.Frame(None, -1, _("HIDDEN FRAME FOR CHANGE SIZER"))

    for c in szr.children[1:]:
        widget = c.item
        widget.sizer = szr
        if not isinstance(widget, SizerSlot):
            # This is necessary as a workaround to a wx.StaticBoxSizer issue:
            # it seems that the wx.StaticBox needs to come before any other
            # widget managed by the wx.StaticBoxSizer in the GetChildren()
            # list. Explicitly reparenting the widgets seems to solve the
            # problem
            if hasattr(widget.widget, 'GetParent'):
                p = widget.widget.GetParent()
                widget.widget.Reparent(_change_sizer_panel)
                widget.widget.Reparent(p)
            if szr.widget is not None:
                szr.widget.Insert(widget.pos, widget.widget, widget.proportion, widget.flag, widget.border)

    if old.widget is not None:
        # previously, this code was after compat.SizerItem_SetSizer(elem, szr.widget)
        # but this does not work any longer w. Phoenix, as probably AssignSizer deletes the old widget already
        for c in old.widget.GetChildren():
            if c and c.IsSizer():
                compat.SizerItem_SetSizer(c, None)
        old.widget.Clear()  # without deleting window items

    if not szr.toplevel:
        szr.sizer = old.sizer
        szr.proportion = old.proportion
        szr.flag = old.flag
        szr.border = old.border
        szr.pos = old.pos
        szr.sizer.children[szr.pos].item = szr
        if szr.sizer.widget:
            elem = szr.sizer.widget.GetChildren()[szr.pos]
            compat.SizerItem_SetSizer(elem, szr.widget)  # old.widget will be deleted here

    common.app_tree.change_node(szr.node, szr)

    old.toplevel = False
    old.children = old.children[:1]
    old.delete()

    if szr.toplevel:
        szr.window.set_sizer(szr)
    szr.layout(True)

    misc.set_focused_widget(szr)



class Sizer(object):
    "Base class for every Sizer handled by wxGlade"

    def __init__(self, window):
        self.window = window
        # initialise instance logger
        self._logger = logging.getLogger(self.__class__.__name__)

    def set_item(self, pos, option=None, flag=None, border=None, size=None, force_layout=True):
        "Updates the layout of the item at the given pos"
        raise NotImplementedError

    def add_item(self, item, pos=None, option=0, flag=0, border=0, size=None, force_layout=True):
        "Adds an item to self"
        raise NotImplementedError

    def remove_item(self, elem, force_layout=True):
        "Removes elem from self"
        pass

    def free_slot(self, pos, force_layout=True):
        "Replaces the element at pos with an empty slot"
        raise NotImplementedError

    def _fix_notebook(self, pos, notebook_sizer, force_layout=True):
        "Internal method used to replace a notebook widget with its notebook sizer."
        pass

    def is_virtual(self):
        "Return true if sizer is virtual (f.e. SplitterWindowSizer)"
        return False

    def get_itempos(self, attrs):
        """For virtual sizers only, returns the position of the item in the parent:
        this is used when loading a wxg file, to build the tree of widgets correctly"""
        raise NotImplementedError

    def __getstate__(self):
        state = self.__dict__.copy()
        del state['_logger']
        return state

    def __setstate__(self, state):
        self.__dict__.update(state)
        # re-initialise logger instance deleted from __getstate__
        self._logger = logging.getLogger(self.__class__.__name__)



class OrientProperty(np.Property):
    "orientation property for BoxSizers; hidden property to be set by the ClassOrientProperty"
    ORIENTATION_to_STRING = {wx.HORIZONTAL: 'wxHORIZONTAL', wx.VERTICAL: 'wxVERTICAL'}
    STRING_to_ORIENTATION = {'wxHORIZONTAL': wx.HORIZONTAL, 'wxVERTICAL': wx.VERTICAL}

    def __init__(self, value, default_value=None, name=None):
        np.Property.__init__(self, value, default_value, name)
    def _set_converter(self, value):
        if not value: return None
        if isinstance(value, int): return value
        return self.STRING_to_ORIENTATION.get(value, value)
    def _write_converter(self, value):
        if not value: return None
        return self.ORIENTATION_to_STRING[value]
    def get_str_value(self):
        return self.ORIENTATION_to_STRING[self.value]


class ClassOrientProperty(np.RadioProperty):
    # radio box: class name and orientation; will not be written to XML file, but will influence class and orient
    CHOICES = [ ('wxBoxSizer (wxVERTICAL)', 'without box and label'),
                ('wxBoxSizer (wxHORIZONTAL)', 'without box and label'),
                ('wxStaticBoxSizer (wxVERTICAL)', 'with box and label'),
                ('wxStaticBoxSizer (wxHORIZONTAL)', 'with box and label'),
                ('wxGridSizer', None),
                ('wxFlexGridSizer', "with columns/rows of different widths") ]

    TOOLTIPS = [c[1] for c in CHOICES]
    CHOICES  = [c[0] for c in CHOICES]

    def __init__(self, value=None):
        np.RadioProperty.__init__(self, value, self.CHOICES, tooltips=self.TOOLTIP)
    def write(self, outfile, tabs=0):
        pass


class SizerBase(Sizer, np.PropertyOwner):
    "Base class for every non-virtual Sizer handled by wxGlade"
    PROPERTIES = ["Common", "name", "class", "orient", "class_orient", # class and orient are hidden
   "attribute"]
    EXTRA_PROPERTIES = []
    
    MANAGED_PROPERTIES  = ["pos", "proportion", "border", "flag"]
    TOPLEVEL_PROPERTIES = ["fit"]

    _PROPERTY_LABELS = {"fit":"Fit parent",
                        "attribute":'Store as attribute'}
    _PROPERTY_HELP = {"fit":'Sizes the window so that it fits around its subwindows'}

    def __init__(self, name, klass, orient, window, toplevel=True, show=True, menu=None):
        np.PropertyOwner.__init__(self)
        Sizer.__init__(self, window)

        # if True, self is not inside another sizer, but it is the responsible of the layout of self.window
        self.toplevel = toplevel

        self.id = wx.NewId()

        # initialise instance properties
        self.name         = np.TextProperty(name)
        self.klass        = np.Property(klass, name="class")             # class and orient are hidden
        self.orient       = OrientProperty(orient)                       # they will be set from the class_orient property
        self.class_orient = ClassOrientProperty(self.get_class_orient()) # this will set the class and orient properties
        self.base         = np.TextProperty(klass, "base")
        self.attribute    = np.CheckBoxProperty(False, default_value=False)
        self.fit          = np.ActionButtonProperty(self.fit_parent)

        if not self.toplevel:
            self.PROPERTIES = self.PROPERTIES + self.MANAGED_PROPERTIES + self.EXTRA_PROPERTIES
            # if within another sizer: the arguments to sizer.Add(self, proportion, flag, border)
            # same as for edit_windows.ManagedBase
            self.pos    = np.LayoutPosProperty(0, None)  # the position within the sizer, 0-based
            self.proportion = np.SpinProperty(1, name="option", immediate=True)
            self.border = np.SpinProperty(0, immediate=True)
            self.flag   = np.ManagedFlags(wx.EXPAND)
            self.sizer = None  # the (parent) sizer instance
        else:
            self.PROPERTIES = self.PROPERTIES + self.TOPLEVEL_PROPERTIES + self.EXTRA_PROPERTIES

        self.children = []    # widgets added to the sizer
        self.widget = None    # actual wxSizer instance
        self._btn = None      # "handle" to activate a Sizer and to access its popup menu (SizerHandleButton)
        self._rmenu = None    # the popup menu

    def create_widget(self):
        "Creates the wxSizer self.widget"
        raise NotImplementedError

    def show_widget(self, yes, dont_set=False):
        if not yes or self.widget:
            return  # nothing to do if the sizer has already been created
        self._btn = SizerHandleButton(self.window, self.id, self ) # XXX handle the popupmenu creation in SizerHandleButton
        # ScreenToClient used by WidgetTree for the popup menu
        wx.EVT_BUTTON(self._btn, self.id, self.on_selection)
        self.create_widget()
        self.widget.Refresh = self.refresh
        self.widget.GetBestSize = self.widget.GetMinSize
        self.widget.ScreenToClient = self._btn.ScreenToClient
        if self.toplevel and not dont_set:
            self.window.set_sizer(self)
        if not config.preferences.show_sizer_handle:
            self.widget.Show(self._btn, False)
        self.update_view(misc.focused_widget == self)
    def on_selection(self, event):
        # button clicked -> set ourself as current widget
        misc.set_focused_widget(self)

    def set_containing_sizer(self, sizer):
        self.sizer = sizer
        #self.properties['option'].set_sizer(sizer)  # XXX create a "SizerProportion property?"
        self.properties['pos'].set_sizer(sizer)
        #p = self.properties.get('pos')
        #if p: p.set_sizer(sizer)



    def set_pos(self, value):
        wx.CallAfter( self.sizer.change_item_pos, self, min( value+1, len(self.sizer.children)-1 ) )

    def update_pos(self, value):
        # self._logger.debug('update pos: %s, %s', self.name, value)
        self.sizer_properties['pos'].set_value(value-1)
        self.pos = value

    def get_class_orient(self):
        # as string
        return self.WX_CLASS

    def properties_changed(self, modified):
        # "class" and "orient" will only display; "class_orient" 
        if not modified or "class" in modified:
            self.properties["class_orient"].set(self.get_class_orient())
        if not modified or "orient" in modified:
            self.properties["class_orient"].set(self.get_class_orient())
        if "class_orient" in modified:
            # user has selected -> change
            value = self.class_orient
            wx.CallAfter(change_sizer, self, value)
        np.PropertyOwner.properties_changed(self, modified)

    def check_compatibility(self, widget):
        return "AddSlot" # a slot is to be added before inserting/pasting

    # popup menu #######################################################################################################
    def popup_menu(self, event):
        "pops up a menu to add or remove slots from self, or to remove self from the application."
        event_widget = event.GetEventObject()
        menu = self._create_popup_menu(widget=event_widget)
        # convert relative event position to relative widget position
        event_pos  = event.GetPosition()
        screen_pos = event_widget.ClientToScreen(event_pos)
        client_pos = event_widget.ScreenToClient(screen_pos)
        event_widget.PopupMenu(menu, client_pos)

    def _create_popup_menu(self, widget):
        # provide popup menu for removal
        self._destroy_popup_menu()
        menu = misc.wxGladePopupMenu(self.name)

        i = misc.append_menu_item(menu, -1, _('Remove\tDel'), wx.ART_DELETE)
        misc.bind_menu_item_after(widget, i, self._remove)

        if not self.toplevel and self.sizer:
            i = misc.append_menu_item( menu, -1, _('Insert slot before...') )
            misc.bind_menu_item_after(widget, i, self.sizer.insert_slot, self.pos)
            menu.AppendSeparator()
        # other menu items: add/insert slot, copy, cut
        i = misc.append_menu_item( menu, -1, _('Add slot') )
        misc.bind_menu_item_after(widget, i, self.add_slot)
        i = misc.append_menu_item( menu, -1, _('Copy\tCtrl+C'), wx.ART_COPY )
        misc.bind_menu_item_after(widget, i, self.clipboard_copy)
        i = misc.append_menu_item( menu, -1, _('Cut\tCtrl+X'), wx.ART_CUT )
        misc.bind_menu_item_after(widget, i, self.clipboard_cut)

        ## for testing without property window: change sizer orientation
        #if hasattr(self, "orient"):
            #menu.AppendSeparator()
            #i = misc.append_menu_item( menu, -1, 'Change orientation' )
            #misc.bind_menu_item_after(widget, i, self.change_orientation)

        # preview (create or close?)
        menu.AppendSeparator()
        p = misc.get_toplevel_widget(self)
        if p is not None and p.preview_is_visible():
            item = _('Close preview (%s)\tF5') % p.name
        else:
            item = _('Preview (%s)\tF5') % p.name
        i = misc.append_menu_item( menu, -1, item )
        misc.bind_menu_item_after(widget, i, self.preview_parent)

        self._rmenu = (menu, widget) # store for destryoing and unbinding
        return menu

    #def change_orientation(self):
        ## for testing without property window
        #current = self.ORIENTATION_to_STRING[self.orient]
        #other = [v for v in self.ORIENTATION_to_STRING.values() if v!=current][0]
        #new = "%s (%s)"%( self.WX_CLASS, other)
        #change_sizer(self, new)

    def _destroy_popup_menu(self):
        if self._rmenu is None: return
        menu, widget = self._rmenu
        widget.Unbind(wx.EVT_MENU)
        menu.Destroy()
        self._rmenu = None
        ####################################################################################################################

    def _remove(self, *args):
        "removes the sizer from his parent, if it has one"
        if self.toplevel:
            window = self.window
            common.app_tree.remove(self.node)
            window.set_sizer(None)
            return
        # XXX as of now: remove old and then create a new slot
        common.app_tree.remove(self.node)
        sizer = self.sizer
        self.sizer.remove_item(self)
        sizer.insert_slot(self.pos)
        # XXX as of now, this won't work
        #self.sizer.free_slot(self.pos)
        #for node in self.node.children:
        #    common.app_tree.remove(self.node)
        self._destroy_popup_menu()

    remove = _remove # needed for consistency (common.focused_widget.remove)

    def Destroy(self):
        self._destroy_popup_menu()
        GenButton.Destroy(self)
        if misc.focused_widget is self:
            misc.focused_widget = None

    def preview_parent(self):
        self._destroy_popup_menu()
        p = misc.get_toplevel_widget(self)
        p.on_preview()

    def set_name(self, value):
        value = "%s" % value
        if ( not config.preferences.allow_duplicate_names
             and (self.widget and common.app_tree.has_name(value, self.node)) ):
            wx.CallAfter( wx.MessageBox, _('Name "%s" is already in use.\nPlease enter a different one.') % value,
                                         _("Error"), wx.OK | wx.ICON_ERROR)
            self.name_prop.set_value(self.name)
            return
        if not re.match(self.set_name_pattern, value):
            self.name_prop.set_value(self.name)
        else:
            oldname = self.name
            self.name = value
            #self.set_menu_title(value)
            try:
                common.app_tree.refresh_name(self.node, oldname)
            except AttributeError:
                self._logger.exception(_('Internal Error'))

    set_name_pattern = re.compile(r'^[a-zA-Z_]+[\w0-9]*$')

    def __getitem__(self, value):
        return self.access_functions[value]

    def fit_parent(self, *args):
        "Tell the sizer to resize the window to match the sizer's minimal size"
        if self.widget and self.window.widget:
            self.widget.Fit(self.window.widget)
            # self.widget.SetSizeHints(self.window.widget)
            self.window.widget.Layout()

    def add_item(self, item, pos=None, proportion=0, flag=0, border=0, size=None, force_layout=True):
        "Adds an item to self."
        proportion = int(proportion)
        flag = int(flag)
        border = int(border)
        if pos is None:
            pos = len(self.children)
        if pos==len(self.children):
            self.add_slot(add_node=False)
        try:
            old_child = self.children[pos]
            if isinstance(old_child.item, SizerSlot):
                old_child.item.delete()
            self.children[pos] = SizerItem(item, pos, proportion, flag, border, size)
        except IndexError:  # this shouldn't happen!
            self._logger.exception(_('Internal Error'))
            self._logger.error('%s, %s', self.children, pos)
            raise SystemExit

        if hasattr(item, 'set_containing_sizer'):
            item.set_containing_sizer(self)
        else:
            item.sizer = self
        item.properties["pos"].set(pos)

        self._add_item_widget(item, pos, proportion, flag, border, size, force_layout)

    def _add_item_widget(self, item, pos, proportion, flag, border, size, force_layout):
        if not self.widget:
            return  # nothing more to do
        if not item.widget:
            return

        try:
            elem = self.widget.GetChildren()[pos]
        except IndexError:  # this happens after loading from xml
            # I have to set wxADJUST_MINSIZE to handle a bug that I'm not
            # able to detect (yet): if the width or height of a widget is -1,
            # the layout is messed up!

            self.widget.Add(item.widget, proportion, flag, border)

            if size:
                w, h = size
            else:
                w, h = item.widget.GetBestSize()
            if w == -1:
                w = item.widget.GetBestSize()[0]
            if h == -1:
                h = item.widget.GetBestSize()[1]
            self.widget.SetItemMinSize(item.widget, w, h)
            return

        self.widget.Insert(pos, item.widget, proportion, flag, border)
        self.widget.Remove(pos + 1)
        # XXX
        #si = wx.SizerItem(item.widget, option, flag, border)
        #self.widget.Replace(pos, si )
        if elem.IsWindow(): # delete old window at this position
            w = elem.GetWindow()
            w.SetContainingSizer(None)
            w.Destroy()

        try:  # if the item was a window, set its size to a reasonable value
            w,h = size or item.widget.GetBestSize()
            if w == -1: w = item.widget.GetBestSize()[0]
            if h == -1: h = item.widget.GetBestSize()[1]

            proportion = elem.GetProportion()
            flag = elem.GetFlag()
            if not size or (proportion == 0 or not (flag & wx.EXPAND)):
                self.widget.SetItemMinSize(item.widget, w, h)
            elif hasattr(item.widget, "GetBestSize"):
                w, h = item.widget.GetBestSize()
                self.widget.SetItemMinSize(item.widget, w, h)
        except Exception:
            # production version: exceptions to be ignored
            import os
            if 'WINGDB_ACTIVE' in os.environ: raise # for debugging: check
        if force_layout:
            self.layout()  # update the layout of self

    def _fix_notebook(self, pos, notebook_sizer, force_layout=True):
        """Replaces the widget at 'pos' with 'notebook_sizer':
        this is intended to be used by wxNotebook widgets, to add the notebook sizer to this sizer.
        This is a hack, but it's the best I could find without having to rewrite too much code :-("""
        # no error checking at all, this is a "protected" method, so it should
        # be safe to assume the caller knows how to use it
        item = self.widget.GetChildren()[pos]
        if item.IsWindow():
            w = item.GetWindow()
            w.SetContainingSizer(None)
        compat.SizerItem_SetSizer(item, notebook_sizer)
        if force_layout:
            self.layout()

    def set_item(self, pos, proportion=None, flag=None, border=None, size=None, force_layout=True):
        "Updates the layout of the item at the given pos"
        # XXX rename method to something more meaningful like 'set_item_layout' or 'update_layout' and option to proportion#
        # don't set the item properties; do this in the caller
        # XXX for editing, the next method is used now
        # XXX this is still used from the xml_builder and builder functions; maybe this can all be changed to the new implementation
        try:
            item = self.children[pos]
        except IndexError:  # this shouldn't happen
            self._logger.exception(_('Internal Error'))
            raise SystemExit
        if proportion is not None:
            proportion = int(proportion)
            item.proportion = proportion
        if flag is not None:
            flag = int(flag)
            item.flag = flag
        if border is not None:
            border = int(border)
            item.border = border
        if size is not None:
            item.size = size

        if not self.widget:
            return

        try:
            elem = self.widget.GetChildren()[pos]
        except IndexError:  # this may happen during xml loading
            return

        if proportion is not None:
            elem.SetProportion(proportion)
        if flag is not None:
            elem.SetFlag(flag)
        if border is not None:
            elem.SetBorder(border)
        if elem.IsWindow():
            if size is None:
                size = elem.GetSize()
            item = elem.GetWindow()
            w, h = size
            if w==-1 or h==-1:
                best_size = item.GetBestSize()
            if w == -1: w = best_size[0]
            if h == -1: h = best_size[1]
            self.widget.SetItemMinSize(item, w, h)

        if force_layout:
            self.layout(True)

    def item_layout_property_changed(self, pos, size=None, force_layout=True):
        # called when a sizer layout related property (border, option/proportion, size) has changed
        if not self.widget:
            return

        try:
            sizer_item = self.children[pos]  # the SizerItem
        except IndexError:  # this shouldn't happen
            self._logger.exception(_('Internal Error'))
            raise SystemExit
        
        item = sizer_item.item  # the ManagedBase or derived class instance

        try:
            elem = self.widget.GetChildren()[pos]
        except IndexError:  # this may happen during xml loading
            return

        # XXX track modifications; if no modifications, including SetItemMinSize
        if item.proportion != sizer_item.proportion or item.proportion != elem.GetProportion():
            sizer_item.proportion = item.proportion
            elem.SetProportion(sizer_item.proportion)
        if item.flag != sizer_item.flag or item.flag != elem.GetFlag():
            sizer_item.flag = item.flag
            elem.SetFlag(sizer_item.flag)
        if item.border != sizer_item.border or item.border != elem.GetBorder():
            sizer_item.border = item.border
            elem.SetBorder(sizer_item.border)

        if size is None:
            size = item.size

        if elem.IsWindow():
            if size is None:
                size = elem.GetSize()
            widget = elem.GetWindow()
            w, h = size
            if w==-1 or h==-1:
                best_size = widget.GetBestSize()
            if w == -1: w = best_size[0]
            if h == -1: h = best_size[1]
            self.widget.SetItemMinSize(widget, w, h)  # XXX store the min size as attribute to track modifications

        if force_layout:
            self.layout(True)

    def remove_item(self, elem, force_layout=True):
        "Removes elem from self"
        if elem:
            for c in self.children[elem.pos + 1:]:
                c.item.pos -= 1
            del self.children[elem.pos]
        if self.widget and elem.widget:
            self.widget.Detach(elem.pos)
            elem.sizer = None
            if force_layout:
                self.layout(True)
                # if not self.toplevel: self.sizer.Layout()

    def layout(self, recursive=True):
        # if not self.widget or not self.window.is_visible(): return
        if not self.widget:
            return

        from edit_windows import TopLevelBase

        if self.toplevel and not isinstance(self.window, TopLevelBase) and hasattr(self.window.sizer, 'widget'):
            if not self.window.properties['size'].is_active():
                szr = self.window.sizer.widget
                w, h = self.window.widget.GetBestSize()
                szr.SetItemMinSize(self.window.widget, w, h)
            if self.window.sizer is not self:
                self.window.sizer.layout(False)
            else:
                szr.Layout()
            return
        elif self.toplevel and isinstance(self.window, TopLevelBase):
            # self.window.widget.Layout()
            self.widget.Layout()
            evt = wx.SizeEvent( self.window.widget.GetSize(), self.window.widget.GetId() )
            wx.PostEvent(self.window.widget, evt)
            # don't change the size of the window
            if not misc.check_wx_version(2, 6, 0):
                self.widget.FitInside(self.window.widget)
            return
        self.widget.SetMinSize(self.widget.CalcMin())
        self.widget.Layout()
        for c in self.children:
            try:
                c.item.widget.Refresh()
            except:
                pass
        if recursive:
            if getattr(self, 'sizer', None) is not None:
                self.sizer.layout(recursive)

    def change_item_pos(self, item, new_pos, force_layout=True):
        "Changes the position of the 'item' so that it is at 'new_pos', which must be a valid position"
        if not self.widget:
            return

        old_pos = item.pos
        import copy

        new_item = copy.copy(self.children[old_pos])
        if old_pos > new_pos:
            for c in self.children[new_pos:old_pos]:
                c.item.update_pos(c.item.pos+1)
            self.children.insert(new_pos, new_item)
            del self.children[old_pos+1]
        else:
            for c in self.children[old_pos+1:new_pos+1]:
                c.item.update_pos(c.item.pos-1)
            del self.children[old_pos]
            # self.children.insert(new_pos+1, new_item)
            self.children.insert(new_pos, new_item)
        item.update_pos(new_pos)

        elem = self.widget.GetChildren()[old_pos]
        # always set the sizer to None because otherwise it will be Destroy'd
        elem.SetSizer(None)
        # this fake_win trick seems necessary because wxSizer::Remove(int pos) doesn't seem to work with grid sizers :-\
        fake_win = wx.Window(self.window.widget, -1)
        compat.SizerItem_SetWindow(elem, fake_win)
        self.widget.Remove(fake_win)
        fake_win.Destroy()
        self.widget.Insert( new_pos, item.widget, int(item.get_option()), item.flag, int(item.get_border()) )
        common.app_tree.change_node_pos(item.node, new_pos-1)
        common.app_tree.select_item(item.node)

        if force_layout:
            self.layout()
            if wx.Platform == '__WXMSW__':
                self.window.widget.Refresh()
                # self._logger.debug('%s', [c.item.name for c in self.children])

    def set_option(self, value):
        "If self is not a toplevel sizer, update the layout to reflect the value of the option property"
        value = int(value)
        #if value==self.option: return
        self.proportion = value
        try:
            #self.sizer.set_item(self.pos, option=self.option)
            self.sizer.item_layout_property_changed(self.pos)
            # self._logger.debug('%s set_option(%s)', self.name, self.option)
        except AttributeError:
            pass
        self.finish_set()

    def set_border(self, value):
        "If self is not a toplevel sizer, update the layout to reflect value of the border property"
        value = int(value)
        #if value==self.border: return
        self.border = value
        try:
            #self.sizer.set_item(self.pos, border=self.border)
            self.sizer.item_layout_property_changed(self.pos)
        except AttributeError:
            self._logger.exception(_('Internal Error'))

    def get_option(self):
        if not hasattr(self, 'sizer'):
            return '1'
        return str(self.proportion)

    def delete(self):
        "Destructor"
        self._destroy_popup_menu()
        if self._btn:
            self._btn.Destroy()
            self._btn = None
        for c in self.children:
            if c.item and isinstance(c.item, SizerSlot):
                c.item.delete()
        if self.toplevel:
            self.window.set_sizer(None)

    if wx.Platform == '__WXMSW__':
        def finish_set(self):
            for c in self.children:
                if c.item.widget:
                    try:
                        c.item.widget.Refresh()
                    except AttributeError:
                        pass  # sizers have no Refresh
    else:
        def finish_set(self):
            pass

    def refresh(self, *args):
        # this will be self.widget.Refresh
        for c in self.children:
            if c.item.widget:
                try:
                    c.item.widget.Refresh()
                except AttributeError:
                    pass

    def update_view(self, selected):
        if self._btn is None: return
        if selected:
            color = wx.RED
        else:
            color = compat.wx_SystemSettings_GetColour(wx.SYS_COLOUR_BTNFACE)
        self._btn.SetBackgroundColour(color)
        self._btn.Refresh(True)

    def add_slot(self, add_node=True, **kwds):
        "adds a slot to the sizer, i.e. a fake window that will accept the dropping of widgets"
        tmp = SizerSlot(self.window, self, len(self.children))
        item = SizerItem(tmp, len(self.children), 1, wx.EXPAND)
        self.children.append(item)
        
        if add_node:
            # insert node into tree
            tmp.node = node = SlotNode(tmp)
            common.app_tree.add(node, self.node, image=None)
        
        #if not self.widget:
            #return
        if self.widget:
            tmp.show_widget(True)  # create the actual SizerSlot widget
            self.widget.Add(tmp.widget, 1, wx.EXPAND)
            self.widget.SetItemMinSize(tmp.widget, 20, 20)
            force_layout = kwds.get('force_layout', True)
            if force_layout:
                self.layout(True)
        common.app_tree.app.saved = False

    def insert_slot(self, pos=None, force_layout=True):
        "Inserts a slot into the sizer at pos (1 based); optionally force layout update"
        # pos is 1 based here
        tmp = SizerSlot(self.window, self, pos)
        item = SizerItem(tmp, pos, 1, wx.EXPAND, 0)
        for c in self.children[pos:]: # self.children[0] is a Dummy
            c.item.pos += 1
        self.children.insert(pos, item)

        # insert node into tree
        tmp.node = node = SlotNode(tmp)
        common.app_tree.insert(node, self.node, pos-1, image=None)

        if self.widget:
            tmp.show_widget(True)  # create the actual SizerSlot
            self.widget.Insert(pos, tmp.widget, 1, wx.EXPAND)
            self.widget.SetItemMinSize(tmp.widget, 20, 20)
            if force_layout:
                self.layout(True)
        common.app_tree.app.saved = False

    def free_slot(self, pos, force_layout=True):
        "Replaces the element at pos with an empty slot"
        old_node = self.children[pos].item.node

        tmp = SizerSlot(self.window, self, pos)
        item = SizerItem(tmp, pos, 1, wx.EXPAND, 0)
        self.children[pos] = item

        # replace the node with a SlotNode
        tmp.node = node = SlotNode(tmp)
        common.app_tree.change_node( old_node, tmp, node )

        if self.widget:
            tmp.show_widget(True)  # create the actual SizerSlot as wx.Window with hatched background
            # pos is 1 based, Insert/Detach are 0 based, but item at 0 is the handle button
            self.widget.Insert(pos, tmp.widget, 1, wx.EXPAND)
            # detach is not needed here any more, as change_node does this already
            #self.widget.Detach(pos-1) # does only remove from sizer, but not destroy item
            if force_layout:
                self.layout()

    def is_visible(self):
        return self.window.is_visible()

    def clipboard_copy(self, event=None):
        "Store a widget copy into the clipboard, @see: L{clipboard.copy()}"
        clipboard.copy(self)

    def clipboard_cut(self, event=None):
        "Store a copy of self into the clipboard and delete the widget, @see: L{clipboard.cut()}"
        clipboard.cut(self)

    def post_load(self):
        """Called after loading of an app from a XML file, before showing the hierarchy of widget for the first time.
        This is used only for container widgets, to adjust their size appropriately."""
        if not self.toplevel:
            return
        if not self.window.properties['size'].is_active():
            self.fit_parent()
            w, h = self.widget.GetSize()
            prefix = ''
            if config.preferences.use_dialog_units:
                w, h = self.window.widget.ConvertPixelSizeToDialog( self.widget.GetSize() )
                prefix = 'd'
            self.window.set_size('%s, %s%s' % (w, h, prefix))



class wxGladeBoxSizer(wx.BoxSizer):
    def SetItemMinSize(self, item, w, h):
        if w==-1 or h==-1:
            try:
                w2, h2 = item.GetBestSize()
                if w == -1: w = w2
                if h == -1: h = h2
            except AttributeError:
                pass
        wx.BoxSizer.SetItemMinSize(self, item, w, h)



class BoxSizerBase(SizerBase):
    "orientation handling for BoxSizer and StaticBoxSizer"
    
    def __init__(self, name, window, orient=wx.VERTICAL, elements=0, toplevel=True, show=True):
        # elements: number of slots
        SizerBase.__init__(self, name, self.WX_CLASS, orient, window, toplevel, show)

        class Dummy(object):
            widget = None

        # add to self.children the SizerItem for self._btn
        self.children = [SizerItem(Dummy(), 0, 0, wx.EXPAND)]
        for i in range(1, elements + 1):
            self.add_slot()
            #tmp = SizerSlot(self.window, self, i) # XXX no node?
            #self.children.append(SizerItem(tmp, i, 1, wx.EXPAND))

    def get_class_orient(self):
        return '%s (%s)'%( self.WX_CLASS, self.properties["orient"].get_str_value() )

    def properties_changed(self, modified):
        if not modified or "orient" in modified and self.node:
            # update the image
            image = common.app_tree.get_image(self)
            common.app_tree.SetItemImage(self.node.item, image )

        SizerBase.properties_changed(self, modified)


class EditBoxSizer(BoxSizerBase):
    "Class to handle wxBoxSizer objects"
    WX_CLASS = "wxBoxSizer"

    def create_widget(self):
        self.widget = wxGladeBoxSizer(self.orient)
        self.widget.Add(self._btn, 0, wx.EXPAND)
        to_lay_out = []
        for c in self.children[1:]:  # we've already added self._btn
            c.item.show_widget(True)
            if isinstance(c.item, SizerSlot):
                self.widget.Add(c.item.widget, 1, wx.EXPAND)
                self.widget.SetItemMinSize(c.item.widget, 20, 20)
            else:
                sp = c.item.properties.get('size')
                if sp and sp.is_active():
                    if (c.proportion != 0 or (c.flag & wx.EXPAND)) and not (c.flag & wx.FIXED_MINSIZE):
                        c.item.widget.Layout()
                        w, h = c.item.widget.GetBestSize()
                        # self._logger.debug("HERE: %d, %d", w, h)
                        c.item.widget.SetMinSize((w, h))
                    else:
                        size = sp.get_value().strip()
                        if size[-1] == 'd':
                            size = size[:-1]
                            use_dialog_units = True
                        else:
                            use_dialog_units = False
                        w, h = [int(v) for v in size.split(',')]
                        if use_dialog_units:
                            w, h = wx.DLG_SZE(c.item.widget, (w, h))
                        # now re-set the item to update the size correctly...
                        to_lay_out.append((c.item.pos, (w, h)))
        for pos, size in to_lay_out:
            self.set_item(pos, size=size, force_layout=False)
        self.layout(True)
        if not self.toplevel and getattr(self, 'sizer'):
            # hasattr(self, 'sizer') is False only in case of a 'change_sizer' call
            self.sizer.add_item( self, self.pos, self.proportion, self.flag, self.border, self.widget.GetMinSize() )



class wxGladeStaticBoxSizer(wx.StaticBoxSizer):
    def SetItemMinSize(self, item, w, h):
        if w==-1 or h==-1:
            try:
                w2, h2 = item.GetBestSize()
                if w == -1: w = w2
                if h == -1: h = h2
            except AttributeError:
                pass
        wx.StaticBoxSizer.SetItemMinSize(self, item, w, h)



class EditStaticBoxSizer(BoxSizerBase):
    "Class to handle wxStaticBoxSizer objects"
    WX_CLASS = "wxStaticBoxSizer"
    EXTRA_PROPERTIES = ["label"]

    def __init__(self, name, window, orient=wx.VERTICAL, label='', elements=3, toplevel=True, show=True):
        BoxSizerBase.__init__(self, name, window, orient, elements, toplevel, show)
        self.label = np.TextProperty(label)

    def create_widget(self):
        self.widget = wxGladeStaticBoxSizer( wx.StaticBox(self.window.widget, -1, self.label), self.orient )
        self.widget.Add(self._btn, 0, wx.EXPAND)
        for c in self.children[1:]:  # we've already added self._btn
            c.item.show_widget(True)
            if isinstance(c.item, SizerSlot):
                self.widget.Add(c.item.widget, 1, wx.EXPAND)
                self.widget.SetItemMinSize(c.item.widget, 20, 20)
            else:
                sp = c.item.properties.get('size')
                if sp and sp.is_active() and ( c.proportion == 0 or not (c.flag & wx.EXPAND) ):
                    size = sp.get_value().strip()
                    if size[-1] == 'd':
                        size = size[:-1]
                        use_dialog_units = True
                    else:
                        use_dialog_units = False
                    w, h = [int(v) for v in size.split(',')]
                    if use_dialog_units:
                        w, h = wx.DLG_SZE(c.item.widget, (w, h))
                else:
                    w, h = c.item.widget.GetBestSize()
                self.widget.SetItemMinSize(c.item.widget, w, h)
        self.layout()
        if not self.toplevel and getattr(self, 'sizer'):
            # getattr(self, 'sizer') is False only in case of a 'change_sizer' call
            self.sizer.add_item(self, self.pos, self.proportion, self.flag, self.border, self.widget.GetMinSize())

    def set_label(self, value):
        "Sets the label of the static box"
        self.label = misc.wxstr(value)
        common.app_tree.refresh_name(self.node)
        if self.widget:
            self.widget.GetStaticBox().SetLabel(self.label)
        self.layout()

    def delete(self):
        if self.widget:
            self.widget.GetStaticBox().Destroy()
        SizerBase.delete(self)


class CustomSizer(wx.BoxSizer):
    "Custom wxSizer class used to implement a GridSizer with an additional handle button"

    def __init__(self, parent, factory, rows, cols, vgap, hgap):
        wx.BoxSizer.__init__(self, wx.VERTICAL)
        self.parent = parent
        self._grid = factory(rows, cols, vgap, hgap)
        wx.BoxSizer.Add(self, self.parent._btn, 0, wx.EXPAND)
        wx.BoxSizer.Add(self, self._grid, 1, wx.EXPAND)

    def __getattr__(self, name):
        return getattr(self._grid, name)

    def GetBestSize(self):
        return self._grid.GetMinSize()

    def Add(self, *args, **kwds):
        self._grid.Add(*args, **kwds)

    def Insert(self, pos, *args, **kwds):
        self._grid.Insert(pos - 1, *args, **kwds)

    def Remove(self, *args, **kwds):
        try:
            pos = int(args[0]) - 1
            self._grid.Remove(pos)
        except TypeError:
            self._grid.Remove(*args, **kwds)

    def RemovePos(self, pos):
        self._grid.Remove(pos - 1)

    def Detach(self, pos_or_obj):
        try:
            pos = int(pos_or_obj) - 1
            self._grid.Detach(pos)
        except TypeError:
            self._grid.Detach(pos_or_obj)

    def SetItemMinSize(self, item, w, h):
        try:
            w2, h2 = item.GetBestSize()
            if w == -1: w = w2
            if h == -1: h = h2
        except AttributeError:
            pass
        self._grid.SetItemMinSize(item, w, h)

    def GetChildren(self):
        return [None] + list(self._grid.GetChildren())

    def Layout(self):
        self._grid.Layout()
        wx.BoxSizer.Layout(self)



class GridSizerBase(SizerBase):
    "Base class for Grid sizers"
    _PROPERTY_HELP = {"rows":'Numbers of sizer rows',
                      "cols":'Numbers of sizer columns',
                      "vgap":'Vertical extra space between all children',
                      "hgap":'Horizontal extra space between all children'}

    EXTRA_PROPERTIES = ["Grid", "rows", "cols", "vgap", "hgap"]

    def __init__(self, name, klass, window, rows=3, cols=3, vgap=0, hgap=0, toplevel=True, show=True):
        #if self.cols or self.rows:
            #if not self.rows:
                #self.rows = 1
            #elif not self.cols:
                #self.cols = 1
        menu = [(_('Add slot'), self.add_slot),
                (_('Insert slot...'), self.insert_slot),
                (_('Add row'), self.add_row),
                (_('Add column'), self.add_col),
                (_('Insert row...'), self.insert_row),
                (_('Insert column...'), self.insert_col)]
        SizerBase.__init__(self, name, klass, None, window, toplevel, show, menu)

        self.rows = np.SpinProperty(rows)
        self.cols = np.SpinProperty(cols)
        self.vgap = np.SpinProperty(vgap)
        self.hgap = np.SpinProperty(hgap)

        class Dummy(object):
            widget = None

        # add to self.children the SizerItem for self._btn
        self.children = [SizerItem(Dummy(), 0, 0, wx.EXPAND)]
        for i in range(1, self.rows * self.cols + 1):
            tmp = SizerSlot(self.window, self, i) # XXX no node?
            self.children.append(SizerItem(tmp, i, 1, wx.EXPAND))


    def create_widget(self):
        "This must be overriden and called at the end of the overriden version"
        to_lay_out = []
        for c in self.children[1:]:  # we've already added self._btn
            c.item.show_widget(True)
            if isinstance(c.item, SizerSlot):
                self.widget.Add(c.item.widget, 1, wx.EXPAND)
                self.widget.SetItemMinSize(c.item.widget, 20, 20)
            else:
                sp = c.item.properties.get('size')
                if sp and sp.is_active():
                    if (c.proportion != 0 or (c.flag & wx.EXPAND)) and not (c.flag & wx.FIXED_MINSIZE):
                        c.item.widget.Layout()
                        w, h = c.item.widget.GetBestSize()
                        c.item.widget.SetMinSize((w, h))
                    else:
                        size = sp.get_value().strip()
                        if size[-1] == 'd':
                            size = size[:-1]
                            use_dialog_units = True
                        else:
                            use_dialog_units = False
                        w, h = [int(v) for v in size.split(',')]
                        if use_dialog_units:
                            w, h = wx.DLG_SZE(c.item.widget, (w, h))
                        # now re-set the item to update the size correctly...
                        to_lay_out.append((c.item.pos, (w, h)))

        for pos, size in to_lay_out:
            # self._logger.debug('set_item: %s, %s', pos, size)
            self.set_item(pos, size=size, force_layout=False)
        self.layout(True)

    def _set_rows_cols(self, rows, cols):
        self.rows = rows
        self.cols = cols
        self.properties['rows'].set_value(rows)
        self.properties['cols'].set_value(cols)
        if self.widget:
            self.widget.SetRows(self.rows)
            self.widget.SetCols(self.cols)
            self.layout(True)

    def set_rows(self, rows):
        self.rows = int(rows)
        if self.widget:
            self.widget.SetRows(self.rows)
            self.layout(True)

    def set_cols(self, cols):
        self.cols = int(cols)
        if self.widget:
            self.widget.SetCols(self.cols)
            self.layout(True)

    def set_hgap(self, hgap):
        self.hgap = int(hgap)
        if self.widget:
            self.widget.SetHGap(self.hgap)
            self.layout()

    def set_vgap(self, vgap):
        self.vgap = int(vgap)
        if self.widget:
            self.widget.SetVGap(self.vgap)
            self.layout()

    def fit_parent(self, *args):
        "Tell the sizer to resize the window to match the sizer's minimal size"
        if self.widget and self.window.widget:
            self.widget.Fit(self.window.widget)
            self.widget.SetSizeHints(self.window.widget)

    def insert_slot(self, pos=0, force_layout=True):
        "Inserts a slot into the sizer at 'pos'; optionally force a layout update"
        if not self.widget: return

        new_slot = SizerSlot(self.window, self, pos)
        for c in self.children[pos:]:
            c.item.pos += 1
        self.children.insert(pos, SizerItem(new_slot, pos, 1, wx.EXPAND, 0))
        new_slot.show_widget(True)  # create the actual SizerSlot
        self.widget.Insert(pos, new_slot.widget, 1, wx.EXPAND)
        self.widget.SetItemMinSize(new_slot.widget, 20, 20)
        if force_layout:
            self.layout(True)
        common.app_tree.app.saved = False

    def add_row(self, *args, **kwds):
        if not self.widget:
            return
        self._insert_row(compat.GridSizer_GetRows(self.widget) + 1)

    def insert_row(self, *args):
        if not self.widget:
            return
        dialog = InsertDialog(compat.GridSizer_GetRows(self.widget))
        if dialog.ShowModal() == wx.ID_OK:
            self._insert_row(dialog.pos + 1)
        dialog.Destroy()

    def _insert_row(self, pos):
        rows = compat.GridSizer_GetRows(self.widget)
        cols = compat.GridSizer_GetCols(self.widget)
        pos = (pos - 1) * cols + 1
        if pos >= len(self.children):
            # fix the out of bounds index...
            tot = len(self.children) - 1
            rows = tot // cols
            if tot % cols:
                rows += 1
            # self._logger.debug('fixed rows: %s', rows)
            if rows * cols > tot:
                for i in range(rows * cols - tot):
                    self.insert_slot( tot+i+1, force_layout=False )
            pos = rows * cols + 1
        self.set_rows(rows + 1)
        for i in range(cols):
            self.insert_slot( pos + i, force_layout=False )
        self.properties['rows'].set_value(self.rows)
        self.layout(True)
        common.app_tree.app.saved = False

    def add_col(self, *args, **kwds):
        if not self.widget:
            return
        self._insert_col( compat.GridSizer_GetCols(self.widget) + 1 )

    def insert_col(self, *args):
        if not self.widget:
            return
        dialog = InsertDialog(compat.GridSizer_GetCols(self.widget))
        if dialog.ShowModal() == wx.ID_OK:
            self._insert_col(dialog.pos + 1)
        dialog.Destroy()

    def _insert_col(self, pos):
        rows = compat.GridSizer_GetRows(self.widget)
        cols = compat.GridSizer_GetCols(self.widget)
        if pos >= len(self.children):
            # fix the out of bounds index...
            tot = len(self.children) - 1
            cols = tot / rows
            if tot % rows:
                cols += 1
            # self._logger.debug('fixed cols: %s', cols)
            if rows * cols > tot:
                for i in range(rows * cols - tot):
                    self.insert_slot( tot+i+1, force_layout=False)
            pos = rows*cols + 1
        self.set_cols(cols+1)
        for i in range(rows):
            self.insert_slot( pos + self.cols*i, force_layout=False )
        self.properties['cols'].set_value(self.cols)
        self.layout(True)
        common.app_tree.app.saved = False



class EditGridSizer(GridSizerBase):
    "Class to handle wxGridSizer objects"

    WX_CLASS = "wxGridSizer"
    def __init__(self, name, window, rows=3, cols=3, vgap=0, hgap=0, toplevel=True, show=True):
        GridSizerBase.__init__(self, name, 'wxGridSizer', window, rows, cols, vgap, hgap, toplevel, show)

    def create_widget(self):
        self.widget = CustomSizer(self, wx.GridSizer, self.rows, self.cols, self.vgap, self.hgap)
        if not self.toplevel and getattr(self, 'sizer', None):
            # getattr(self, 'sizer') is False only in case of a 'change_sizer' call
            self.sizer.add_item(self, self.pos, self.proportion, self.flag,  self.border) # , self.widget.GetMinSize())
        GridSizerBase.create_widget(self)



class CheckListDialogProperty(np.DialogProperty):
    dialog = [None]

    def __init__(self, owner, name, parent, title, message, callback, can_disable=True):
        self.title = title
        self.message = message
        if not self.dialog[0]:
            class Dialog(wx.Dialog):
                def __init__(self):
                    wx.Dialog.__init__(self, parent, -1, title)
                    sizer = wx.BoxSizer(wx.VERTICAL)
                    self.message = wx.StaticText(self, -1, "")
                    sizer.Add(self.message, 0, wx.TOP | wx.LEFT | wx.RIGHT | wx.EXPAND, 10)
                    self.choices = wx.CheckListBox(self, -1, choices=['dummy'])
                    sizer.Add(self.choices, 1, wx.EXPAND | wx.LEFT | wx.RIGHT, 10)
                    sizer.Add(wx.StaticLine(self, -1), 0, wx.EXPAND | wx.ALL, 10)
                    sz2 = wx.BoxSizer(wx.HORIZONTAL)
                    sz2.Add(wx.Button(self, wx.ID_OK, ""), 0, wx.ALL, 10)
                    sz2.Add(wx.Button(self, wx.ID_CANCEL, ""), 0, wx.ALL, 10)
                    sizer.Add(sz2, 0, wx.ALIGN_CENTER)
                    self.SetAutoLayout(True)
                    self.SetSizer(sizer)
                    sizer.Fit(self)
                    self.CenterOnScreen()

                def get_value(self):
                    ret = []
                    for c in range(self.choices.GetCount()):
                        if self.choices.IsChecked(c):
                            ret.append(str(c))
                    return ",".join(ret)

                def set_choices(self, values):
                    if wx.Platform != '__WXGTK__':
                        self.choices.Set(values)
                    else:
                        self.choices.Clear()
                        for v in values:
                            self.choices.Append(v)

                def set_descriptions(self, title, message):
                    self.SetTitle(title)
                    self.message.SetLabel(message)

            # end of class Dialog
            self.dialog[0] = Dialog()

        DialogProperty.__init__(self, owner, name, parent, self.dialog[0], can_disable, label=title)
        self.choices_setter = callback

    def display_dialog(self, event):
        self.set_choices(self.choices_setter())
        self.dialog.set_descriptions(self.title, self.message)
        DialogProperty.display_dialog(self, event)

    def set_choices(self, values):
        self.dialog.set_choices(values)



class EditFlexGridSizer(GridSizerBase):
    "Class to handle wxFlexGridSizer objects"
    WX_CLASS = "wxFlexGridSizer"

    EXTRA_PROPERTIES = GridSizerBase.EXTRA_PROPERTIES + ["growable_rows", "growable_cols"]
    _PROPERTY_HELP = {"growable_rows":'Select growable rows',
                      "growable_cols":'Select growable columns'}

    def __init__(self, name, window, rows=3, cols=3, vgap=0, hgap=0, toplevel=True, show=True):
        GridSizerBase.__init__(self, name, 'wxFlexGridSizer', window, rows, cols, vgap, hgap, toplevel, show)
        self.growable_rows = np.TextPropertyD("", default_value="")  # XXX modify CheckListDialog above and use this, with [] as value
        self.growable_cols = np.TextPropertyD("", default_value="")
        

    def create_widget(self):
        self.widget = CustomSizer(self, wx.FlexGridSizer, self.rows, self.cols, self.vgap, self.hgap)
        GridSizerBase.create_widget(self)
        for r in self.growable_rows.split(","):
            if not r.strip(): continue
            r = int(r)
            self.widget.AddGrowableRow(r)
        for c in self.growable_cols.split(","):
            if not c.strip(): continue
            c = int(c)
            self.widget.AddGrowableCol(c)
        if not self.toplevel and getattr(self, 'sizer', None) is not None:
            # hasattr(self, 'sizer') is False only in case of a 'change_sizer' call
            self.sizer.add_item(self, self.pos, self.proportion, self.flag, self.border)

    def _recreate(self):
        # recreatea the sizer by calling change_sizer
        if self.widget is None: return
        page = self.notebook.GetSelection()  if self.notebook  else 0
        wx.CallAfter(change_sizer, self, self.get_class_orient(), page)

    def set_growable_rows(self, value):
        try:
            self.grow_rows = [int(i) for i in value.split(',')]
        except:
            if not value.strip():
                self.grow_rows = []
            else:
                self.properties['growable_rows'].set_value( self.get_growable_rows() )
                return
        self._recreate()

    def set_growable_cols(self, value):
        try:
            self.grow_cols = [int(i) for i in value.split(',')]
        except:
            if not value.strip():
                self.grow_cols = []
            else:
                self.properties['growable_cols'].set_value( self.get_growable_cols() )
                return
        self._recreate()

    def get_growable_rows(self):
        return ','.join( str(r) for r in self.grow_rows )

    def get_growable_cols(self):
        return ','.join( str(c) for c in self.grow_cols )

    def _insert_row(self, pos):
        for row in self.grow_rows:
            if row >= pos-1:
                row += 1
        GridSizerBase._insert_row(self, pos)
        self.set_growable_rows(self.get_growable_rows())

    def _insert_col(self, pos):
        for column in self.grow_cols:
            if column >= pos-1:
                column += 1
        GridSizerBase._insert_col(self, pos)
        self.set_growable_cols(self.get_growable_cols())



def _builder(parent, sizer, pos, orientation=wx.VERTICAL, slots=1, is_static=False, label="", number=[1], show=True):
    num = slots
    name = 'sizer_%d' % number[0]
    while common.app_tree.has_name(name):
        number[0] += 1
        name = 'sizer_%d' % number[0]
    if sizer is not None:
        topl = False
    else:
        topl = True

    # add slots later
    if is_static:
        sz = EditStaticBoxSizer(name, parent, orientation, label, 0, topl)
    else:
        sz = EditBoxSizer(name, parent, orientation, 0, topl)


    if sizer is not None:
        sizer.add_item(sz, pos, 1, wx.EXPAND)
        node = Node(sz)
        sz.node = node
        common.app_tree.insert(node, sizer.node, pos-1)
        common.adding_sizer = False
    else:
        parent.set_sizer(sz)
        node = Node(sz)
        sz.node = node
        if pos is None:
            common.app_tree.add(node, parent.node)
        else:
            common.app_tree.insert(node, parent.node, pos-1)
            sz.pos = pos

    # add the slots
    for i in range(num):
        sz.add_slot()

    if parent.is_visible():
        sz.show_widget(show)
    if sizer is not None:
        sz.sizer_properties['flag'].set_value('wxEXPAND')
        sz.sizer_properties['pos'].set_value(pos-1)


class _SizerDialog(wx.Dialog):
    def __init__(self, parent):
        wx.Dialog.__init__( self, misc.get_toplevel_parent(parent), -1, _('Select sizer type') )
        self.orientation = wx.RadioBox( self, -1, _('Orientation'), choices=[_('Horizontal'), _('Vertical')] )
        self.orientation.SetSelection(0)
        tmp = wx.BoxSizer(wx.HORIZONTAL)
        tmp.Add( wx.StaticText(self, -1, _('Slots: ')), 0, wx.ALL | wx.ALIGN_CENTER_VERTICAL, 3 )
        self.num = wx.SpinCtrl(self, -1)
        self.num.SetRange(1, 100)
        self.num.SetValue(1)
        tmp.Add(self.num, 1, wx.ALL, 3)
        szr = wx.BoxSizer(wx.VERTICAL)
        szr.Add(self.orientation, 0, wx.ALL | wx.EXPAND, 4)
        szr.Add(tmp, 0, wx.EXPAND)
        CHECK_ID = wx.NewId()
        self.check = wx.CheckBox(self, CHECK_ID, _('Has a Static Box'))
        self.label = wx.TextCtrl(self, -1, "")
        self.label.Enable(False)
        wx.EVT_CHECKBOX(self, CHECK_ID, self.on_check_statbox)
        szr.Add(self.check, 0, wx.ALL | wx.EXPAND, 4)
        tmp = wx.BoxSizer(wx.HORIZONTAL)
        tmp.Add(wx.StaticText(self, -1, _("Label: ")), 0, wx.ALIGN_CENTER)
        tmp.Add(self.label, 1)
        szr.Add(tmp, 0, wx.ALL | wx.EXPAND, 4)

        btn = wx.Button(self, wx.ID_OK, _('OK'))
        btn.SetDefault()
        szr.Add(btn, 0, wx.ALL | wx.ALIGN_CENTER, 10)
        self.SetAutoLayout(1)
        self.SetSizer(szr)
        szr.Fit(self)
        self.Layout()
        self.CenterOnScreen()

    def reset(self):
        self.orientation.SetSelection(0)
        self.num.SetValue(1)
        self.check.SetValue(0)
        self.label.SetValue("")
        self.label.Enable(False)

    def on_check_statbox(self, event):
        self.label.Enable(event.IsChecked())



def builder(parent, sizer, pos, number=[1], show=True):
    "factory function for box sizers"

    dialog = _SizerDialog(parent)
    dialog.ShowModal()
    if dialog.orientation.GetStringSelection() == _('Horizontal'):
        orientation = wx.HORIZONTAL
    else:
        orientation = wx.VERTICAL
    num = dialog.num.GetValue()

    _builder (parent, sizer, pos, orientation, num, dialog.check.GetValue(), dialog.label.GetValue() )

    dialog.Destroy()


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    "factory function to build EditBoxSizer objects from a XML file"
    from xml_parse import XmlParsingError

    try:
        name = attrs['name']
    except KeyError:
        raise XmlParsingError( _("'name' attribute missing") )
    orientation = wx.VERTICAL  # default value
    topl = True  if sizer is None  else False
    if attrs['base'] == 'EditStaticBoxSizer':
        sz = EditStaticBoxSizer(name, parent, orientation, '', 0, topl)
    else:
        sz = EditBoxSizer(name, parent, orientation, 0, topl)
    if sizer is not None:
        if sizeritem is None:
            raise XmlParsingError( _("'sizeritem' object not found") )
        sizer.add_item( sz, pos=pos, proportion=sizeritem.proportion, flag=sizeritem.flag, border=sizeritem.border )
        node = Node(sz)
        sz.node = node
        if pos is None:
            common.app_tree.add(node, sizer.node)
        else:
            common.app_tree.insert(node, sizer.node, pos-1)
    else:
        parent.set_sizer(sz)
        node = Node(sz)
        sz.node = node
        common.app_tree.add(node, parent.node)
    return sz


class _GridBuilderDialog(wx.Dialog):
    def __init__(self, parent):
        wx.Dialog.__init__( self, misc.get_toplevel_parent(parent), -1, _('Select sizer attributes') )
        import widget_properties as wp
        self.rows = wp.SpinProperty(self, 'rows', self, label=_("rows"))
        self.cols = wp.SpinProperty(self, 'cols', self, label=_("cols"))
        self.vgap = wp.SpinProperty(self, 'vgap', self, label=_("vgap"))
        self.hgap = wp.SpinProperty(self, 'hgap', self, label=_("hgap"))
        self.rows.set_tooltip(_('Numbers of sizer rows'))
        self.cols.set_tooltip(_('Numbers of sizer columns'))
        self.vgap.set_tooltip(_('Vertical extra space between all children'))
        self.hgap.set_tooltip(_('Horizontal extra space between all children'))
        self.rows.spin.SetFocus()
        self.rows.spin.SetSelection(-1, -1)

        self.flex = wp.CheckBoxProperty( self, 'flex', self, _('Flexible'), write_always=True )
        self.flex.set_tooltip(_('Create a wxFlexGridSizer instead of a wxGridSizer'))

        self.rows.set_value(3)
        self.cols.set_value(3)
        self.vgap.set_value(0)
        self.hgap.set_value(0)

        sizer = wx.BoxSizer(wx.VERTICAL)
        sizer.Add(self.rows.panel, 0, wx.LEFT | wx.RIGHT | wx.TOP | wx.EXPAND, 10)
        sizer.Add(self.cols.panel, 0, wx.LEFT | wx.RIGHT | wx.EXPAND, 10)
        sizer.Add(self.vgap.panel, 0, wx.LEFT | wx.RIGHT | wx.EXPAND, 10)
        sizer.Add(self.hgap.panel, 0, wx.LEFT | wx.RIGHT | wx.EXPAND, 10)
        sizer.Add(self.flex.panel, 0, wx.LEFT | wx.RIGHT | wx.EXPAND, 10)
        szr = wx.BoxSizer(wx.HORIZONTAL)
        btn = wx.Button(self, wx.ID_OK, _('OK'))
        btn.SetDefault()
        szr.Add(btn)
        sizer.Add(szr, 0, wx.ALL | wx.ALIGN_CENTER, 10)
        self.SetAutoLayout(True)
        self.SetSizer(sizer)
        sizer.Fit(self)
        self.Layout()
        self.CentreOnParent()

    def __getitem__(self, name):
        return lambda: 0, lambda v: None


def grid_builder(parent, sizer, pos, number=[1], show=True):
    "factory function for grid sizers"
    dialog = _GridBuilderDialog(parent)
    dialog.ShowModal()
    rows = int(dialog.rows.get_value())
    cols = int(dialog.cols.get_value())
    vgap = int(dialog.vgap.get_value())
    hgap = int(dialog.hgap.get_value())
    is_flexible = dialog.flex.get_value()
    dialog.Destroy()

    name = 'grid_sizer_%d' % number[0]
    while common.app_tree.has_name(name):
        number[0] += 1
        name = 'grid_sizer_%d' % number[0]
    is_toplevel = True
    if is_flexible:
        constructor = EditFlexGridSizer
    else:
        constructor = EditGridSizer
    if sizer is not None:
        is_toplevel = False
    #sz = constructor(name, parent, rows, cols, vgap, hgap, is_toplevel)
    sz = constructor(name, parent, 0, cols, vgap, hgap, is_toplevel) # add slots later
    if sizer is not None:
        sizer.add_item(sz, pos, 1, wx.EXPAND)
        node = Node(sz)
        sz.node = node
        common.app_tree.insert(node, sizer.node, pos-1)
        common.adding_sizer = False
    else:
        parent.set_sizer(sz)
        node = Node(sz)
        sz.node = node
        if pos is None:
            common.app_tree.add(node, parent.node)
        else:
            common.app_tree.insert(node, parent.node, pos-1)
            sz.pos = pos

    # add the slots
    for i in range(rows*cols):
        sz.add_slot()

    if parent.is_visible():
        sz.show_widget(show)  # True)

    if sizer is not None:
        sz.sizer_properties['flag'].set_value('wxEXPAND')
        sz.sizer_properties['pos'].set_value(pos - 1)


def grid_xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    "factory function to build EditGridSizer objects from a XML file"
    from xml_parse import XmlParsingError

    try:
        name = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    if attrs['base'] == 'EditGridSizer':
        constructor = EditGridSizer
    else:
        constructor = EditFlexGridSizer
    if sizer is not None:
        sz = constructor(name, parent, rows=0, cols=0, toplevel=False)
        if sizeritem is None:
            raise XmlParsingError(_("'sizeritem' object not found"))
        sizer.add_item(sz, pos=pos, proportion=sizeritem.proportion, flag=sizeritem.flag, border=sizeritem.border)
        node = Node(sz)
        sz.node = node
        if pos is None:
            common.app_tree.add(node, sizer.node)
        else:
            common.app_tree.insert(node, sizer.node, pos-1)
    else:
        sz = constructor(name, parent, rows=0, cols=0, toplevel=True)
        parent.set_sizer(sz)
        node = Node(sz)
        sz.node = node
        common.app_tree.add(node, parent.node)
    return sz


def init_all():
    "Module initialization function: returns a list of buttons for the main palette to add the various sizers"
    cw = common.widgets
    cw['EditBoxSizer'] = builder
    cw['EditGridSizer'] = grid_builder

    cwx = common.widgets_from_xml
    cwx['EditBoxSizer'] = xml_builder
    cwx['EditStaticBoxSizer'] = xml_builder
    cwx['EditGridSizer'] = grid_xml_builder
    cwx['EditFlexGridSizer'] = grid_xml_builder

    import os.path

    WidgetTree.images['EditStaticBoxSizer'] = os.path.join( config.icons_path, 'sizer.xpm')
    WidgetTree.images['EditFlexGridSizer']  = os.path.join( config.icons_path, 'grid_sizer.xpm' )
    
    WidgetTree.images['EditVerticalSizer']   = os.path.join( config.icons_path, 'sizer_v.xpm' )
    WidgetTree.images['EditHorizontalSizer'] = os.path.join( config.icons_path, 'sizer_h.xpm' )
    
    WidgetTree.images['EditVerticalSizerSlot']   = os.path.join( config.icons_path, 'sizer_slot_v.xpm' )
    WidgetTree.images['EditHorizontalSizerSlot'] = os.path.join( config.icons_path, 'sizer_slot_h.xpm' )
    WidgetTree.images['EditSizerSlot'] = os.path.join( config.icons_path, 'sizer_slot.xpm' )

    ret = {'Sizers': [
        common.make_object_button('EditBoxSizer', 'sizer.xpm'),
        common.make_object_button('EditGridSizer', 'grid_sizer.xpm'),
    ]}
    return ret
