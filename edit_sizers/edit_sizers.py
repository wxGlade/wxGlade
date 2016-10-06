"""
Hierarchy of Sizers supported by wxGlade

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import logging
import wx
from wx.lib.buttons import GenButton

import new_properties as np
from tree import WidgetTree, Node, SlotNode
import clipboard
import common, compat, config, misc


def _grid_row_col(pos, cols):
    "calculate the 0-based row and column for an item at pos (1-based)"
    row = pos // cols
    col = pos%cols - 1
    if pos%cols == 0:
        # last col
        row -= 1
        col = cols-1
    return (row, col)


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
            return [], [], []  # init, props, layout

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

        return init, [], layout  # init, props, layout

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
    def __init__(self, parent, sizer, pos=0, label=None):
        np.PropertyOwner.__init__(self)
        # initialise instance logger
        self._logger = logging.getLogger(self.__class__.__name__)
        self.klass = "SLOT"
        self.label = label

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
        self.widget.SetBackgroundStyle(wx.BG_STYLE_CUSTOM)
        self.widget.SetAutoLayout(True)
        self.widget.Bind(wx.EVT_PAINT, self.on_paint)
        self.widget.Bind(wx.EVT_ERASE_BACKGROUND, self.on_erase_background)
        self.widget.Bind(wx.EVT_RIGHT_DOWN, self.popup_menu)
        self.widget.Bind(wx.EVT_LEFT_DOWN, self.on_drop_widget)
        self.widget.Bind(wx.EVT_MIDDLE_DOWN, misc.exec_after(self.on_select_and_paste))
        self.widget.Bind(wx.EVT_ENTER_WINDOW, self.on_enter)
        self.widget.Bind(wx.EVT_LEAVE_WINDOW, self.on_leave)
        self.widget.Bind(wx.EVT_KEY_DOWN, misc.on_key_down_event)

    def is_visible(self):
        return False

    def create(self):
        if self.widget: return
        self.create_widget()

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
        self._draw_background(dc)

    def on_erase_background(self, event):
        dc = event.GetDC()
        if not dc:
            dc = wx.ClientDC(self)
            rect = self.widget.GetUpdateRegion().GetBox()
            dc.SetClippingRect(rect)
        self._draw_background(dc, clear=False)

    def _draw_background(self, dc, clear=True):
        "draw the hatches on device context dc (red if selected)"
        # fill background first; propably needed only on MSW and not for on_erase_background
        if clear:
            dc.SetBackground(wx.Brush(wx.LIGHT_GREY))
            dc.Clear()
        color = wx.BLUE  if misc.focused_widget is self  else  wx.BLACK
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
    def popup_menu(self, event, pos=None):
        event_widget = event.GetEventObject()
        menu = self._create_popup_menu(widget=event_widget)
        if pos is None:
            # convert relative event position to relative widget position
            event_pos  = event.GetPosition()
            screen_pos = event_widget.ClientToScreen(event_pos)
            pos        = event_widget.ScreenToClient(screen_pos)
        event_widget.PopupMenu(menu, pos)

    def _create_popup_menu(self, widget):
        self._destroy_popup_menu()
        
        # menu title
        if isinstance(self.sizer, GridSizerBase):
            rows = self.sizer.rows
            cols = self.sizer.cols
            # calculate row and pos of our slot
            row,col = _grid_row_col(self.pos, cols)
            menu = wx.Menu(_("Slot %d/%d"%(row+1,col+1)))
        else:
            menu = wx.Menu(_("Slot %d"%self.pos))

        # edit: paste
        i = misc.append_menu_item(menu, -1, _('Paste\tCtrl+V'), wx.ART_PASTE)
        misc.bind_menu_item_after(widget, i, self.clipboard_paste)
        menu.AppendSeparator()


        # slot actions
        if not self.sizer.is_virtual():
            # we can add/remove items only from non-virtual sizers
            i = misc.append_menu_item(menu, -1, _('Remove Slot\tDel'), wx.ART_DELETE)
            misc.bind_menu_item_after(widget, i, self.remove)
            if len(self.sizer.children)<=2: i.Enable(False)

            # if inside a grid sizer: allow removal of empty rows/cols
            if isinstance(self.sizer, GridSizerBase):
                # check whether all slots in same row/col are empty
                row_is_empty = col_is_empty = True
                for pos,child in enumerate(self.sizer.children):
                    if pos==0: continue
                    child_row, child_col = _grid_row_col(pos, cols)
                    if child_row==row and not isinstance(child.item, SizerSlot):
                        row_is_empty = False
                    if child_col==col and not isinstance(child.item, SizerSlot):
                        col_is_empty = False

                # allow removal of empty row
                i = misc.append_menu_item(menu, -1, _('Remove Row %d'%(row+1)) )
                misc.bind_menu_item_after(widget, i, self.sizer.remove_row, self.pos)
                if not row_is_empty or rows<=1: i.Enable(False)

                # allow removal of empty col
                i = misc.append_menu_item(menu, -1, _('Remove Column %d'%(col+1)) )
                misc.bind_menu_item_after(widget, i, self.sizer.remove_col, self.pos)
                if not col_is_empty or cols<=1: i.Enable(False)
                menu.AppendSeparator()

            # for all sizers: insert/add slots
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
            p.preview()

    ####################################################################################################################

    def remove(self, *args):
        self._destroy_popup_menu()
        if self.sizer.is_virtual() or len(self.sizer.children)<=2: return

        sizer = self.sizer
        node = self.sizer.children[self.pos].item.node
        self.sizer.remove_item(self)
        self.delete()
        common.app_tree.remove(node)
        # set focused widget
        pos = (self.pos - 1) or 1
        if pos >= len(sizer.children):
            pos -= 1  # the deleted slot was the last one; the check above ensures that at least one more slot is left
        misc.set_focused_widget(sizer.children[pos].item)

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
        if self.widget: self.widget.Hide()
        if clipboard.paste(self.parent, self.sizer, self.pos, clipboard_data):
            common.app_tree.app.saved = False
        else:
            if self.widget: self.widget.Show()

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
        if isinstance(item, np.PropertyOwner):
            self.item.properties["pos"].set(pos)
        else:
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

    # construct without children, take then the children from the old sizer
    szr = constructors[new]()
    szr.children.extend(old.children[1:])
    szr.node = old.node

    if isinstance(szr, GridSizerBase):
        # take rows, cols, hgap, vgap from old sizer, if applicable
        szr.properties["rows"].set( getattr(old, "rows", 1) )
        szr.properties["cols"].set( getattr(old, "cols", len(szr.children)-1) )
        szr.properties["hgap"].set( getattr(old, "hgap", 0) )
        szr.properties["vgap"].set( getattr(old, "vgap", 0) )
        szr.properties_changed( ["rows","cols","hgap","vgap"] )

    if isinstance(szr, EditFlexGridSizer) and isinstance(old, EditFlexGridSizer):
        # take growable rows and cols from old sizer
        grow_r_p = old.properties["growable_rows"]
        grow_c_p = old.properties["growable_cols"]
        if grow_r_p.is_active():
            szr.properties['growable_rows'].value = grow_r_p.value
            szr.properties['growable_rows'].deactivated = False
        if grow_c_p.is_active():
            szr.properties['growable_cols'].value = grow_c_p.value
            szr.properties['growable_cols'].deactivated = False
    # XXX keep rows, cols, growable_rows, growable_cols in attributes of new sizer if it's not a (Flex)GridSizer
    #     and re-use them if user switches back

    global _change_sizer_panel
    if old.widget is not None:
        for c in old.widget.GetChildren():
            if c and c.IsSizer():
                compat.SizerItem_SetSizer(c, None)
        old.widget.Clear()  # without deleting window items; but sets the sizer of the windows to NULL

        szr.create(dont_set=True)
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

    if not szr.toplevel:
        szr.sizer = old.sizer
        szr.properties["proportion"].set(old.proportion)
        szr.properties["flag"].set(old.flag)
        szr.properties["border"].set(old.border)
        szr.properties["pos"].set(old.pos)
        szr.sizer.children[szr.pos].item = szr
        if szr.sizer.widget:
            elem = szr.sizer.widget.GetChildren()[szr.pos]
            compat.SizerItem_SetSizer(elem, szr.widget)

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
        "Return true if sizer is virtual (e.g. SplitterWindowSizer or NotebookSizer)"
        return False

    def is_fixed(self):
        "Return True if sizer has a fixed number of slots (e.g. SplitterWindowSizer)"

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
    _PROPERTY_HELP = {"fit":'Sizes the window so that it fits around its subwindows',
                      "attribute":'Store instance as attribute of window class; e.g. self.sizer_1 = wx.BoxSizer(...)\n'
                                  'Without this, you can not access the sizer from your program'}

    def __init__(self, name, klass, orient, window, toplevel=True):
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

    def create(self, dont_set=False):
        if self.widget: return  # nothing to do if the sizer has already been created
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

    def on_selection(self, event):
        # button clicked -> set ourself as current widget
        misc.set_focused_widget(self)

    def set_containing_sizer(self, sizer):
        self.sizer = sizer
        self.properties['pos'].set_sizer(sizer)

    def set_pos(self, value):
        # XXX currently not used; make np.LayoutPosProperty editable again
        wx.CallAfter( self.sizer.change_item_pos, self, min( value+1, len(self.sizer.children)-1 ) )

    def update_pos(self, value):
        # XXX currently not used; make np.LayoutPosProperty editable again
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
            if misc.focused_widget is self: misc.set_focused_widget(None)
            wx.CallAfter(change_sizer, self, value)
        if (not modified or "flag" in modified or "proportion" in modified or "border" in modified) and self.widget:
            if not self.toplevel:
                self.sizer.set_item(self.pos, self.proportion, self.flag, self.border)
        np.PropertyOwner.properties_changed(self, modified)

    def check_compatibility(self, widget):
        return "AddSlot" # a slot is to be added before inserting/pasting

    # popup menu #######################################################################################################
    def popup_menu(self, event, pos=None):
        "pops up a menu to add or remove slots from self, or to remove self from the application."
        event_widget = event.GetEventObject()
        menu = self._create_popup_menu(widget=event_widget)
        if pos is None:
            # convert relative event position to relative widget position
            event_pos  = event.GetPosition()
            screen_pos = event_widget.ClientToScreen(event_pos)
            pos        = event_widget.ScreenToClient(screen_pos)
        event_widget.PopupMenu(menu, pos)

    def _create_popup_menu(self, widget):
        # provide popup menu for removal
        self._destroy_popup_menu()
        menu = misc.wxGladePopupMenu(self.name)

        widgetclass = self.__class__.__name__.lstrip("Edit")
        i = misc.append_menu_item(menu, -1, _('Remove %s\tDel')%widgetclass, wx.ART_DELETE)
        misc.bind_menu_item_after(widget, i, self._remove)

        if not self.toplevel and self.sizer:
            i = misc.append_menu_item( menu, -1, _('Insert slot before\tCtrl+I') )
            misc.bind_menu_item_after(widget, i, self.sizer.insert_slot, self.pos)
            menu.AppendSeparator()
        # other menu items: add/insert slot, copy, cut
        i = misc.append_menu_item( menu, -1, _('Add slot\tCtrl+A') )
        misc.bind_menu_item_after(widget, i, self.add_slot)

        if "cols" in self.PROPERTIES:  # a grid sizer
            i = misc.append_menu_item( menu, -1, _('Add row') )
            misc.bind_menu_item_after(widget, i, self.insert_row, -1)
            i = misc.append_menu_item( menu, -1, _('Add column') )
            misc.bind_menu_item_after(widget, i, self.insert_col, -1)
            menu.AppendSeparator()

        i = misc.append_menu_item( menu, -1, _('Copy\tCtrl+C'), wx.ART_COPY )
        misc.bind_menu_item_after(widget, i, self.clipboard_copy)
        i = misc.append_menu_item( menu, -1, _('Cut\tCtrl+X'), wx.ART_CUT )
        misc.bind_menu_item_after(widget, i, self.clipboard_cut)

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
        # XXX as of now: remove old and then create a new slot; maybe there's a better way to change the node directly
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
        # context menu callback
        self._destroy_popup_menu()
        p = misc.get_toplevel_widget(self)
        p.preview()

    def fit_parent(self, *args):
        "Tell the sizer to resize the window to match the sizer's minimal size"
        if self.widget and self.window.widget:
            self.widget.Fit(self.window.widget)
            # self.widget.SetSizeHints(self.window.widget)
            self.window.widget.Layout()

    def add_item(self, item, pos=None, proportion=0, flag=0, border=0, size=None, force_layout=True):
        "Adds an item to self."
        # called from ManagedBase.__init__ when adding an item to the end from XML parser
        # or interactively when adding an item to an empty sizer slot
        proportion = int(proportion)
        flag = int(flag)
        border = int(border)
        if pos is None:
            pos = len(self.children)
        if pos==len(self.children):
            self.children.append(None)
        else:
            old_child = self.children[pos]
            if isinstance(old_child.item, SizerSlot):
                old_child.item.delete()
        if "rows" in self.PROPERTIES: self._adjust_rows_cols()  # for GridSizer
        self.children[pos] = SizerItem(item, pos, proportion, flag, border, size)

        if hasattr(item, 'set_containing_sizer'):
            item.set_containing_sizer(self)
        else:
            item.sizer = self
        item.properties["pos"].set(pos)

        self._add_item_widget(item, pos, proportion, flag, border, size, force_layout)

    def _add_item_widget(self, item, pos, proportion, flag, border, size, force_layout):
        # add item.widget to self.widget, i.e. the wxWidget to the wxSizer
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
        # called e.g. from context menu of SizerSlot
        if elem:
            for c in self.children[elem.pos + 1:]:
                c.item.properties["pos"].value -= 1
            del self.children[elem.pos]
        if "rows" in self.PROPERTIES: self._adjust_rows_cols()  # for GridSizer
        if self.widget and elem.widget:
            self.widget.Detach(elem.pos)
            elem.sizer = None
            if force_layout:
                self.layout(True)
                # if not self.toplevel: self.sizer.Layout()

    def layout(self, recursive=True):
        # update slot labels in tree view
        for c in self.children:
            if isinstance(c.item, SizerSlot):
                common.app_tree.refresh_name( c.item.node )

        if not self.widget:
            return

        #from edit_windows import TopLevelBase

        # layout
        #if self.toplevel and not isinstance(self.window, TopLevelBase) and hasattr(self.window.sizer, 'widget'):
        if self.toplevel and not self.window._is_toplevel and hasattr(self.window.sizer, 'widget'):
            if not self.window.properties['size'].is_active():
                szr = self.window.sizer.widget
                w, h = self.window.widget.GetBestSize()
                szr.SetItemMinSize(self.window.widget, w, h)
            if self.window.sizer is not self:
                self.window.sizer.layout(False)
            else:
                szr.Layout()
            return
        #elif self.toplevel and isinstance(self.window, TopLevelBase):
        elif self.toplevel and self.window._is_toplevel:
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
        def finish_set(self):  # previously called after self.set_option(...)
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

    # add/insert/free slots; interface mainly from context menus #######################################################
    def _add_slot(self):
        "adds an empty slot to the sizer, i.e. a fake window that will accept the dropping of widgets"
        # called from "add slot" context menu handler of sizer
        # called from XML parser for adding empty 'sizerslot': sizer.add_slot()
        tmp = SizerSlot(self.window, self, len(self.children))
        item = SizerItem(tmp, len(self.children), 1, wx.EXPAND)
        self.children.append(item)
        if "rows" in self.PROPERTIES: self._adjust_rows_cols()  # for GridSizer

        # insert node into tree
        tmp.node = node = SlotNode(tmp)
        common.app_tree.add(node, self.node)

        if self.widget:
            tmp.create()  # create the actual SizerSlot widget
            self.widget.Add(tmp.widget, 1, wx.EXPAND)
            self.widget.SetItemMinSize(tmp.widget, 20, 20)

    def _insert_slot(self, pos=None):
        "Inserts an empty slot into the sizer at pos (1 based); optionally force layout update"
        # called from context menu handler; multiple times if applicable; layout will be called there
        # also called from SizerBase._remove after a sizer has removed itself and inserts an empty slot instead
        # pos is 1 based here
        tmp = SizerSlot(self.window, self, pos)
        item = SizerItem(tmp, pos, 1, wx.EXPAND, 0)
        for c in self.children[pos:]: # self.children[0] is a Dummy
            c.item.properties["pos"].value += 1
        self.children.insert(pos, item)
        if "rows" in self.PROPERTIES: self._adjust_rows_cols()  # for GridSizer

        # insert node into tree
        tmp.node = node = SlotNode(tmp)
        common.app_tree.insert(node, self.node, pos-1)
        for c in self.children:
            if isinstance(c.item, SizerSlot):
                common.app_tree.refresh_name( c.item.node )

        if self.widget:
            tmp.create()  # create the actual SizerSlot
            self.widget.Insert(pos, tmp.widget, 1, wx.EXPAND)
            self.widget.SetItemMinSize(tmp.widget, 20, 20)

    # insert/add slot callbacks for context menus ######################################################################
    def _ask_count(self, insert=True):
        # helper for next method (insertion/adding of multiple slots)
        choices = [str(n) for n in range(1,11)]
        if insert:
            dlg = wx.SingleChoiceDialog(None, "Select number of slots to be inserted", "Insert Slots", choices)
        else:
            dlg = wx.SingleChoiceDialog(None, "Select number of slots to be added", "Add Slots", choices)
        ret = 0  if dlg.ShowModal()==wx.ID_CANCEL  else   int(dlg.GetStringSelection())
        dlg.Destroy()
        return ret

    def insert_slot(self, pos, multiple=False):
        # insert before current
        count = self._ask_count() if multiple else 1
        for n in range(count):
            self._insert_slot(pos)
        if self.widget: self.layout(True)
        common.app_tree.app.saved = False

    def add_slot(self, multiple=False):
        # add to the end
        count = self._ask_count(insert=False) if multiple else 1
        for n in range(count):
            self._add_slot()
        if self.widget: self.layout()
        common.app_tree.app.saved = False

    def free_slot(self, pos, force_layout=True):
        "Replaces the element at pos with an empty slot"
        # called from ManagedBase context menu when removing an item
        old_node = self.children[pos].item.node

        tmp = SizerSlot(self.window, self, pos)
        item = SizerItem(tmp, pos, 1, wx.EXPAND, 0)
        self.children[pos] = item

        # replace the node with a SlotNode
        tmp.node = node = SlotNode(tmp)
        common.app_tree.change_node( old_node, tmp, node )

        if self.widget:
            tmp.create()  # create the actual SizerSlot as wx.Window with hatched background
            # pos is 1 based, Insert/Detach are 0 based, but item at 0 is the handle button
            self.widget.Insert(pos, tmp.widget, 1, wx.EXPAND)
            # detach is not needed here any more, as change_node does this already
            #self.widget.Detach(pos-1) # does only remove from sizer, but not destroy item
            if force_layout:
                self.layout()
    ####################################################################################################################
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
        if "rows" in self.PROPERTIES: self._adjust_rows_cols()  # for GridSizer
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


class Dummy(object):
    "dummy item for SizerItem that contains the sizer handle button sizer._btn"
    widget = None


class BoxSizerBase(SizerBase):
    "orientation handling for BoxSizer and StaticBoxSizer"

    def __init__(self, name, window, orient=wx.VERTICAL, elements=0, toplevel=True):
        # elements: number of slots
        SizerBase.__init__(self, name, self.WX_CLASS, orient, window, toplevel)

        self.children = [SizerItem(Dummy(), 0, 0, wx.EXPAND)]  # add to self.children the SizerItem for self._btn
        for i in range(1, elements + 1):
            self._add_slot()
        self.layout()

    def get_class_orient(self):
        return '%s (%s)'%( self.WX_CLASS, self.properties["orient"].get_str_value() )

    def properties_changed(self, modified):
        if not modified or "orient" in modified and self.node:
            # update the image
            common.app_tree.SetItemImage(self.node.item, self.node.get_image() )

        SizerBase.properties_changed(self, modified)


class EditBoxSizer(BoxSizerBase):
    "Class to handle wxBoxSizer objects"
    WX_CLASS = "wxBoxSizer"

    def create_widget(self):
        self.widget = wxGladeBoxSizer(self.orient)
        self.widget.Add(self._btn, 0, wx.EXPAND)
        to_lay_out = []
        for c in self.children[1:]:  # we've already added self._btn
            c.item.create()
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

    def __init__(self, name, window, orient=wx.VERTICAL, label='', elements=3, toplevel=True):
        BoxSizerBase.__init__(self, name, window, orient, elements, toplevel)
        self.label = np.TextProperty(label)

    def create_widget(self):
        self.widget = wxGladeStaticBoxSizer( wx.StaticBox(self.window.widget, -1, self.label), self.orient )
        self.widget.Add(self._btn, 0, wx.EXPAND)
        for c in self.children[1:]:  # we've already added self._btn
            c.item.create()
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

    def properties_changed(self, modified):
        if not modified or "label" in modified and self.widget:
            self.widget.GetStaticBox().SetLabel(self.label or "")
            #self.layout()
        if not modified or "label" in modified or "name" in modified and self.node:
            common.app_tree.refresh_name(self.node)

        BoxSizerBase.properties_changed(self, modified)

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

    def __init__(self, name, klass, window, rows=3, cols=3, vgap=0, hgap=0, toplevel=True):
        SizerBase.__init__(self, name, klass, None, window, toplevel)

        self.rows = np.SpinProperty(rows, immediate=True)
        self.cols = np.SpinProperty(cols, immediate=True)
        self.vgap = np.SpinProperty(vgap, immediate=True)
        self.hgap = np.SpinProperty(hgap, immediate=True)

        self.children = [SizerItem(Dummy(), 0, 0, wx.EXPAND)]  # add to self.children the SizerItem for self._btn
        for i in range(1, self.rows * self.cols + 1):
            tmp = SizerSlot(self.window, self, i) # XXX no node?
            self.children.append(SizerItem(tmp, i, 1, wx.EXPAND))

    def create_widget(self):
        "This must be overriden and called at the end of the overriden version"
        to_lay_out = []
        for c in self.children[1:]:  # we've already added self._btn
            c.item.create()
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

    def fit_parent(self, *args):
        "Tell the sizer to resize the window to match the sizer's minimal size"
        if self.widget and self.window.widget:
            self.widget.Fit(self.window.widget)
            self.widget.SetSizeHints(self.window.widget)

    def _adjust_rows_cols(self):
        # adjust rows and cols properties; set slot names
        cols_p = self.properties["cols"]
        rows_p = self.properties["rows"]
        if cols_p.get()==0:
            rows_new = len(self.children)-1
        else:
            rows_new = (len(self.children)-1) // cols_p.get()
            if (len(self.children)-1) % cols_p.get(): rows_new += 1
        if rows_new!=rows_p.value: rows_p.set(rows_new)

    # context menu actions #############################################################################################
    def insert_row(self, pos=-1):
        "inserts slots for a new row"
        rows = self.rows
        cols = self.cols

        # calculate the row (0 based) to be inserted
        if pos==-1:
            add_row = rows
            # ensure that the last row is full
            for n in range( len(self.children) -rows*cols - 1 ):
                self._insert_slot( len(self.children) )
        else:
            add_row, dummy = _grid_row_col(pos, cols)

        self.properties["rows"].set( rows+1 )

        for n in range(cols):
            self._insert_slot( n+1 + add_row*cols )

        self.layout(True)
        common.app_tree.app.saved = False

    def insert_col(self, pos=-1):
        "inserts slots for a new column"
        rows = self.rows
        cols = self.cols

        # calculate the column (0 based) to be added
        if pos==-1:
            add_col = cols
        else:
            dummy, add_col = _grid_row_col(pos, cols)

        # calculate the column index of the last child (0 based)
        last_pos = len(self.children)-1
        last_pos_col = (last_pos % cols) - 1
        if last_pos_col == -1: last_pos_col = cols-1
        # fill up the last row up to the insertion position if required
        if last_pos_col < min(add_col,cols-1):
            for i in range(min(add_col,cols-1)-last_pos_col):
                self._insert_slot( last_pos+1+i )

        # insert the new colum
        self.properties["cols"].set( cols+1 )
        for r in range(rows-1,-1,-1):
            self._insert_slot( add_col+1 + r*cols )

        if self.widget:
            if self.widget.GetCols()!=cols+1: self.widget.SetCols(cols+1)
            self.layout(True)
        common.app_tree.app.saved = False

    def remove_row(self, pos):
        rows = self.rows
        cols = self.cols
        # calculate row and pos of the slot
        row,col = _grid_row_col(pos, rows)
        # find the slots that are in the same row
        slots = []
        for pos,child in enumerate(self.children):
            if pos==0: continue
            child_row, child_col = _grid_row_col(pos, cols)
            if child_row==row: slots.append(child.item)
        self.properties["rows"].set( rows-1 )
        # actually remove the slots
        for slot in reversed(slots): slot.remove()
        if self.widget: self.layout(True)

    def remove_col(self, pos):
        cols = self.cols
        # calculate row and pos of the slot
        row,col = _grid_row_col(pos, cols)
        # find the slots that are in the same row
        slots = []
        for pos,child in enumerate(self.children):
            if pos==0: continue
            child_row, child_col = _grid_row_col(pos, cols)
            if child_col==col: slots.append(child.item)
        self.properties["cols"].set( cols-1 )
        # actually remove the slots
        for slot in reversed(slots): slot.remove()
        if self.widget:
            self.widget.SetCols(cols-1)
            self.layout(True)

    ####################################################################################################################
    def properties_changed(self, modified):
        cols = self.cols
        rows = self.rows
        if rows*cols < len(self.children) + 1:
            # number of rows/cols too low
            if not modified or "cols" in modified:
                # adjust number of rows if required
                if rows*cols < len(self.children) + 1:
                    # more rows required
                    rows = (len(self.children)-1) // (cols or 1)
                    if (len(self.children)-1) % (cols or 1): rows += 1
                self.properties["rows"].set(rows)
                if modified and not "rows" in modified: modified.append("rows")
            elif "rows" in modified:
                cols = (len(self.children)-1) // (rows or 1)
                if (len(self.children)-1) % (rows or 1): cols += 1
                self.properties["cols"].set(cols)
                if modified and not "cols" in modified: modified.append("cols")

        if not "class_orient" in modified:  # otherwise, change_sizer will be called and we can skip the following
            layout = False
            if not modified or "rows" in modified and self.widget:
                if self.widget.GetRows()!=rows:
                    self.widget.SetRows(rows)
                    layout = True
            if not modified or "cols" in modified and self.widget:
                if self.widget.GetCols()!=cols:
                    self.widget.SetCols(cols)
                    layout = True
            if not modified or "hgap" in modified and self.widget:
                if self.widget.GetHGap()!=self.hgap:
                    self.widget.SetHGap(self.hgap)
                    layout = True
            if not modified or "vgap" in modified and self.widget:
                if self.widget.GetVGap()!=self.vgap:
                    self.widget.SetVGap(self.vgap)
                    layout = True

            if layout:
                self.layout(True)

        SizerBase.properties_changed(self, modified)


class EditGridSizer(GridSizerBase):
    "Class to handle wxGridSizer objects"

    WX_CLASS = "wxGridSizer"
    def __init__(self, name, window, rows=3, cols=3, vgap=0, hgap=0, toplevel=True):
        GridSizerBase.__init__(self, name, 'wxGridSizer', window, rows, cols, vgap, hgap, toplevel)

    def create_widget(self):
        self.widget = CustomSizer(self, wx.GridSizer, self.rows, self.cols, self.vgap, self.hgap)
        if not self.toplevel and getattr(self, 'sizer', None):
            # getattr(self, 'sizer') is False only in case of a 'change_sizer' call
            self.sizer.add_item(self, self.pos, self.proportion, self.flag,  self.border) # , self.widget.GetMinSize())
        GridSizerBase.create_widget(self)


class _GrowableDialog(wx.Dialog):
    def __init__(self, parent, title):
        wx.Dialog.__init__(self, parent, -1, title)
        self.sizer = sizer = wx.BoxSizer(wx.VERTICAL)
        self.message = wx.StaticText(self, -1, "")
        sizer.Add(self.message, 0, wx.TOP | wx.LEFT | wx.RIGHT | wx.EXPAND, 10)
        self.choices = wx.CheckListBox(self, -1, choices=[])
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
        for c,choice in enumerate(self._choices):
            #in range(self.choices.GetCount()):
            if self.choices.IsChecked(c):
                ret.append(choice)
        return ",".join(ret)

    def set_choices(self, choices, values):
        if wx.Platform != '__WXGTK__':
            self.choices.Set(choices)
        else:
            self.choices.Clear()
            for v in values:
                self.choices.Append(v)
        self._choices = choices
        for i,value in enumerate(choices):
            if value in values: self.choices.Check(i)

    def set_descriptions(self, title, message):
        self.SetTitle(title)
        self.message.SetLabel(message)



class _GrowablePropertyD(np.DialogPropertyD):
    def _create_dialog(self):
        if self.dialog is None:
            parent = self.text.GetTopLevelParent()
            self.dialog = _GrowableDialog(parent, self.title)
        row_or_col_count = getattr(self.owner, self.name.split("_")[-1])
        choices = [ str(n) for n in range(1, row_or_col_count+1) ]
        selected = self.get_list()
        self.dialog.set_choices(choices, selected)
        self.dialog.sizer.Fit(self.dialog)
        return self.dialog

    def _set_converter(self, value):
        # 0-based -> 1 based
        ret = [str(int(n)+1) for n in value.split(",")]
        return ",".join(ret)

    def _convert_from_text(self, value=None):
        if value is None: value = self.text.GetValue()
        row_or_col_count = getattr(self.owner, self.name.split("_")[-1])
        try:
            numbers = [int(n) for n in value.split(",")]
            if len(numbers)!=len(set(numbers)):  return None                 # double numbers
            if numbers and not row_or_col_count: return None                 # no rows/cols
            if min(numbers)<1 or max(numbers)>row_or_col_count: return None  # number out of range
        except:
            return None
        return value

    def get_list(self):
        return self.value.split(",")

    def get_str_value(self):
        # for XML file writing; 0-based
        ret = [str(int(n)-1) for n in self.get_list()]
        return ",".join(ret)


class EditFlexGridSizer(GridSizerBase):
    "Class to handle wxFlexGridSizer objects"
    WX_CLASS = "wxFlexGridSizer"

    EXTRA_PROPERTIES = GridSizerBase.EXTRA_PROPERTIES + ["growable_rows", "growable_cols"]
    _PROPERTY_HELP = {"growable_rows":'Select growable rows',
                      "growable_cols":'Select growable columns'}

    def __init__(self, name, window, rows=3, cols=3, vgap=0, hgap=0, toplevel=True):
        GridSizerBase.__init__(self, name, 'wxFlexGridSizer', window, rows, cols, vgap, hgap, toplevel)
        self.growable_rows = _GrowablePropertyD("", default_value="")
        self.growable_cols = _GrowablePropertyD("", default_value="")
        self.properties["growable_rows"].title = 'Select growable rows'
        self.properties["growable_cols"].title = 'Select growable cols'

    def create_widget(self):
        self.widget = CustomSizer(self, wx.FlexGridSizer, self.rows, self.cols, self.vgap, self.hgap)
        GridSizerBase.create_widget(self)
        for r in self.growable_rows.split(","):
            if not r.strip(): continue
            self.widget.AddGrowableRow( int(r)-1 )  # the text property is 1-based
        for c in self.growable_cols.split(","):
            if not c.strip(): continue
            self.widget.AddGrowableCol( int(c)-1 )
        if not self.toplevel and getattr(self, 'sizer', None) is not None:
            # hasattr(self, 'sizer') is False only in case of a 'change_sizer' call
            self.sizer.add_item(self, self.pos, self.proportion, self.flag, self.border)

    def _recreate(self):
        # recreate the sizer by calling change_sizer
        if self.widget is None: return
        if misc.focused_widget is self: misc.set_focused_widget(None)
        wx.CallAfter(change_sizer, self, self.get_class_orient())

    def properties_changed(self, modified):
        # XXX unify recreate with the other methods called in GridSizerBase, SizerBase
        # if recreate is required, it's not necessary to call layout
        # EditFlexGridSizer: calls _recreate, which later calls change_sizer
        # GridSizerBase: calls layout (when rows, cols, hgap, vgap are modified)
        # SizerBase: calls change_sizer if class_orient is modified

        recreate = False
        if not modified or "growable_rows" in modified or "growable_cols" in modified:
            recreate = True
        if recreate:
            self._recreate()
            return # the sizer is re-created, so we need not consider other modifications
        GridSizerBase.properties_changed(self, modified)



def _builder(parent, sizer, pos, orientation=wx.VERTICAL, slots=1, is_static=False, label="", number=[1]):
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
        sz._add_slot()
    sz.layout()

    if parent.widget: sz.create()
    if sizer is not None:
        sz.properties['flag'].set('wxEXPAND')
        sz.properties['pos'].set(pos)


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



def builder(parent, sizer, pos, number=[1]):
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
        self.rows = wx.SpinCtrl(self, -1, "3")
        self.cols = wx.SpinCtrl(self, -1, "3")
        self.vgap = wx.SpinCtrl(self, -1, "0")
        self.hgap = wx.SpinCtrl(self, -1, "0")
        # the main sizer
        sizer = wx.BoxSizer(wx.VERTICAL)
        # grid sizer with the controls
        gsizer = wx.FlexGridSizer(cols=2)
        for label, control, tooltip in [("Rows", self.rows, 'Numbers of sizer rows'),
                                        ("Cols", self.cols, 'Numbers of sizer colums'),
                                        ("Vgap", self.vgap, 'Vertical extra space between all children'),
                                        ("Hgap", self.hgap, 'Horizontal extra space between all children')]:
            gsizer.Add(wx.StaticText(self, -1, _(label)), 0, wx.ALL | wx.ALIGN_CENTER_VERTICAL, 5)
            gsizer.Add(control, 0, wx.ALL | wx.EXPAND | wx.ALIGN_CENTER_VERTICAL, 3)
            compat.SetToolTip( control, tooltip )
        self.rows.SetFocus()
        self.rows.SetSelection(-1, -1)

        gsizer.Add(wx.StaticText(self, -1, _('Flexible')), 0, wx.ALL | wx.ALIGN_CENTER_VERTICAL, 5)
        self.flex = wx.CheckBox(self, -1)
        compat.SetToolTip(self.flex, _('Create a wxFlexGridSizer instead of a wxGridSizer') )
        gsizer.Add(self.flex, 0, wx.TOP|wx.BOTTOM, 10)
        sizer.Add(gsizer)
        # horizontal sizer for action buttons
        hsizer = wx.BoxSizer(wx.HORIZONTAL)
        hsizer.Add( wx.Button(self, wx.ID_CANCEL, _('Cancel')), 1, wx.ALL, 5)
        btn = wx.Button(self, wx.ID_OK, _('OK') )
        btn.SetDefault()
        hsizer.Add(btn, 1, wx.ALL, 5)
        sizer.Add(hsizer, 0, wx.EXPAND|wx.ALIGN_CENTER )

        self.SetAutoLayout(True)
        self.SetSizer(sizer)

        sizer.Fit(self)
        self.Layout()
        self.CentreOnParent()



def grid_builder(parent, sizer, pos, number=[1]):
    "factory function for grid sizers"
    dialog = _GridBuilderDialog(parent)
    res = dialog.ShowModal()
    rows = dialog.rows.GetValue()
    cols = dialog.cols.GetValue()
    vgap = dialog.vgap.GetValue()
    hgap = dialog.hgap.GetValue()
    is_flexible = dialog.flex.GetValue()
    dialog.Destroy()
    if res != wx.ID_OK: return

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
        sz._add_slot()
    sz.layout()

    if parent.widget: sz.create()

    if sizer is not None:
        sz.properties['flag'].set('wxEXPAND')
        sz.properties['pos'].set(pos)


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
    "Module initialization function: returns dict w. key 'Sizers' and an assigned list of buttons for the main palette"

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
