# edit_sizers.py: hierarchy of Sizers supported by wxGlade
# 
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: GPL (see license.txt)

from widget_properties import *
from tree import Tree, WidgetTree
import common, math, misc, sys

class SizerSlot:
    "a window to represent a slot in a sizer"
    def __init__(self, parent, sizer, pos=0):
        self.widget = None # reference to the widget resembling the slot
        self.sizer = sizer
        self.parent = parent
        self.pos = pos
        self.menu = None

    def create_widget(self):
        self.widget = wxPanel(self.parent.widget, -1)
        self.widget.SetBackgroundColour(wxLIGHT_GREY)
        self.widget.SetAutoLayout(True)
        self.menu = wxMenu('Options')
        REMOVE_ID, PASTE_ID = wxNewId(), wxNewId()
        self.menu.Append(REMOVE_ID, 'Remove')
        self.menu.Append(PASTE_ID, 'Paste')
        EVT_PAINT(self.widget, self.on_paint)
        EVT_RIGHT_DOWN(self.widget, self.popup_menu)
        EVT_MENU(self.widget, REMOVE_ID, self.remove)
        EVT_MENU(self.widget, PASTE_ID, self.clipboard_paste)
        EVT_LEFT_DOWN(self.widget, self.drop_widget)
        EVT_ENTER_WINDOW(self.widget, self.on_enter)

    def show_widget(self, yes):
        if yes and not self.widget: self.create_widget()
        if self.widget: self.widget.Show(yes)

    def on_enter(self, event):
        if common.adding_widget: self.widget.SetCursor(wxCROSS_CURSOR)
        else: self.widget.SetCursor(wxNullCursor)
        
    def on_paint(self, event):
        dc = wxPaintDC(self.widget)
        dc.BeginDrawing()
        dc.SetBrush(wxBrush("black", wxFDIAGONAL_HATCH))
        dc.SetPen(wxBLACK_PEN)
        w, h = self.widget.GetClientSize()
        dc.DrawRectangle(0, 0, w, h)
        dc.EndDrawing()

    def on_size(self, event):
        self.widget.Refresh()

    def popup_menu(self, event):
        self.widget.PopupMenu(self.menu, event.GetPosition())

    def remove(self, event):
        self.sizer.remove_item(self)
        self.delete()

    def drop_widget(self, event):
        """\
        replaces self with a widget in self.sizer. This method is called
        to add every non-toplevel widget or sizer, and in turn calls the
        appropriate builder function (found in the ``common.widgets'' dict)
        """
        if not common.adding_widget: return
        common.adding_widget = False
        common.adding_sizer = False
        self.widget.SetCursor(wxNullCursor)
        # call the appropriate builder
        common.widgets[common.widget_to_add](self.parent, self.sizer, self.pos)
        common.widget_to_add = None
        common.app_tree.app.saved = False # update the status of the app

    def clipboard_paste(self, event):
        import clipboard
        if clipboard.paste(self.parent, self.sizer, self.pos):
            common.app_tree.app.saved = False # update the status of the app

    def delete(self, delete_widget=True):
        if self.menu: self.menu.Destroy()
        if delete_widget and self.widget: self.widget.Destroy()

# end of class SizerSlot


class SizerHandleButton(wxButton):
    """\
    Provides a ``handle'' to activate a Sizer and to access its popup menu 
    """
    def __init__(self, parent, id, sizer, menu):
        # menu: list of 2-tuples: (label, function)
        wxButton.__init__(self, parent.widget, id, '', size=(5, 5))
        self.sizer = sizer
        # provide popup menu for removal
        REMOVE_ID = wxNewId() 
        self._rmenu = misc.wxGladePopupMenu(sizer.name)
        self._rmenu.Append(REMOVE_ID, 'Remove')
        for item in menu:
            id = wxNewId()
            self._rmenu.Append(id, item[0])
            EVT_MENU(self, id, item[1])
        self.sizer._rmenu = self._rmenu
        EVT_RIGHT_DOWN(self, lambda event: self.PopupMenu(self._rmenu,
                                                          event.GetPosition()))
        EVT_MENU(self, REMOVE_ID, self._remove)

    def set_menu_title(self, title):
        self._rmenu.SetTitle(title)

    def _remove(self, *args):
        # removes the sizer from his parent, if it has one
        if self.sizer.toplevel:
            window = self.sizer.window
            common.app_tree.remove(self.sizer.node)
            window.set_sizer(None)
            return
        self.sizer.sizer.free_slot(self.sizer.pos)
        common.app_tree.remove(self.sizer.node)

    def Destroy(self):
        self._rmenu.Destroy()
        wxButton.Destroy(self)

# end of class SizerHandleButton


class SizerItem:
    """\
    Represents a child of a sizer
    """
    def __init__(self, item, pos, option=0, flag=0, border=0, size=None):
        self.item = item
        self.item.pos = pos
        self.option = option
        self.flag = flag
        self.border = border
        self.size = size

# end of class SizerItem


class SizerBase:
    """\
    Base class for every Sizer handled by wxGlade
    """
    def __init__(self, name, klass, window, toplevel=True, show=True,
                 menu=None):
        self.id = wxNewId()
        self.name = name
        self.klass = klass
        self.base = klass
        self.properties = {}
        self.property_window = window.property_window 
        self.window = window # window this sizer is responsible
                             # for the layout of

        self.widget = None # this is the actual wxSizer instance

        # toplevel: if True, self is not inside another sizer, but it is the
        # responsible of the layout of self.window
        self.toplevel = toplevel
        if not self.toplevel:
            self.option = 1
            self.flag = wxEXPAND
            self.border = 0
            
        self.menu = menu
        if self.menu is None:
            self.menu = [('Add slot', self.add_slot),
                         ('Insert slot...', self.insert_slot)]
        if not self.toplevel:
            self.menu.extend([ ('Copy', self.clipboard_copy),
                               ('Cut', self.clipboard_cut) ])

        self._btn = None # SizerHandleButton

        self.notebook = None
        self._property_setup()

        self.children = [] # list of widgets added to the sizer

    def create_widget(self):
        """\
        Creates the wxSizer self.widget
        """
        raise NotImplementedError

    def show_widget(self, yes):
        if not yes or self.widget:
            return # nothing to do if the sizer has already been created
        self._btn = SizerHandleButton(self.window, self.id, self, self.menu)
        # ScreenToClient used by WidgetTree for the popup menu
        EVT_BUTTON(self._btn, self.id, self.show_properties)
        self.create_widget()
        self.widget.Refresh = self.refresh
        self.widget.GetBestSize = self.widget.GetMinSize
        self.widget.ScreenToClient = self._btn.ScreenToClient
        if self.toplevel: self.window.set_sizer(self)

    def _property_setup(self):
        """\
        Setup of the Properties of self.
        """
        self.flags_pos = [wxEXPAND, wxALIGN_RIGHT, wxALIGN_BOTTOM,
                          wxALIGN_CENTER_HORIZONTAL, wxALIGN_CENTER_VERTICAL,
                          wxLEFT, wxRIGHT, wxTOP, wxBOTTOM]

        self.access_functions = {
            'name' : (lambda : self.name, self.set_name),
            'class' : (lambda : self.klass, lambda v: None)
            }
        if not self.toplevel:
            self.access_functions['option'] = (self.get_option,self.set_option)
            self.access_functions['flag'] = (self.get_flag, self.set_flag)
            self.access_functions['border'] = (self.get_border,self.set_border)

        self.name_prop = TextProperty(self, 'name', None)
        self.klass_prop = TextProperty(self, 'class', None, readonly=True)
        if not self.toplevel:
            prop = self.sizer_properties = {}
            prop['option'] = SpinProperty(self, 'option', None, 0, (0, 1000))
            flag_labels = ['#section#Alignment', 'wxEXPAND', 'wxALIGN_RIGHT',
                           'wxALIGN_BOTTOM', 'wxALIGN_CENTER_HORIZONTAL',
                           'wxALIGN_CENTER_VERTICAL', '#section#Border',
                           'wxLEFT', 'wxRIGHT', 'wxTOP', 'wxBOTTOM']
            prop['flag'] = CheckListProperty(self, 'flag', None, flag_labels)
            prop['border'] = SpinProperty(self, 'border', None, 0, (0, 1000))

    def create_properties(self):
        """\
        Displays the Properties of self
        """
        self.notebook = wxNotebook(common.property_panel, -1)
        nb_sizer = wxNotebookSizer(self.notebook)
        self.notebook.sizer = nb_sizer
        self.notebook.SetAutoLayout(True)
        panel = wxScrolledWindow(self.notebook, -1)
        sizer_tmp = wxBoxSizer(wxVERTICAL)
        self.name_prop.display(panel)
        self.klass_prop.display(panel)
        sizer_tmp.Add(self.name_prop.panel, 0, wxEXPAND)
        sizer_tmp.Add(self.klass_prop.panel, 0, wxEXPAND)
        if not self.toplevel:
            prop = self.sizer_properties
            prop['option'].display(panel)
            prop['flag'].display(panel)
            prop['border'].display(panel)
            sizer_tmp.Add(prop['option'].panel, 0, wxEXPAND)
            sizer_tmp.Add(prop['border'].panel, 0, wxEXPAND)
            sizer_tmp.Add(prop['flag'].panel, 0, wxEXPAND)
        else:
            # button to Fit parent
            FIT_ID = wxNewId()
            self.fit_btn = wxButton(panel, FIT_ID, 'Fit parent')
            EVT_BUTTON(self.fit_btn, FIT_ID, self.fit_parent)
            sizer_tmp.Add(self.fit_btn, 0, wxALL|wxEXPAND, 5)
        panel.SetAutoLayout(True)
        panel.SetSizer(sizer_tmp)
        sizer_tmp.Fit(panel)
        
        self.notebook.AddPage(panel, "Common")
        w, h = panel.GetClientSizeTuple()
        panel.SetScrollbars(1, 5, 1, math.ceil(h/5.0))        

    def popup_menu(self, event):
        """\
        pops up a menu to add or remove slots from self, or to remove self
        from the application.
        """
        if self._btn:
            self._btn.PopupMenu(self._btn._rmenu, event.GetPosition())

    def set_name(self, value):
        self.name = value
        self._btn.set_menu_title(value)
        try: common.app_tree.set_name(self.node, self.name)
        except AttributeError:
            import traceback; traceback.print_exc()
            
    def __getitem__(self, value):
        return self.access_functions[value]

    def show_properties(self, *args):
        """\
        Updates common.property_panel to show the notebook with the Properties
        of self
        """
        if not self.window.is_visible(): return
        if not self.notebook:
            self.create_properties()
        sizer_tmp = self.property_window.GetSizer()
        sizer_tmp = wxPyTypeCast(sizer_tmp, "wxBoxSizer")
        child = wxPyTypeCast(sizer_tmp.GetChildren()[0], "wxSizerItem")
        w = wxPyTypeCast(child.GetWindow(), "wxWindow")
        if w is self.notebook: return
        w.Hide()
        child.SetWindow(self.notebook)
        self.property_window.Layout()
        self.property_window.SetTitle('Properties - <%s>' % self.name)
        if hasattr(self, 'node'): common.app_tree.select_item(self.node)
        self.notebook.Show()
        
    def fit_parent(self, *args):
        """\
        Tell the sizer to resize the window to match the sizer's minimal size
        """
        if self.widget and self.window.widget:
            self.widget.Fit(self.window.widget)
    
    def add_item(self, item, pos=None, option=0, flag=0, border=0, size=None,
                 force_layout=True):
        """\
        Adds an item to self.
        """
        option = int(option); flag = int(flag); border = int(border)
        if pos is None:
            pos = len(self.children)
            self.children.append(SizerItem(item, pos, option, flag, border,
                                           size))
            self.add_slot()
        try:
            old_child = self.children[pos]
            if isinstance(old_child.item, SizerSlot):
                old_child.item.delete(False)            
            self.children[pos] = SizerItem(item, pos, option, flag, border,
                                           size)
        except IndexError: # this shouldn't happen!
            import traceback; traceback.print_exc()
            raise SystemExit

        item.sizer = self
        item.pos = pos

        self._add_item_widget(item, pos, option, flag, border, size,
                              force_layout)
        
    def _add_item_widget(self, item, pos, option, flag, border, size,
                         force_layout):
        if not self.widget: return # nothing more to do
        if not item.widget: return

        try:
            elem = self.widget.GetChildren()[pos]
        except IndexError: # this happens after loading from xml
            self.widget.Add(item.widget, option, flag, border)
            if size: w, h = size
            else: w, h = item.widget.GetBestSize()
            self.widget.SetItemMinSize(item.widget, w, h)
            return
        
        if elem.IsWindow(): # remove the previous item at pos
            w = elem.GetWindow()
            elem.SetWindow(None)
            w.Destroy()
        try: # let's see if the item to add is a window
            elem.SetWindow(item.widget)
        except TypeError: # suppose the item to add is a sizer
            elem.SetSizer(item.widget)
        elem.SetOption(option)
        elem.SetFlag(flag)
        elem.SetBorder(border)
        try: # if the item was a window, set its size to a reasonable value
            if size: w, h = size
            else: w, h = item.widget.GetBestSize()
            self.widget.SetItemMinSize(item.widget, w, h)
        except: pass
        if force_layout:
            self.Layout() # update the layout of self
       
    def set_item(self, pos, option=None, flag=None, border=None, size=None,
                 force_layout=True):
        """\
        Updates the layout of the item at the given pos.
        """
        try: item = self.children[pos]
        except IndexError: # this shouldn't happen
            import traceback; traceback.print_exc()
            raise SystemExit
        if option is not None:
            option = int(option)
            item.option = option
        if flag is not None:
            flag = int(flag)
            item.flag = flag
        if border is not None:
            border = int(border)
            item.border = border
        if size is not None: item.size = size

        self._set_item_widget(pos, option, flag, border, size, force_layout)

    def _set_item_widget(self, pos, option, flag, border, size, force_layout):
        if not self.widget: return
        
        try: elem = self.widget.GetChildren()[pos]
        except IndexError: return # this may happen during xml loading
        
        if option is not None: elem.SetOption(option)
        if flag is not None: elem.SetFlag(flag)
        if border is not None: elem.SetBorder(border)
        if elem.IsWindow():
            if size is None: size = elem.GetSize()
            item = elem.GetWindow()
            self.widget.SetItemMinSize(item, size[0], size[1])
        if force_layout:
            self.Layout()
            try: self.sizer.Layout()
            except AttributeError: pass

    def remove_item(self, elem, force_layout=True):
        """\
        Removes elem from self.
        """
        if elem:
            for c in self.children[elem.pos+1:]: c.item.pos -= 1
            del self.children[elem.pos]
        if self.widget and elem.widget:
            self.widget.Remove(elem.widget)
            if force_layout:
                self.Layout()
                if not self.toplevel: self.sizer.Layout()
    Remove = remove_item # maybe this is needed, I have to check...

    def Layout(self):
        #if not self.widget or not self.window.is_visible(): return
        if not self.widget: return
        self.widget.Layout()
        for c in self.children:
            try: c.item.widget.Refresh()
            except: pass

    def set_option(self, value):
        """\
        If self is not a toplevel sizer, update the layout to reflect the value
        of the option property
        """
        self.option = int(value)
        try:
            self.sizer.set_item(self.pos, option=self.option)
        except AttributeError, e: print e
        self.finish_set()

    def set_flag(self, value):
        """\
        If self is not a toplevel sizer, update the layout to reflect
        value of the flag property
        """
        value = self.sizer_properties['flag'].prepare_value(value)
        flags = 0
        for v in range(len(value)):
            if value[v]:
                flags |= self.flags_pos[v]
        self.flag = flags
        try: self.sizer.set_item(self.pos, flag=flags)
        except AttributeError, e: print 'Errore: ' + str(e)
        self.finish_set()

    def set_border(self, value):
        """\
        If self is not a toplevel sizer, update the layout to reflect
        value of the border property
        """
        self.border = int(value)
        try: self.sizer.set_item(self.pos, border=self.border)
        except AttributeError, e: print e

    def get_option(self):
        if not hasattr(self, 'sizer'): return '0'
        return str(self.option)

    def get_flag(self):
        retval = [0] * len(self.flags_pos)
        if not hasattr(self, 'sizer'): return retval
        try:
            flag = self.flag
            for i in range(len(self.flags_pos)):
                if flag & self.flags_pos[i]: retval[i] = 1
        except AttributeError: pass
        return retval

    def get_int_flag(self): return self.flag

    def get_border(self):
        if not hasattr(self, 'sizer'): return '0'
        return str(self.border)

    def remove(self):
        # this function is here for clipboard compatibility
        if not self._btn: return
        self._btn._remove()

    def delete(self):
        """\
        ``Destructor''
        """
        self._rmenu = None
        if self._btn: self._btn.Destroy()
        if self.notebook:
            for p in self.properties.itervalues():
                if p.panel: p.panel.Destroy()
            if self.name_prop.panel: self.name_prop.panel.Destroy()
            if self.klass_prop.panel: self.klass_prop.panel.Destroy()
            if hasattr(self, 'sizer_properties'):
                for p in self.sizer_properties.itervalues():
                    if p.panel: p.panel.Destroy()
            nb_szr = self.notebook.sizer
            self.notebook.Destroy()
            #self.notebook.sizer.Destroy()
            nb_szr.Destroy()
        for c in self.children:
            if c.item and isinstance(c.item, SizerSlot): c.item.delete()
        if self.toplevel:
            self.window.set_sizer(None)

    if wxPlatform == '__WXMSW__':
        def finish_set(self):
            for c in self.children:
                if c.item.widget:
                    try: c.item.widget.Refresh()
                    except AttributeError: pass # sizers have no Refresh
    else:
        def finish_set(self): pass

    def refresh(self, *args):
        # this will be self.widget.Refresh
        for c in self.children:
            if c.item.widget: 
                try: c.item.widget.Refresh()
                except AttributeError: pass

    def update_view(self, *args): pass

    def add_slot(self, *args, **kwds):
        """\
        adds a slot to the sizer, i.e. a fake window that will accept the
        dropping of widgets
        """
        if not self.widget: return
        tmp = SizerSlot(self.window, self, len(self.children))
        item = SizerItem(tmp, len(self.children), 1, wxEXPAND)
        self.children.append(item)
        if self.widget:
            tmp.show_widget(True) # create the actual SizerSlot widget
            self.widget.Add(tmp.widget, 1, wxEXPAND)
            self.widget.SetItemMinSize(tmp.widget, 20, 20)
            force_layout = kwds.get('force_layout', True)
            if force_layout: self.Layout()
        common.app_tree.app.saved = False

    def insert_slot(self, *args, **kwds):
        """\
        inserts a slot into the sizer: the user will be asked for a position
        before which to insert the SizerSlot object. This method is meaningful
        only in an interactive session
        """
        if not self.widget: return

        class Dialog(wxDialog):
            def __init__(self, max_val):
                wxDialog.__init__(self, None, -1, "Select a position")
                self.pos = 0
                pos_prop = SpinProperty(self, 'position', self, r=(0, max_val))
                szr = wxBoxSizer(wxVERTICAL)
                szr.Add(pos_prop.panel, 0, wxEXPAND)
                szr.Add(wxButton(self, wxID_OK, "OK"), 0, wxALL|wxALIGN_CENTER,
                        3)
                self.SetAutoLayout(True)
                self.SetSizer(szr)
                szr.Fit(self)
            def __getitem__(self, name):
                def set_pos(v): self.pos = int(v)
                return (lambda : self.pos, set_pos)
            
        # end of inner class

        dialog = Dialog(len(self.children)-1)
        dialog.ShowModal()
        pos = dialog.pos + 1
        tmp = SizerSlot(self.window, self, pos)
        for c in self.children[pos:]:
            c.item.pos += 1
        self.children.insert(pos, SizerItem(tmp, pos, 1, wxEXPAND, 0))
        tmp.show_widget(True) # create the actual SizerSlot
        self.widget.Insert(pos, tmp.widget, 1, wxEXPAND)
        self.widget.SetItemMinSize(tmp.widget, 20, 20)
        force_layout = kwds.get('force_layout', True)
        if force_layout: self.Layout()
        common.app_tree.app.saved = False

    def free_slot(self, pos, force_layout=True):
        """\
        Replaces the element at pos with an empty slot
        """
        tmp = SizerSlot(self.window, self, pos)
        item = SizerItem(tmp, pos, 1, wxEXPAND, 0)
        self.children[pos] = item
        if self.widget:
            tmp.show_widget(True) # create the actual SizerSlot
            elem = self.widget.GetChildren()[pos]
            elem.SetWindow(tmp.widget)
            elem.SetSizer(None)
            elem.SetOption(1)
            elem.SetBorder(0)
            elem.SetFlag(wxEXPAND)
            if force_layout: self.widget.Layout()

    def is_visible(self):
        return self.window.is_visible()
            
    def clipboard_copy(self, event):
        """\
        returns a copy of self to be inserted in the clipboard
        """
        import clipboard
        clipboard.copy(self)

    def clipboard_cut(self, event):
        import clipboard
        clipboard.cut(self)

# end of class SizerBase


class EditBoxSizer(SizerBase):
    """\
    Class to handle wxBoxSizer objects
    """
    def __init__(self, name, window, orient=wxVERTICAL, elements=3,
                 toplevel=True, show=True):
        SizerBase.__init__(self, name, 'wxBoxSizer', window, toplevel, show)
        self.access_functions['orient'] = (self.get_orient, self.set_orient)
        self.properties = {'orient': HiddenProperty(self, 'orient',
                                                    (orient==wxHORIZONTAL and
                                                     'wxHORIZONTAL' or
                                                     'wxVERTICAL')) }
        class Dummy: widget = None
        # add to self.children the SizerItem for self._btn
        self.children = [SizerItem(Dummy(), 0, 0, wxEXPAND)]
        for i in range(1, elements+1):
            tmp = SizerSlot(self.window, self, i)
            self.children.append(SizerItem(tmp, i, 1, wxEXPAND))
        
        self.orient = orient

    def create_widget(self):
        self.widget = wxBoxSizer(self.orient)
        self.widget.Add(self._btn, 0, wxEXPAND)
        for c in self.children[1:]: # we've already added self._btn
            c.item.show_widget(True)
            if isinstance(c.item, SizerSlot):
                self.widget.Add(c.item.widget, 1, wxEXPAND)
                self.widget.SetItemMinSize(c.item.widget, 20, 20)
            else:
                sp = c.item.properties.get('size')
                if sp and sp.is_active():
                    w, h = [ int(v) for v in sp.get_value().split(',') ]
                else: w, h = c.item.widget.GetBestSize()
                self.widget.SetItemMinSize(c.item.widget, w, h)
        if not self.toplevel:
            self.sizer.add_item(self, self.pos, self.option, self.flag,
                                self.border, self.widget.GetMinSize())

    def get_orient(self):
        od = { wxHORIZONTAL: 'wxHORIZONTAL',
               wxVERTICAL: 'wxVERTICAL' }
        return od.get(self.orient)
    
    def set_orient(self, value):
        od = { 'wxHORIZONTAL': wxHORIZONTAL,
               'wxVERTICAL': wxVERTICAL }
        self.orient = od.get(value, wxVERTICAL)

# end of class EditBoxSizer


class EditStaticBoxSizer(SizerBase):
    """\
    Class to handle wxStaticBoxSizer objects
    """
    def __init__(self, name, window, orient=wxVERTICAL, label=None, elements=3,
                 toplevel=True, show=True):
        if not label: label = name
        self.label = label
        self.orient = orient
        SizerBase.__init__(self, name, 'wxStaticBoxSizer', window, toplevel,
                           show)
        self.access_functions['orient'] = (self.get_orient, self.set_orient)
        self.properties['orient'] = HiddenProperty(self, 'orient',
                                                   (orient==wxHORIZONTAL and
                                                    'wxHORIZONTAL' or
                                                    'wxVERTICAL'))
        class Dummy: widget = None
        # add to self.children the SizerItem for self._btn
        self.children = [SizerItem(Dummy(), 0, 0, wxEXPAND)]
        for i in range(1, elements+1):
            tmp = SizerSlot(self.window, self, i)
            self.children.append(SizerItem(tmp, i, 1, wxEXPAND))

    def create_widget(self):
        self.widget = wxStaticBoxSizer(wxStaticBox(self.window.widget, -1,
                                                   self.label), self.orient)
        self.widget.Add(self._btn, 0, wxEXPAND)
        for c in self.children[1:]: # we've already added self._btn
            c.item.show_widget(True)
            if isinstance(c.item, SizerSlot):
                self.widget.Add(c.item.widget, 1, wxEXPAND)
                self.widget.SetItemMinSize(c.item.widget, 20, 20)
            else:
                sp = c.item.properties.get('size')
                if sp and sp.is_active():
                    w, h = [ int(v) for v in sp.get_value().split(',') ]
                else: w, h = c.item.widget.GetBestSize()
                self.widget.SetItemMinSize(c.item.widget, w, h)
        if not self.toplevel:
            self.sizer.add_item(self, self.pos, self.option, self.flag,
                                self.border, self.widget.GetMinSize())

    def _property_setup(self):
        SizerBase._property_setup(self)
        self.access_functions['label'] = (self.get_label, self.set_label)
        self.properties['label'] = TextProperty(self, 'label', None) #panel)

    def create_properties(self):
        SizerBase.create_properties(self)
        panel = self.notebook.GetPage(0)
        sizer = panel.GetSizer()
        self.properties['label'].display(panel)
        sizer.Add(self.properties['label'].panel, 0, wxEXPAND)
        sizer.Layout()
        w, h = sizer.GetMinSize()
        panel.SetScrollbars(1, 5, 1, math.ceil(h/5.0))

    def set_label(self, value):
        """\
        Sets the label of the static box
        """
        self.label = str(value)
        if self.widget: self.widget.GetStaticBox().SetLabel(self.label)

    def get_label(self): return self.label

    def delete(self):
        if self.widget: self.widget.GetStaticBox().Destroy()
        SizerBase.delete(self)
        
    def get_orient(self):
        od = { wxHORIZONTAL: 'wxHORIZONTAL',
               wxVERTICAL: 'wxVERTICAL' }
        return od.get(self.orient)
    
    def set_orient(self, value):
        od = { 'wxHORIZONTAL': wxHORIZONTAL,
               'wxVERTICAL': wxVERTICAL }
        self.orient = od.get(value, wxVERTICAL)

# end of class EditStaticBoxSizer


class CustomSizer(wxBoxSizer):
    """\
    Custom wxSizer class used to implement a GridSizer with an additional
    handle button
    """
    def __init__(self, parent, factory, rows, cols, vgap, hgap):
        wxBoxSizer.__init__(self, wxVERTICAL)
        self.parent = parent
        self._grid = factory(rows, cols, vgap, hgap)
        wxBoxSizer.Add(self, self.parent._btn, 0, wxEXPAND)
        wxBoxSizer.Add(self, self._grid, 1, wxEXPAND)

    def __getattr__(self, name):
        return getattr(self._grid, name)

    def GetBestSize(self):
        return self._grid.GetMinSize()

    def Add(self, *args, **kwds): self._grid.Add(*args, **kwds)
    def Insert(self, *args, **kwds): self._grid.Insert(*args, **kwds)
    def Remove(self, *args, **kwds): self._grid.Remove(*args, **kwds)
    def SetItemMinSize(self, *args, **kwds):
        self._grid.SetItemMinSize(*args, **kwds)

    def GetChildren(self):
        return [None] + self._grid.GetChildren()

    def Layout(self):
        self._grid.Layout()
        wxBoxSizer.Layout(self)

# end of class CustomSizer


class GridSizerBase(SizerBase):
    """\
    Base class for Grid sizers. Must not be instantiated.
    """
    def __init__(self, name, klass, window, rows=3, cols=3, vgap=0, hgap=0,
                 toplevel=True, show=True):
        self.rows = rows; self.cols = cols
        self.vgap = vgap; self.hgap = hgap
        if self.cols or self.rows:
            if not self.rows: self.rows = 1
            elif not self.cols: self.cols = 1
        SizerBase.__init__(self, name, klass, window, toplevel, show)

        class Dummy: widget = None
        # add to self.children the SizerItem for self._btn
        self.children = [SizerItem(Dummy(), 0, 0, wxEXPAND)]
        for i in range(1, self.rows*self.cols+1):
            tmp = SizerSlot(self.window, self, i)
            self.children.append(SizerItem(tmp, i, 1, wxEXPAND))

    def create_widget(self):
        """\
        This must be overriden and called at the end of the overriden version
        """
        for c in self.children[1:]: # we've already added self._btn
            c.item.show_widget(True)
            if isinstance(c.item, SizerSlot):
                self.widget.Add(c.item.widget, 1, wxEXPAND)
                self.widget.SetItemMinSize(c.item.widget, 20, 20)
            else:
                sp = c.item.properties.get('size')
                if sp and sp.is_active():
                    w, h = [ int(v) for v in sp.get_value().split(',') ]
                else: w, h = c.item.widget.GetBestSize()
                self.widget.SetItemMinSize(c.item.widget, w, h)

    def _property_setup(self):
        SizerBase._property_setup(self)
        self.access_functions['rows'] = (self.get_rows, self.set_rows)
        self.access_functions['cols'] = (self.get_cols, self.set_cols)
        self.access_functions['hgap'] = (self.get_hgap, self.set_hgap)
        self.access_functions['vgap'] = (self.get_vgap, self.set_vgap)
        props = { 'rows': SpinProperty(self, 'rows', None),
                  'cols': SpinProperty(self, 'cols', None),
                  'hgap': SpinProperty(self, 'hgap', None),
                  'vgap': SpinProperty(self, 'vgap', None) }
        self.properties = props

    def create_properties(self):
        SizerBase.create_properties(self)
        page = wxScrolledWindow(self.notebook, -1)
        sizer = wxBoxSizer(wxVERTICAL)
        props = self.properties
        props['rows'].display(page)
        props['cols'].display(page)
        props['vgap'].display(page)
        props['hgap'].display(page)
        sizer.Add(props['rows'].panel, 0, wxEXPAND)
        sizer.Add(props['cols'].panel, 0, wxEXPAND)
        sizer.Add(props['vgap'].panel, 0, wxEXPAND)
        sizer.Add(props['hgap'].panel, 0, wxEXPAND)
        page.SetAutoLayout(True)
        page.SetSizer(sizer)
        sizer.Fit(page)
        self.notebook.AddPage(page, "Grid")

    def get_rows(self): return self.rows
    def get_cols(self): return self.cols
    def get_vgap(self): return self.vgap
    def get_hgap(self): return self.hgap

    def set_rows(self, rows):
        self.rows = int(rows)
        if self.widget:
            self.widget.SetRows(self.rows)
            self.Layout()

    def set_cols(self, cols):
        self.cols = int(cols)
        if self.widget:
            self.widget.SetCols(self.cols)
            self.Layout()

    def set_hgap(self, hgap):
        self.hgap = int(hgap)
        if self.widget:
            self.widget.SetHGap(self.hgap)
            self.Layout()

    def set_vgap(self, vgap):
        self.vgap = int(vgap)
        if self.widget:
            self.widget.SetVGap(self.vgap)
            self.Layout()

    def fit_parent(self, event):
        """\
        Tell the sizer to resize the window to match the sizer's minimal size
        """
        if self.widget and self.window.widget:
            self.widget.Fit(self.window.widget)
        
    def insert_slot(self, *args, **kwds):
        """\
        inserts a slot into the sizer: the user will be asked for a position
        before which to insert the SizerSlot object
        """
        if not self.widget: return
        
        class Dialog(wxDialog):
            def __init__(self, max_val):
                wxDialog.__init__(self, None, -1, "Select a position")
                self.pos = 0
                pos_prop = SpinProperty(self, 'position', self, r=(0, max_val))
                szr = wxBoxSizer(wxVERTICAL)
                szr.Add(pos_prop.panel, 0, wxEXPAND)
                szr.Add(wxButton(self, wxID_OK, "OK"), 0, wxALL|wxALIGN_CENTER,
                        3)
                self.SetAutoLayout(True)
                self.SetSizer(szr)
                szr.Fit(self)
            def __getitem__(self, name):
                def set_pos(v): self.pos = int(v)
                return (lambda : self.pos, set_pos)
            
        # end of inner class

        dialog = Dialog(len(self.children))
        dialog.ShowModal()
        pos = dialog.pos 
        tmp = SizerSlot(self.window, self, pos)
        for c in self.children[pos:]:
            c.item.pos += 1
        self.children.insert(pos, SizerItem(tmp, pos, 1, wxEXPAND, 0))
        tmp.show_widget(True) # create the actual SizerSlot
        self.widget.Insert(pos, tmp.widget, 1, wxEXPAND)
        self.widget.SetItemMinSize(tmp.widget, 20, 20)
        force_layout = kwds.get('force_layout', True)
        if force_layout: self.Layout()
        common.app_tree.app.saved = False

    def _adjust_initial_size(self, w, h): pass

# end of class GridSizerBase


class EditGridSizer(GridSizerBase):
    """\
    Class to handle wxGridSizer objects
    """
    def __init__(self, name, window, rows=3, cols=3, vgap=0, hgap=0,
                 toplevel=True, show=True):
        GridSizerBase.__init__(self, name, 'wxGridSizer', window, rows, cols,
                               vgap, hgap, toplevel, show)
        
    def create_widget(self):
        self.widget = CustomSizer(self, wxGridSizer, self.rows, self.cols,
                                  self.vgap, self.hgap)
        if not self.toplevel:
            self.sizer.add_item(self, self.pos, self.option, self.flag,
                                self.border) #, self.widget.GetMinSize())
        GridSizerBase.create_widget(self)

# end of class EditGridSizer


class EditFlexGridSizer(GridSizerBase):
    """\
    Class to handle wxFlexGridSizer objects
    """
    def __init__(self, name, window, rows=3, cols=3, vgap=0, hgap=0,
                 toplevel=True, show=True):
        GridSizerBase.__init__(self, name, 'wxFlexGridSizer', window, rows,
                               cols, vgap, hgap, toplevel, show)

    def create_widget(self):
        self.widget = CustomSizer(self, wxFlexGridSizer, self.rows, self.cols,
                                  self.vgap, self.hgap)
        GridSizerBase.create_widget(self)
        if not self.toplevel:
            self.sizer.add_item(self, self.pos, self.option, self.flag,
                                self.border) #, self.widget.GetMinSize())
        else:
            w, h = self.window.widget.GetClientSize()
            self._adjust_initial_size(w, h)

    def _adjust_initial_size(self, w, h):
        if self.widget:
            w = w/(self.widget.GetCols() or 1)
            h = h/(self.widget.GetRows() or 1)
            for c in self.children[1:]:
                self.widget.SetItemMinSize(c.item.widget, w, h)
            self.Layout()

# end of class EditFlexGridSizer
    

def builder(parent, sizer, pos, number=[1]):
    """\
    factory function for box sizers.
    """
    class SizerDialog(wxDialog):
        def __init__(self, parent):
            wxDialog.__init__(self, misc.get_toplevel_parent(parent), -1,
                              'Select sizer type')
            tmp = wxFlexGridSizer(2, 2)
            tmp.Add(wxStaticText(self, -1, 'Orientation'), 0, wxALL, 3)
            self.orientation = wxChoice(self, -1, choices=['Horizontal',
                                                           'Vertical'])
            self.orientation.SetSelection(0)
            tmp.Add(self.orientation, 0, wxALL|wxEXPAND, 3)
            tmp.Add(wxStaticText(self, -1, 'Slots'), 0, wxALL, 3)
            self.num = wxSpinCtrl(self, -1)
            self.num.SetRange(1, 100)
            self.num.SetValue(1)
            tmp.Add(self.num, 0, wxALL, 3)
            szr = wxBoxSizer(wxVERTICAL)
            szr.Add(tmp)
            CHECK_ID = wxNewId()
            self.check = wxCheckBox(self, CHECK_ID, 'Has a Static Box')
            self.label = wxTextCtrl(self, -1, "")
            self.label.Enable(False)
            EVT_CHECKBOX(self, CHECK_ID, self.on_check_statbox)
            szr.Add(self.check, 0, wxALL|wxEXPAND, 4)
            tmp = wxBoxSizer(wxHORIZONTAL)
            tmp.Add(wxStaticText(self, -1, "Label: "))
            tmp.Add(self.label, 1)
            szr.Add(tmp, 0, wxALL|wxEXPAND, 4)
            
            tmp = wxBoxSizer(wxVERTICAL)
            tmp.Add(wxButton(self, wxID_OK, 'OK'))
            szr.Add(tmp, 0, wxALL|wxALIGN_CENTER, 3)
            self.SetAutoLayout(1)
            self.SetSizer(szr)
            szr.Fit(self)

        def reset(self):
            self.orientation.SetSelection(0);
            self.num.SetValue(1)
            self.check.SetValue(0)
            self.label.SetValue("")
            self.label.Enable(False)

        def on_check_statbox(self, event):
            self.label.Enable(event.IsChecked())

    # end of class SizerDialog

    dialog = SizerDialog(parent)
    dialog.ShowModal()
    if dialog.orientation.GetStringSelection() == 'Horizontal':
        orientation = wxHORIZONTAL
    else: orientation = wxVERTICAL
    num = dialog.num.GetValue()
    name = 'sizer_%d' % number[0]
    while common.app_tree.has_name(name):
        number[0] += 1
        name = 'sizer_%d' % number[0]
    if sizer is not None: topl = 0
    else: topl = 1
    if dialog.check.GetValue():
        sz = EditStaticBoxSizer(name, parent, orientation,
                                dialog.label.GetValue(), num, topl)
    else:
        sz = EditBoxSizer(name, parent, orientation, num, topl)
    if sizer is not None:
        sizer.add_item(sz, pos, 1, wxEXPAND)
        node = Tree.Node(sz)
        sz.node = node
        common.app_tree.insert(node, sizer.node, pos-1)
        common.adding_sizer = False
    else:
        parent.set_sizer(sz)
        node = Tree.Node(sz)
        sz.node = node
        if pos is None: common.app_tree.add(node, parent.node)
        else:
            common.app_tree.insert(node, parent.node, pos-1)
            sz.pos = pos

    sz.show_widget(True)
    
    dialog.Destroy()


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    """\
    factory function to build EditBoxSizer objects from an xml file
    """
    from xml_parse import XmlParsingError
    try: name = attrs['name']
    except KeyError: raise XmlParsingError, "'name' attribute missing"
    orientation = wxVERTICAL # default value
    if sizer is not None: topl = False
    else: topl = True
    if attrs['base'] == 'EditStaticBoxSizer':
        sz = EditStaticBoxSizer(name, parent, orientation,
                                name, 0, topl)
    else:
        sz = EditBoxSizer(name, parent, orientation, 0, topl)
    if sizer is not None:
        if sizeritem is None:
            raise XmlParsingError, "'sizeritem' object not found"
        sizer.add_item(sz, pos=pos, option=sizeritem.option,
                       flag=sizeritem.flag, border=sizeritem.border) 
        node = Tree.Node(sz)
        sz.node = node
        if pos is None: common.app_tree.add(node, sizer.node)
        else: common.app_tree.insert(node, sizer.node, pos-1)
    else:
        parent.set_sizer(sz)
        node = Tree.Node(sz)
        sz.node = node
        common.app_tree.add(node, parent.node)
    return sz


def grid_builder(parent, sizer, pos, number=[1]):
    """\
    factory function for grid sizers
    """
    class Dialog(wxDialog):
        def __init__(self, parent):
            wxDialog.__init__(self, misc.get_toplevel_parent(parent), -1,
                              'Select sizer attributes')
            self.rows = SpinProperty(self, 'rows', self)
            self.cols = SpinProperty(self, 'cols', self)
            self.vgap = SpinProperty(self, 'vgap', self)
            self.hgap = SpinProperty(self, 'hgap', self)
            self.flex = wxCheckBox(self, -1, '')

            self.rows.set_value(3)
            self.cols.set_value(3)
            self.vgap.set_value(0)
            self.hgap.set_value(0)

            szr = wxBoxSizer(wxHORIZONTAL)
            szr.Add(wxStaticText(self, -1, 'Flexible'), 2, wxALL, 4)
            szr.Add(self.flex, 5, wxALL, 4)

            sizer = wxBoxSizer(wxVERTICAL)
            sizer.Add(self.rows.panel, 0, wxEXPAND)
            sizer.Add(self.cols.panel, 0, wxEXPAND)
            sizer.Add(self.vgap.panel, 0, wxEXPAND)
            sizer.Add(self.hgap.panel, 0, wxEXPAND)
            sizer.Add(szr, 0, wxEXPAND)
            szr = wxBoxSizer(wxHORIZONTAL)
            szr.Add(wxButton(self, wxID_OK, 'OK'))
            sizer.Add(szr, 0, wxALL|wxALIGN_CENTER, 4)
            self.SetAutoLayout(True)
            self.SetSizer(sizer)
            sizer.Fit(self)

        def __getitem__(self, name):
            return (lambda : 0, lambda v: None)

    # end of inner class

    dialog = Dialog(parent)
    dialog.ShowModal()
    rows = int(dialog.rows.get_value())
    cols = int(dialog.cols.get_value())
    vgap = int(dialog.vgap.get_value())
    hgap = int(dialog.hgap.get_value())

    name = 'grid_sizer_%d' % number[0]
    while common.app_tree.has_name(name):
        number[0] += 1
        name = 'grid_sizer_%d' % number[0]
    topl = True
    if dialog.flex.GetValue(): constructor = EditFlexGridSizer
    else: constructor = EditGridSizer
    if sizer is not None: topl = False
    sz = constructor(name, parent, rows, cols, vgap, hgap, topl)
    if sizer is not None:
        sizer.add_item(sz, pos, 1, wxEXPAND)
        node = Tree.Node(sz)
        sz.node = node
        common.app_tree.insert(node, sizer.node, pos-1)
        common.adding_sizer = False
    else:
        parent.set_sizer(sz)
        node = Tree.Node(sz)
        sz.node = node
        if pos is None: common.app_tree.add(node, parent.node)
        else:
            common.app_tree.insert(node, parent.node, pos-1)
            sz.pos = pos

    sz.show_widget(True)

    dialog.Destroy()


def grid_xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    """\
    factory function to build EditGridSizer objects from an xml file
    """
    from xml_parse import XmlParsingError
    try: name = attrs['name']
    except KeyError: raise XmlParsingError, "'name' attribute missing"
    if attrs['base'] == 'EditGridSizer': constructor = EditGridSizer
    else: constructor = EditFlexGridSizer
    if sizer is not None: 
        sz = constructor(name, parent, rows=0, cols=0, toplevel=False)
        if sizeritem is None:
            raise XmlParsingError, "'sizeritem' object not found"
        sizer.add_item(sz, pos=pos, option=sizeritem.option,
                       flag=sizeritem.flag, border=sizeritem.border)
        node = Tree.Node(sz)
        sz.node = node
        if pos is None: common.app_tree.add(node, sizer.node)
        else: common.app_tree.insert(node, sizer.node, pos-1)
    else: 
        sz = constructor(name, parent, rows=0, cols=0, toplevel=True)
        parent.set_sizer(sz)
        node = Tree.Node(sz)
        sz.node = node
        common.app_tree.add(node, parent.node)
    return sz
        

def wxBoxSizer_builder(obj):
    """\
    function used to generate the python code for wxBoxSizer objects.
    """
    orient = obj.properties.get('orient', 'wxHORIZONTAL')
    init = ['self.%s = wxBoxSizer(%s)\n' % (obj.name, orient)]
    layout = []
    if obj.is_toplevel:
        if not obj.parent.is_toplevel: parent = 'self.%s' % obj.parent.name
        else: parent = 'self'
        layout.append('%s.SetAutoLayout(1)\n' % parent)
        layout.append('%s.SetSizer(self.%s)\n' % (parent, obj.name))
        if not obj.parent.properties.has_key('size'):
            layout.append('self.%s.Fit(%s)\n' % (obj.name, parent))
    return init, [], layout

def wxStaticBoxSizer_builder(obj):
    """\
    function used to generate the python code for wxStaticBoxSizer objects.
    """
    orient = obj.properties.get('orient', 'wxHORIZONTAL')
    label = obj.properties.get('label', '')
    if not obj.parent.is_toplevel: parent = 'self.%s' % obj.parent.name
    else: parent = 'self'
    init = ['self.%s = wxStaticBoxSizer(wxStaticBox(%s, -1, "%s"), %s)\n' %
            (obj.name, parent, label.replace('"', '\"'), orient)]
    layout = []
    if obj.is_toplevel:
        layout.append('%s.SetAutoLayout(1)\n' % parent)
        layout.append('%s.SetSizer(self.%s)\n' % (parent, obj.name))
        if not obj.parent.properties.has_key('size'):
            layout.append('self.%s.Fit(%s)\n' % (obj.name, parent))
    return init, [], layout

def _GridSizers_builder(obj, klass):
    props = obj.properties
    if not obj.parent.is_toplevel: parent = 'self.%s' % obj.parent.name
    else: parent = 'self'
    rows = props.get('rows', '0')
    cols = props.get('cols', '0')
    vgap = props.get('vgap', '0')
    hgap = props.get('hgap', '0')
    init = [ 'self.%s = %s(%s, %s, %s, %s)\n' % (obj.name, klass, rows, cols,
                                                 vgap, hgap) ]
    layout = []
    if obj.is_toplevel:
        layout.append('%s.SetAutoLayout(1)\n' % parent)
        layout.append('%s.SetSizer(self.%s)\n' % (parent, obj.name))
        if not obj.parent.properties.has_key('size'):
            layout.append('self.%s.Fit(%s)\n' % (obj.name, parent))
    return init, [], layout   

def wxGridSizer_builder(obj):
    """\
    function used to generate the python code for wxGridSizer objects.
    """
    return _GridSizers_builder(obj, 'wxGridSizer')

def wxFlexGridSizer_builder(obj):
    """\
    function used to generate the python code for wxFlexGridSizer objects.
    """
    return _GridSizers_builder(obj, 'wxFlexGridSizer')


def init_all():
    """\
    module initialization function: returns a list of buttons (to add to the
    main palette) to add the various sizers
    """
    cw = common.widgets
    cw['EditBoxSizer'] = builder
    cw['EditGridSizer'] = grid_builder

    cwx = common.widgets_from_xml
    cwx['EditBoxSizer'] = xml_builder
    cwx['EditStaticBoxSizer'] = xml_builder
    cwx['EditGridSizer'] = grid_xml_builder
    cwx['EditFlexGridSizer'] = grid_xml_builder

    cn = common.class_names
    cn['EditBoxSizer'] = 'wxBoxSizer'
    cn['EditStaticBoxSizer'] = 'wxStaticBoxSizer'
    cn['EditGridSizer'] = 'wxGridSizer'
    cn['EditFlexGridSizer'] = 'wxFlexGridSizer'

    from tree import WidgetTree
    WidgetTree.images['EditStaticBoxSizer'] = 'icons/sizer.xpm'
    WidgetTree.images['EditFlexGridSizer'] = 'icons/grid_sizer.xpm'

    # python code generation functions
    pygen = common.code_writers.get("python")
    if pygen:
        awh = pygen.add_widget_handler
        awh('wxBoxSizer', wxBoxSizer_builder)
        awh('wxStaticBoxSizer', wxStaticBoxSizer_builder)
        awh('wxGridSizer', wxGridSizer_builder)
        awh('wxFlexGridSizer', wxFlexGridSizer_builder)

    return [common.make_object_button('EditBoxSizer', 'icons/sizer.xpm'),
            common.make_object_button('EditGridSizer', 'icons/grid_sizer.xpm')]
