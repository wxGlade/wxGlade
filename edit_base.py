
import logging
import wx
import new_properties as np
import common, misc, compat, clipboard, config

MANAGED_PROPERTIES  = ["pos", "span", "proportion", "border", "flag"]

if config.debugging:
    class _UniqueList(list):
        def append(self, obj):
            if obj in self and obj is not None:
                raise AssertionError("Element already in list")
            list.append(self, obj)
        def insert(self, index, obj):
            if obj in self and obj is not None:
                raise AssertionError("Element already in list")
            list.insert(self, index, obj)
        def __setitem__(self, index, obj):
            if obj in self and obj is not None:
                raise AssertionError("Element already in list")
            list.__setitem__(self, index, obj)
else:
    _UniqueList = list


class EditBase(np.PropertyOwner):
    #is_sizer = False
    IS_TOPLEVEL = IS_SLOT = IS_SIZER = IS_WINDOW = IS_ROOT = IS_TOPLEVEL_WINDOW = False
    IS_CLASS = False  # generate class for this item; can be dynamically set during code generation
    WX_CLASS = None # needs to be defined in every derived class; e.g. "wxFrame", "wxBoxSizer", "wxPandel", "TopLevelPanel"
    #CHILDREN = 1  # 0 or a fixed number or None for e.g. a sizer with a variable number of children
    ATT_CHILDREN = None

    def __init__(self, name, parent, pos=None):
        assert self.WX_CLASS
        np.PropertyOwner.__init__(self)
        # initialise instance logger
        self._logger = logging.getLogger(self.__class__.__name__)

        self.widget = None          # this is the reference to the actual wxWindow widget, created when required
        self._dont_destroy = False  # for notebook pages, this will be set to True
        self.item = None            # the TreeCtrl item

        # initialise instance properties
        self.name  = np.NameProperty(name)
        if self.IS_TOPLEVEL: self.names = {}  # XXX change to set

        # initialise structure
        self.parent = parent
        if self.CHILDREN is None:
            # variable number of children
            self.children = _UniqueList([])
        elif self.CHILDREN:
            # fixed number of children
            self.children = _UniqueList([None]*self.CHILDREN)
        else:
            # no children
            self.children = None
        self.id = wx.NewId()  # id used for internal purpose events
        if isinstance(pos, str):
            setattr(self.parent, pos, self)
            self.pos = pos
        else:
            self.parent.add_item(self, pos)

        # the toplevel parent keeps track of the names
        self.toplevel_parent.names[name] = 1

    # tree navigation (parent and children) ############################################################################
    @property
    def toplevel_parent(self):
        # go up to parent until that's parent IS_ROOT
        item = self
        parent = item.parent
        while not parent.IS_ROOT:
            item = parent
            parent = item.parent
        return item

    @property
    def parent_window(self):
        # go up to parent until it is no sizer
        item = self.parent
        while True:
            if item.IS_WINDOW: return item
            item = item.parent
            if item is None: return None

    @property
    def toplevel_parent_window(self):
        # go up to parent until IS_TOPLEVEL is True
        item = self.parent
        while True:
            if item.IS_TOPLEVEL: return item
            item = item.parent

    @property
    def class_object(self):
        # used for code generation: the object for which a class code is generated
        item = self
        parent = item.parent
        while parent is not None:
            if not item.IS_SIZER and item.IS_CLASS: return item
            item = parent
            parent = item.parent
        return None

    @property
    def parent_class_object(self):
        # same as before, but start with parent
        item = self.parent
        parent = item.parent
        while parent is not None:
            if not item.IS_SIZER and item.IS_CLASS: return item
            item = parent
            parent = item.parent
        return None

    @property
    def sizer(self):
        # return the containing sizer or None
        if self.parent.IS_SIZER: return self.parent
        return None

    def get_all_children(self):
        ret = []
        if self.ATT_CHILDREN:
            for att in self.ATT_CHILDREN or []:
                child = getattr(self, att)
                if child is not None: ret.append(child)
        ret.extend(self.children or [])
        return ret

    def _get_child(self, pos):
        if pos is None and self.children:
            return self.children[0]
        if isinstance(pos, str) and pos in self.ATT_CHILDREN:
            return getattr(self, pos)
        if self.children and pos<=len(self.children):
            return self.children[pos]
        return self

    def _get_child_pos(self, child):
        # calculate pos including named attributes; this is the position in the tree view
        pos = 0
        if self.ATT_CHILDREN:
            for att in self.ATT_CHILDREN or []:
                c = getattr(self, att)
                if c is not None:
                    if child is c: return pos
                    pos += 1
        return pos + self.children.index(child)

    def add_item(self, child, pos=None):
        if pos is None:
            if self.CHILDREN is None:
                # variable number of children
                self.children.append(child)
                return
            # fixed number of children; fill first free position (a None or a Slot)
            assert self.CHILDREN
            if None in self.children:
                pos = self.children.index(None)
            else:
                old_slot = [c for c in self.children if c.IS_SLOT][0]
                common.app_tree.remove(old_slot)
                pos = old_slot.pos

        if len(self.children)<=pos:
            self.children += [None]*(pos - len(self.children) + 1)
        if self.children[pos] is not None:
            old_child = self.children[pos]
        else:
            old_child = None
        self.children[pos] = child
        if old_child:
            old_child.recursive_remove()

    def has_ancestor(self, node):
        "Returns True if node is parent or parents parent ..."
        parent = self.parent
        if parent is None: return False
        while True:
            if node is parent: return True
            if parent.parent is None: return False
            parent = parent.parent

    # property handling ################################################################################################
    @staticmethod
    def MOVE_PROPERTY(PROPERTIES, move_property, after_property):
        "move a property to another position, right behind after_property"
        PROPERTIES.remove( move_property )
        PROPERTIES.insert( PROPERTIES.index(after_property)+1, move_property )

    def properties_changed(self, modified):
        if modified and "name" in modified and self.properties["name"].previous_value is not None:
            previous_name = self.properties["name"].previous_value
            try:
                self.toplevel_parent.names[previous_name]
            except KeyError:
                if config.debugging: raise
            common.app_tree.refresh(self, refresh_label=True, refresh_image=False)
        elif not modified or "class" in modified or "name" in modified:
            common.app_tree.refresh(self, refresh_label=True, refresh_image=False)

    # widget creation and destruction ##################################################################################

    def create_widgets(self):
        "Shows the widget of the given node and all its children"
        self.create()
        if self.children:
            for c in self.children:
                c.create_widgets()
        self.post_load()  # SizerBase uses this for toplevel sizers; also EditNotebook

    def create(self):
        "create the wx widget"
        if not self.IS_TOPLEVEL and self.parent.widget is None: return
        #if self.parent is not None and self.parent.widget is None: return
        if self.widget: return
        self.create_widget()
        self.finish_widget_creation()

    def create_widget(self):
        "Initializes self.widget and shows it"
        raise NotImplementedError

    def finish_widget_creation(self, *args, **kwds):
        "Creates the popup menu and connects some event handlers to self.widgets"
        self.widget.Bind(wx.EVT_RIGHT_DOWN, self.popup_menu)

    def delete(self):
        """Destructor. deallocates the popup menu, the notebook and all the properties.
        Why we need explicit deallocation? Well, basically because otherwise we get a lot of memory leaks... :)"""
        # XXX tell property editor
        self.destroy_widget()
        if misc.focused_widget is self:
            misc.focused_widget = None

    def destroy_widget(self):
        if not self.widget or self._dont_destroy: return
        if self.parent.IS_SIZER and self.parent.widget:
            self.parent.widget.Detach(self.widget)  # remove from sizer without destroying
        compat.DestroyLater(self.widget)
        self.widget = None

    # from tree.Tree ###################################################################################################
    def recursive_remove(self):
        # recursively remove from parent, delete widget, remove from Tree (build to be called separately)
        if self.children:
            for c in self.get_all_children():
                c.recursive_remove()
        try:
            self.parent.children.remove(self)
        except:
            print(" **** node_remove failed")  # XXX  might have been overwritten in Slot -> add_item
            pass
        self.delete()
        common.app_tree.remove(self)  # remove mutual reference from widget to/from Tree item
        if not self.IS_SLOT and self.name:
            try:
                del self.toplevel_parent.names[self.name]
            except:
                print("XXX delete: name '%s' already removed"%self.name)

    ####################################################################################################################

    # XXX check this
    def remove(self, *args):
        # entry point from GUI
        common.root.saved = False  # update the status of the app
        # remove is called from the context menus; for other uses, delete is applicable
        self._dont_destroy = False  # always destroy when explicitly asked
        self.recursive_remove()
        misc.rebuild_tree(self.parent, recursive=False, focus=True)

    # XML generation ###################################################################################################
    def write(self, output, tabs, class_names=None):
        "Writes the xml code for the widget to the given output file"
        # write object tag, including class, name, base
        classname = getattr(self, '_classname', self.__class__.__name__)
        # to disable custom class code generation (for panels...)
        if getattr(self, 'no_custom_class', False):
            no_custom = u' no_custom_class="1"'
        else:
            no_custom = ""
        outer_tabs = u'    ' * tabs
        output.append(u'%s<object %s %s %s%s>\n' % ( outer_tabs,
                                                     common.format_xml_attrs(**{'class': self.klass}),
                                                     common.format_xml_attrs(name=self.name),
                                                     common.format_xml_attrs(base=classname),
                                                     no_custom) )

        # write properties, but without name and class
        # XXX be 100% compatible to 0.7.2, where option is written into the object; remove later
        properties = self.get_properties(without=set(MANAGED_PROPERTIES))
        for prop in properties:
            prop.write(output, tabs+1)

        if class_names is not None and self.__class__.__name__ != 'CustomWidget':
            class_names.add(self.klass)

        if self.IS_SIZER:
            for child in self.children or []:
                if not child.IS_SLOT:
                    inner_xml = []

                    for name in MANAGED_PROPERTIES:
                        name = child.properties[name]
                        if name is not None:
                            name.write(inner_xml, tabs+2)

                    child.write(inner_xml, tabs+2, class_names)
                    stmt = common.format_xml_tag( u'object', inner_xml, tabs+1,
                                                  is_xml=True, **{'class': 'sizeritem'} )
                    output.extend(stmt)
                else:
                    child.write(output, tabs+1)
        elif self.children is not None or self.ATT_CHILDREN is not None:
            for child in self.get_all_children():
                assert not config.debugging or child is not None
                child.write(output, tabs+1, class_names)
        output.append(u'%s</object>\n' % outer_tabs)

    # XML loading and slot handling ####################################################################################
    def _add_slots(self, pos_max=None):
        "replace None with Slot"
        for pos, child in enumerate(self.children):
            if child is None:
                if pos_max is not None and pos>pos_max: continue
                Slot(self, pos)

    def on_load(self):
        "called from XML parser, right after the widget is loaded; children have been loaded already"
        if not self.CHILDREN is 0:
            self._add_slots()

    def post_load(self):
        """Called after the loading of an app from a XML file, before showing the hierarchy of widget for the first time.
        The default implementation does nothing."""
        pass

    def free_slot(self, pos, force_layout=True):
        "Replaces the element at pos with an empty slot"
        # called from ManagedBase context menu when removing an item
        slot = self._free_slot(pos, force_layout)
        misc.rebuild_tree(slot)

    def _free_slot(self, pos, force_layout=True):
        with self.toplevel_parent.frozen():
            old_child = self.children[pos]
            slot = Slot(self, pos)
            #old_child.tree_remove()
            #common.app_tree.remove(old_child)
            if self.widget:
                slot.create()  # create the actual SizerSlot as wx.Window with hatched background
        return slot

    # for tree display #################################################################################################
    def _get_tree_label(self):
        # get a label for node
        s = self.name
        if (self.__class__.__name__=="CustomWidget" or
            (self.klass != self.base and self.klass != 'wxScrolledWindow') ):
            # special case...
            s += ' (%s)' % self.klass
            if getattr(self, "has_title", None):
                # include title
                s += ': "%s"'%self.title
        elif "label" in self.properties and self.properties["label"].is_active():
            # include label of control
            label = self.label
            label = label.replace("\n","\\n").replace("\t","\\t")
            if '"' in label:
                if len(label)>36:
                    s += ": '%s..."%(label[:30])
                else:
                    s += ": '%s'"%label
            else:
                if len(label)>24:
                    s += ': "%s...'%(label[:30])
                else:
                    s += ': "%s"'%label
        elif getattr(self, "has_title", None):
            # include title
            s += ': "%s"'%self.title
        elif getattr(self, "parent", None) and self.parent.klass=="wxNotebook":
            # notebook pages: include page title: "[title] name"
            notebook = self.parent
            if self in notebook.children:
                title = notebook.tabs[notebook.children.index(self)][0]
                s = '[%s] %s'%(title, s)
        return s

    def _get_tree_image(self):
        "Get an image name for tree display"
        name = self.__class__.__name__
        return name

    def _label_editable(self):
        if not "name" in self.properties: return False
        if not "label" in self.properties: return True
        label = self.label
        # no editing in case of special characters
        if "\n" in label or "\t" in label or "'" in label or '"' in label: return False
        if len(label)>24: return False
        return True


class Slot(EditBase):
    "A window to represent an empty slot, e.g. single slot of a Frame or a page of a Notebook"
    PROPERTIES = ["Slot", "pos"]
    IS_TOPLEVEL = IS_SIZER = IS_WINDOW = False
    IS_SLOT = True
    CHILDREN = 0

    def __init__(self, parent, pos=0, label=None):
        # XXX unify with EditBase.__init__ ?
        assert isinstance(pos, int)
        assert not parent.IS_SLOT
        np.PropertyOwner.__init__(self)
        # initialise instance logger
        self._logger = logging.getLogger(self.__class__.__name__)
        self.klass = self.classname = self.base = "slot"
        self.label = label

        # initialise instance properties
        self.widget = None          # Reference to the widget resembling the slot (a wx.Window)
        self._dont_destroy = False  # for notebook pages, this will be set to True
        self.name = "SLOT"
        self.overlapped = False  # for spanning in GridBagSizer
        self.item = None

        # initialise structure
        self.parent = parent
        self.children = None
        self.parent.add_item(self, pos)
        self.pos = np.LayoutPosProperty(pos)  # position within the sizer or 0

        # the following are just set to use the same Add call as with widgets
        self.proportion = 1
        self.span = (1,1)
        self.flag = wx.EXPAND
        self.border = 0

    def update_view(self, selected):
        # we can ignore selected here, as the repainting only takes place later
        if self.widget:
            self.widget.Refresh()

    def create_widget(self):
        style = wx.FULL_REPAINT_ON_RESIZE
        if self.parent.CHILDREN==1:  # e.g. Panel in a Frame
            size = self.parent.widget.GetClientSize()
        else:
            size=(20, 20)
        self.widget = wx.Window(self.parent_window.widget, -1, size=size, style=style)
        self.widget.SetBackgroundStyle(wx.BG_STYLE_CUSTOM)
        #self.widget.SetAutoLayout(True)
        self.widget.Bind(wx.EVT_PAINT, self.on_paint)
        self.widget.Bind(wx.EVT_ERASE_BACKGROUND, self.on_erase_background)
        self.widget.Bind(wx.EVT_RIGHT_DOWN, self.popup_menu)
        self.widget.Bind(wx.EVT_LEFT_DOWN, self.on_drop_widget)
        self.widget.Bind(wx.EVT_MIDDLE_DOWN, misc.exec_after(self.on_select_and_paste))
        self.widget.Bind(wx.EVT_ENTER_WINDOW, self.on_enter)
        self.widget.Bind(wx.EVT_LEAVE_WINDOW, self.on_leave)
        #self.widget.Bind(wx.EVT_CHAR_HOOK, misc.on_key_down_event)  # catch cursor keys   XXX still required?

    def is_visible(self):
        return False

    def on_enter(self, event):
        # hack. definitely. but...
        misc.currently_under_mouse = self.widget
        # a sizer can be added to sizers or to windows with exactly one child
        can_add_sizer = self.parent.IS_SIZER or self.parent.CHILDREN is 1
        if common.adding_widget and (not common.adding_sizer or can_add_sizer):
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
        #if not self.sizer: return  # in deletion
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
        #if self.parent.IS_TOPLEVEL:
            ## frame, panel, dialog
            #size = self.parent.widget.GetClientSize()
            #self.widget.SetSize( size )
        #else:
        size = self.widget.GetSize()
        small = size[0]<10 or size[1]<10
        focused = misc.focused_widget is self
        if clear:
            if small and focused:
                dc.SetBackground(wx.Brush(wx.BLUE))
            else:
                dc.SetBackground(wx.Brush(wx.LIGHT_GREY))
            dc.Clear()
        if small and focused:
            color = wx.WHITE
        elif small or not focused:
            color = wx.BLACK
        else:
            color = wx.BLUE

        if focused:
            hatch = compat.BRUSHSTYLE_CROSSDIAG_HATCH
        elif not self.parent.IS_SIZER:
            hatch = compat.BRUSHSTYLE_FDIAGONAL_HATCH
        else:
            if not "cols" in self.parent.PROPERTIES:  # horizontal/vertical sizer or grid sizer?
                pos = self.pos
            else:
                pos = sum( self.sizer._get_row_col(self.pos) )
            hatch = compat.BRUSHSTYLE_FDIAGONAL_HATCH  if pos%2 else  compat.BRUSHSTYLE_BDIAGONAL_HATCH
        brush = wx.Brush(color, hatch)
        # draw hatched lines in foreground
        dc.SetBrush(brush)
        size = self.widget.GetClientSize()
        dc.DrawRectangle(0, 0, size.width, size.height)

    #def set_size(self, size):
        #if not self.widget: return
        #size = self.parent.widget.GetClientSize()
        #self.widget.SetSize( size )

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
        menu.Destroy()

    def _create_popup_menu(self, widget):
        # menu title
        if self.parent.IS_SIZER and "cols" in self.parent.properties:
            rows, cols = self.parent._get_actual_rows_cols()
            # calculate row and pos of our slot
            row,col = self.parent._get_row_col(self.pos)
            menu = wx.Menu(_("Slot %d/%d"%(row+1,col+1)))
        elif "pos" in self.properties:
            menu = wx.Menu(_("Slot %d"%self.pos))
        else:
            menu = wx.Menu(_("Slot"))

        # edit: paste
        i = misc.append_menu_item(menu, -1, _('Paste\tCtrl+V'), wx.ART_PASTE)
        misc.bind_menu_item_after(widget, i, clipboard.paste, self)
        if not clipboard.check("widget","sizer"): i.Enable(False)
        menu.AppendSeparator()

        # slot actions
        if self.parent.IS_SIZER:
            if not "cols" in self.parent.properties:
                i = misc.append_menu_item(menu, -1, _('Remove Slot\tDel'), wx.ART_DELETE)
                misc.bind_menu_item_after(widget, i, self.remove)
                if len(self.parent.children)<=2: i.Enable(False)
            else:
                # if inside a grid sizer: allow removal of empty rows/cols
                # check whether all slots in same row/col are empty
                row_is_empty = col_is_empty = True
                for pos,child in enumerate(self.parent.children):
                    child_row, child_col = self.parent._get_row_col(pos)
                    if child_row==row and not child.IS_SLOT:
                        row_is_empty = False
                    if child_col==col and not child.IS_SLOT:
                        col_is_empty = False

                # allow removal of empty row
                i = misc.append_menu_item(menu, -1, _('Remove Row %d'%(row+1)) )
                misc.bind_menu_item_after(widget, i, self.parent.remove_row, self.pos)
                if not row_is_empty or rows<=1: i.Enable(False)

                # allow removal of empty col
                i = misc.append_menu_item(menu, -1, _('Remove Column %d'%(col+1)) )
                misc.bind_menu_item_after(widget, i, self.parent.remove_col, self.pos)
                if not col_is_empty or cols<=1: i.Enable(False)
                menu.AppendSeparator()

            self.parent._add_popup_menu_items(menu, self, widget)

        p = self.toplevel_parent_window # misc.get_toplevel_widget(self.sizer)
        #if p is not None and p.preview_is_visible():
        if p.preview_is_visible():
            item = _('Close preview (%s)\tF5') % p.name
        else:
            item = _('Preview (%s)\tF5') % p.name

        i = misc.append_menu_item( menu, -1, item )
        #misc.bind_menu_item_after(widget, i, self.preview_parent)
        misc.bind_menu_item_after(widget, i, p.preview)

        return menu

    ####################################################################################################################
    def _remove(self):
        # does not set focus
        if not self.parent.IS_SIZER: return
        with self.toplevel_parent_window.frozen():
            self.parent.remove_item(self)  # replaces self.parent.children[pos] and detaches also widget from sizer
            self.destroy_widget(detach=False)  # self.delete() would be OK, but would detach again...
            common.app_tree.remove(self)  # destroy tree leaf

    def remove(self):
        # entry point from GUI
        common.root.saved = False  # update the status of the app
        # set focused widget
        i = self.pos - 1
        self._remove()
        if i >= len(self.parent.children):
            i = len(self.parent.children)-1
        misc.rebuild_tree(self.parent, recursive=False, focus=False)
        if i>=0:
            misc.set_focused_widget( self.parent.children[i] )
        else:
            misc.set_focused_widget( self.parent )

    def on_drop_widget(self, event):
        """replaces self with a widget in self.sizer. This method is called
        to add every non-toplevel widget or sizer, and in turn calls the
        appropriate builder function (found in the ``common.widgets'' dict)"""
        if not common.adding_widget:  # widget focused/selecte
            misc.set_focused_widget(self)
            if self.widget:
                self.widget.Refresh()
                self.widget.SetFocus()
            return
        if common.adding_sizer and self.parent.CHILDREN is not 1 and not self.IS_SLOT:
            return
        if self.widget:
            self.widget.SetCursor(wx.NullCursor)
        common.adding_window = event and event.GetEventObject().GetTopLevelParent() or None
        # call the appropriate builder
        new_widget = common.widgets[common.widget_to_add](self.parent, self.pos)
        if new_widget is None: return
        misc.rebuild_tree(new_widget)
        if event is None or not misc.event_modifier_copy(event):
            common.adding_widget = common.adding_sizer = False
            common.widget_to_add = None

    def check_drop_compatibility(self):
        if common.adding_sizer and self.parent.CHILDREN is not 1 and not self.IS_SLOT:
            return (False, "No sizer can be added here")
        return (True,None)

    # clipboard handling ###############################################################################################
    def check_compatibility(self, widget, typename=None):
        "check whether widget can be pasted here"
        if typename is not None:
            if typename=="sizer" and self.parent.CHILDREN is not 1:
                return (False, "No sizer can be pasted here")
            if typename=="window":
                return (False, "No toplevel object can be pasted here.")
            return (True,None)

        if widget.IS_TOPLEVEL:
            return (False, "No toplevel object can be pasted here.")
        if self.parent.CHILDREN is not 1 and widget.IS_SIZER:
            # e.g. a sizer dropped on a splitter window slot; instead, a panel would be required
            return (False, "No sizer can be pasted here")
        return (True,None)

    def clipboard_paste(self, clipboard_data):
        "Insert a widget from the clipboard to the current destination"
        return clipboard._paste(self.parent, self.pos, clipboard_data)

    def on_select_and_paste(self, *args):
        "Middle-click event handler: selects the slot and, if the clipboard is not empty, pastes its content here"
        misc.focused_widget = self
        self.widget.SetFocus()
        clipboard.paste(self)
    ####################################################################################################################

    #def delete(self):
        ## mainly deletes the widget
        #self.destroy_widget()
        #common.root.saved = False

    def destroy_widget(self, detach=True):
        if self.widget is None: return
        if misc.currently_under_mouse is self.widget:
            misc.currently_under_mouse = None

        if self._dont_destroy: return  # on a notebook page
        self.widget.Hide()

        # unbind events to prevent new created (and queued) events
        self.widget.Bind(wx.EVT_PAINT, None)
        self.widget.Bind(wx.EVT_RIGHT_DOWN, None)
        self.widget.Bind(wx.EVT_LEFT_DOWN, None)
        self.widget.Bind(wx.EVT_MIDDLE_DOWN, None)
        self.widget.Bind(wx.EVT_ENTER_WINDOW, None)
        self.widget.Bind(wx.EVT_LEAVE_WINDOW, None)
        self.widget.Bind(wx.EVT_KEY_DOWN, None)
        if detach and self.parent.IS_SIZER and self.parent.widget:
            self.parent.widget.Detach(self.widget)  # this will happen during recursive removal only
        compat.DestroyLater(self.widget)
        self.widget = None

        if misc.focused_widget is self:
            misc.set_focused_widget(None)

    def write(self, output, tabs, class_names=None):
        return

    def _get_tree_label(self):
        if self.label: return str(self.label)
        if self.parent.CHILDREN==1: return "SLOT"
        pos = self.pos
        if hasattr(self.parent, "_get_slot_label"):
            return self.parent._get_slot_label(pos)
        if self.parent.IS_SIZER and "cols" in self.parent.properties:
            # grid sizer: display row/col
            rows, cols = self.parent._get_actual_rows_cols()
            row = pos // cols + 1  # 1 based at the moment
            col = pos %  cols + 1
            return "SLOT  %d/%d"%(row, col)
        else:
            return "SLOT %d"%(pos)

    def _get_tree_image(self):
        "Get an image name for tree display"
        if self.parent.WX_CLASS=="wxSplitterWindow":
            if self.parent.orientation=="wxSPLIT_VERTICAL":
                name = 'EditSplitterSlot-Left'  if self.pos==1 else  'EditSplitterSlot-Right'
            else:
                name = 'EditSplitterSlot-Top'   if self.pos==1 else  'EditSplitterSlot-Bottom'
            return name

        if not self.parent.IS_SIZER: return "EditSlot"
        name = "EditSizerSlot"
        if "orient" in self.parent.properties:
            sizer_orient = self.parent.orient
            if sizer_orient is not None:
                if sizer_orient==wx.VERTICAL:
                    name = "EditVerticalSizerSlot"
                elif sizer_orient==wx.HORIZONTAL:
                    name = "EditHorizontalSizerSlot"
        return name

