
import wx
import new_properties as np
import common, misc, compat, clipboard, config

MANAGED_PROPERTIES  = ["span", "proportion", "border", "flag"]

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
                if obj is self[index]: return
                raise AssertionError("Element already in list")
            list.__setitem__(self, index, obj)
else:
    _UniqueList = list


class EditBase(np.PropertyOwner):
    IS_TOPLEVEL = IS_SLOT = IS_SIZER = IS_WINDOW = IS_ROOT = IS_TOPLEVEL_WINDOW = IS_CONTAINER = False
    IS_CLASS = None  # dynamically set during code generation if a class is generated for this item
    # usually this one is fixed, but EditPanel/EditToplevelPanel will overwrite it depending on the "scrollable" property
    WX_CLASS = None # needs to be defined in every derived class; e.g. "wxFrame", "wxBoxSizer", "TopLevelPanel"
    WX_CLASSES = None  # used if WX_CLASS can be changed dynamically
    WXG_BASE = None # usually None, but if defined, it will be written to the wxg file instead of the editor class name
    IS_NAMED = True  # default, only False for Spacer
    #CHILDREN = 1  # 0 or a fixed number or None for e.g. a sizer with a variable number of children; -1 for 0 or 1
    ATT_CHILDREN = None
    TREE_ICON = None  # defaults to editor class name

    def __init__(self, name, parent, index):
        assert self.WX_CLASS
        np.PropertyOwner.__init__(self)

        self.widget = None          # this is the reference to the actual wxWindow widget, created when required
        self.item = None            # the TreeCtrl item

        # initialise instance properties
        self.name = np.NameProperty(name)

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
        if isinstance(index, str):
            setattr(self.parent, index, self)
            self.attribute_name = index
        else:
            self.parent.add_item(self, index)

        # the toplevel parent keeps track of the names ( see next two methods ...contained_name() )
        if self.IS_TOPLEVEL:
            # either derived from edit_windows.TopLevelBase or a toplevel Menu/ToolBar where IS_TOPLEVEL is set True
            self.names = set([self.name])
            self._NUMBERS = {}  # for finding new names
        elif self.IS_NAMED:
            self.toplevel_parent.track_contained_name( new_name=name )

    # manage names of contained elements ###############################################################################
    # actually, this might be too strict if contained elements are their own classes
    def get_next_contained_name(self, fmt, exclude=None):
        # get a name that is not yet used for one of the children inside
        # will only be used for IS_TOPLEVEL==True; currently only by TopLevelBase; in future maybe for toolbars as well
        number = self._NUMBERS.get(fmt, 1)
        while True:
            name = fmt % number
            if not name in self.names and (not exclude or not name in exclude):
                self._NUMBERS[fmt] = number
                return name
            number += 1

    def track_contained_name(self, old_name=None, new_name=None):
        # only for named elements (IS_NAMED==True)
        #  to remove: new_name=None
        #  to add:    old_name=None
        # EditDialog also uses this to track names for "affirmative" and "escape" properties
        #print("track_contained_name", old_name,new_name, self.names)
        if old_name is not None:
            try:
                self.names.remove( old_name )
            except KeyError:
                pass
        if new_name is not None: self.names.add( new_name )

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

    #@property
    #def class_object(self):
        ## used for code generation: the object for which a class code is generated
        #item = self
        #parent = item.parent
        #while parent is not None:
            #if not item.IS_SIZER and item.IS_CLASS: return item
            #item = parent
            #parent = item.parent
        #return None

    @property
    def parent_class_object(self):
        # same as before, but start with parent
        # used only for XRC code generation, so IS_CLASS is set
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

    @property
    def top_sizer(self):
        # return the top containing sizer or None
        if not self.parent.IS_SIZER:
            if self.IS_SIZER: return self
            return None
        item = self.parent
        while item.parent.IS_SIZER:
            item = item.parent
        return item


    def get_all_children(self):
        # this always returns a copy, as it might be used by recursive_remove
        ret = []
        if self.ATT_CHILDREN:
            for att in self.ATT_CHILDREN or []:
                child = getattr(self, att)
                if child is not None: ret.append(child)
        ret.extend(self.children or [])
        return ret

    def _get_child(self, index):
        if index is None and self.children:
            return self.children[0]
        if isinstance(index, str) and index in self.ATT_CHILDREN:
            return getattr(self, index)
        if self.children and index<=len(self.children):
            return self.children[index]
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
    
    def find_children(self, name=None, wx_class=None):
        ret = []
        for child in self.get_all_children():
            if not child: continue
            ret += child.find_children(name, wx_class)
            if name is not None and child.name!=name: continue
            if wx_class is not None and child.WX_CLASS!=wx_class: continue
            ret.append(child)
        return ret

    ####################################################################################################################
    @property
    def index(self):
        # index or attribute name
        attribute_name = getattr(self, "attribute_name", None)
        if attribute_name is not None: return attribute_name
        return self.parent.children.index(self)

    def add_item(self, child, index=None):
        if index is None:
            # happens during loading or pasting
            if self.CHILDREN is None:
                # variable number of children
                self.children.append(child)
                return
            # fixed number of children; fill first free position (a None or a Slot)
            assert self.CHILDREN
            if None in self.children:
                index = self.children.index(None)
            elif self.CHILDREN in (-1,1):
                index = 0

        if len(self.children)<=index:
            self.children += [None]*(index - len(self.children) + 1)
        if self.children[index] is not None:
            self.children[index].recursive_remove(0, keep_slot=True)
        self.children[index] = child

    def insert_item(self, child, index):
        # for now only for child=None as placeholder; used by notebook
        self.children.insert(index, child)

    def remove_item(self, child, level, keep_slot=False):
        "Removes child from self and adjust pos of following items"
        if not child: return
        if child in self.children:
            index = self.children.index(child)
            if keep_slot:
                self.children[index] = None
            else:
                del self.children[index]
            return
        if hasattr(child, "attribute_name"):
            setattr(self, child.attribute_name, None)
            return
        raise ValueError("Internal error")

    def has_ancestor(self, editor):
        "Returns True if editor is parent or parents parent ..."
        parent = self.parent
        if parent is None: return False
        while True:
            if editor is parent: return True
            if parent.parent is None: return False
            parent = parent.parent
    
    def get_path(self):
        """returns a list of widget names, from the toplevel to the selected one
        Example: ['frame_1', 'sizer_1', 'panel_1', 'sizer_2', 'button_1']
                 if button_1 is the currently selected widget"""
        ret = []
        w = self
        while w:
            if w.IS_SLOT:
                ret.append("SLOT %d"%w.index)
            else:
                ret.append(w.name)
            w = w.parent
        ret.reverse()
        return "/".join(ret)

    # property handling ################################################################################################
    @staticmethod
    def MOVE_PROPERTY(PROPERTIES, move_property, after_property):
        "move a property to another position, right behind after_property"
        PROPERTIES.remove( move_property )
        PROPERTIES.insert( PROPERTIES.index(after_property)+1, move_property )

    def _properties_changed(self, modified, actions):
        if modified and "name" in modified and self.properties["name"].previous_value is not None:
            if config.debugging or config.testing:
                assert self.IS_NAMED
            old_name = self.properties["name"].previous_value or None
            self.toplevel_parent.track_contained_name(old_name, self.name)

        if modified and ("name" in modified or "label" in modified or "title" in modified):
            actions.add("label")

    def properties_changed(self, modified):
        actions = np.PropertyOwner.properties_changed(self, modified)

        if common.app_tree is not None and ("label" in actions or "image" in actions):
            common.app_tree.refresh(self, refresh_label=("label" in actions), refresh_image=("image" in actions))

        return actions

    # widget creation and destruction ##################################################################################
    def create(self):
        # entry point to create widget including all children
        self.recursive_create_widgets(level=0)
        self.layout()

    def recursive_create_widgets(self, level):
        self.create_widget()
        self.finish_widget_creation(level)
        for child in self.get_all_children():
            child.recursive_create_widgets(level+1)
        self.child_widgets_created(level)  # if level==0, only one of the child widgets was created
        self.parent.child_widget_created(self, level)

    def layout(self):
        # called once after all widgets incl. children were created or e.g. layout property modified
        # before 2020-08-10 the ClipboardXmlWidgetBuilder.endElement() had code with
        #  SafeYield, layout, Refresh, GetTopLevelParent().SendSizeEvent()
        self.widget.Layout()
        if self.IS_TOPLEVEL: return
        if self.IS_WINDOW: self.widget.SendSizeEvent()
        parent_window = self.parent_window
        parent_window.widget.SendSizeEvent()
        if ( hasattr(self.widget, "SendSizeEvent") and not self.IS_SIZER and
             (wx.Platform == '__WXGTK__' or not self.parent.IS_SIZER) ):
            # following is required for
            # - gtk when e.g. pasting a button or label with non-standard font; not for Windows or Mac Os
            # - adding e.g. a grid or text control to a frame (without a sizer); not for panel
            wx.SafeYield()
            compat.wxWindow_SendSizeEventToParent(self.widget)
        # following is required when e.g. adding a slot or widget to a sizer on a panel in a sizer
        compat.wxWindow_SendSizeEventToParent(self.parent_window.widget)

    # actual widget creation
    def create_widget(self):
        "Initializes self.widget and shows it"
        raise NotImplementedError

    def finish_widget_creation(self, level):
        "Binds the popup menu handler and connects some event handlers to self.widgets; set tooltip string"
        if not self.widget: return
        self.widget.Bind(wx.EVT_RIGHT_DOWN, self.popup_menu)
        if self.WX_CLASS in ("wxStatusBar",): return
        compat.SetToolTip(self.widget, self._get_tooltip_string())

    # callbacks when children were created
    def child_widget_created(self, child, level):
        # called after child's children were also created; implemented for notebook, splitter, sizers
        pass
    
    def child_widgets_created(self, level):
        # implemented for notebook, splitter, sizers?
        pass

    # actual widget destruction, called from recursive_remove
    def destroy_widget(self, level, later=True):
        # just destroy the widget; all bookkeeping / data structure update is done in recursive_remove
        # level is 0 for toplevel or when the user just deletes this one
        if not self.widget: return
        if later:
            compat.DestroyLater(self.widget)
        else:
            self.widget.Destroy()
        self.widget = None

    def destroying_child_widget(self, child, index):
        # called before a child widget is destroyed; e.g. used by splitter to unsplit
        # child has been removed from self.children already,
        #  except when a widget is re-created or a slot is just set to overlapped
        # index can be an index or an attribute name
        pass

    def destroyed_child_widget(self):
        pass

    ####################################################################################################################
    def recursive_remove(self, level, keep_slot=False):
        "recursively remove children and then self from parent; delete widget; remove from tree and do bookkeeping"
        # this is not a GUI entry point, see remove() for this!
        index = self.index

        # recursively remove children
        if self.children:
            for child in self.get_all_children():
                if child is None: continue  # this might happen during loading when a widget type is not supported
                child.recursive_remove(level+1)

        self.parent.remove_item(self, level, keep_slot)

        if level==0 and self.widget:
            self.parent.destroying_child_widget(self, index)
            self.destroy_widget(level)
            self.parent.destroyed_child_widget()

        # remove from Tree (rebuild_tree to be called separately)
        if misc.focused_widget is self: misc.focused_widget = None
        if common.app_tree is not None:
            common.app_tree.remove(self)  # remove mutual reference from widget to/from Tree item

        # bookkeeping
        if not self.IS_TOPLEVEL and self.IS_NAMED and self.name:
            self.toplevel_parent.track_contained_name( self.name )

    def remove(self, focus=True, user=True):
        # entry point from GUI or script
        if user:
            common.history.widget_removing(self)
            common.root.saved = False   # update the status of the app
        self.recursive_remove(level=0)
        misc.rebuild_tree(self.parent, recursive=False, focus=focus)
        if user: common.history.widget_removed(None)

    # XML generation ###################################################################################################
    def get_editor_name(self):
        # the panel classes will return something else here, depending on self.scrollable
        return self.WXG_BASE or self.__class__.__name__
    def write(self, output, tabs):
        "Writes the xml code for the widget to the given output file"
        # write object tag, including class, name, base
        classname = self.get_editor_name()
        # to disable custom class code generation (for panels...)
        outer_tabs = u'    ' * tabs
        instance_class = ''
        if "class" in self.properties:
            klass = self.get_prop_value("class", default=self.WX_CLASS)
            if self.check_prop_truth("instance_class"):
                instance_class = " " + common.format_xml_attrs(instance_class=self.instance_class)
        else:
            klass = self.get_prop_value("instance_class", default=self.WX_CLASS)
        output.append(u'%s<object %s %s %s%s>\n' % ( outer_tabs,
                                                     common.format_xml_attrs(**{'class': klass}),
                                                     common.format_xml_attrs(name=self.name),
                                                     common.format_xml_attrs(base=classname),
                                                     instance_class) )

        if config.debugging and getattr(self, "_restore_properties", None):
            raise ValueError("properties not restored")
        self.restore_properties()
        # write properties, but without name and class
        # XXX be 100% compatible to 0.7.2, where option is written into the object; remove later
        properties = self.get_properties(without=set(MANAGED_PROPERTIES))
        for prop in properties:
            prop.write(output, tabs+1)

        if self.IS_SIZER:
            for child in self.children or []:
                if not child.IS_SLOT:
                    inner_xml = []

                    for name in MANAGED_PROPERTIES:
                        name = child.properties[name]
                        if name is not None:
                            name.write(inner_xml, tabs+2)

                    child.write(inner_xml, tabs+2)
                    stmt = common.format_xml_tag( u'object', inner_xml, tabs+1,
                                                  is_xml=True, **{'class': 'sizeritem'} )
                    output.extend(stmt)
                else:
                    child.write(output, tabs+1)
        elif self.children is not None or self.ATT_CHILDREN is not None:
            for child in self.get_all_children():
                assert not config.debugging or child is not None
                child.write(output, tabs+1)
        output.append(u'%s</object>\n' % outer_tabs)

    # XML loading and slot handling ####################################################################################
    def _add_slots(self, max_index=None):
        "replace None with Slot"
        for index, child in enumerate(self.children):
            if child is None:
                if max_index is not None and index>max_index: continue
                Slot(self, index)

    def _add_slot(self):
        # used when loading or pasting an (optional) slot to a panel
        self.children.append(None)
        self._add_slots()  # replace the None with a slot

    def on_load(self, child=None):
        "called from XML parser, right after the widget is loaded; children have been loaded already"
        # when a child has been pasted in, it's also called, with argument child
        if self.CHILDREN != 0:
            self._add_slots()

    def free_slot(self, index, force_layout=True):
        "Replaces the element at index with an empty slot"
        # called from ManagedBase context menu when removing an item
        slot = self._free_slot(index, force_layout)
        misc.rebuild_tree(slot)

    def _free_slot(self, index, force_layout=True):
        #with self.toplevel_parent.frozen():  # this does not work on mac os: when deleting a panel notebook page, it will remain black
        slot = Slot(self, index)
        if self.widget: slot.create()  # create the actual SizerSlot as wx.Window with hatched background
        return slot

    # for tree and help display ########################################################################################
    def _get_tree_label(self):
        # get a label for node
        s = self.name
        if self.WX_CLASS=="CustomWidget":
            s += ' (%s)' % self.instance_class
        elif self.check_prop("class"):
            s += ' (%s)' % self.klass
            if getattr(self, "has_title", None):
                # include title
                s += ': "%s"'%self.title
        elif self.check_prop("stockitem"):
            s = "%s: %s"%(s, self.stockitem)
        elif self.check_prop("label"):
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
        elif self.parent.WX_CLASS=="wxNotebook":
            # notebook pages: include page title: "[title] name"
            notebook = self.parent
            if self in notebook.children:
                title = notebook.tabs[notebook.children.index(self)][0]
                s = '[%s] %s'%(title, s)
        return s

    def _get_tree_image(self):
        "Get an image name for tree display"
        return self.TREE_ICON or self.__class__.__name__

    def _label_editable(self):
        # returns True if the label can be edited in the Tree ctrl
        if not "name" in self.properties: return False
        if not "label" in self.properties: return True
        label = self.label
        # no editing in case of special characters
        if "\n" in label or "\t" in label or "'" in label or '"' in label: return False
        if len(label)>24: return False
        return True

    def _get_tooltip_string(self):
        # get tooltip string: first (optional) part from parent, second from ourself
        # used as tooltip for the widget in the Design window and also for the status bar of the Palette
        tooltip = []
        if self.parent.children and self in self.parent.children:
            tooltip.append( self.parent._get_parent_tooltip(self.index) )
        tooltip.append( self._get_tooltip() )
        return "\n".join(s for s in tooltip if s)

    def _get_parent_tooltip(self, index):
        # called for a child; e.g. Splitter pane may return "Left spliter pane:" for pos=0
        return None

    def _get_tooltip(self):
        return None


class Slot(EditBase):
    "A window to represent an empty slot, e.g. single slot of a Frame or a page of a Notebook"
    PROPERTIES = ["Slot", "info"]
    IS_TOPLEVEL = IS_SIZER = IS_WINDOW = False
    IS_SLOT = True
    IS_NAMED = False
    CHILDREN = 0

    def __init__(self, parent, index, label=None):
        # XXX unify with EditBase.__init__ ?
        assert isinstance(index, int)
        assert not parent.IS_SLOT
        np.PropertyOwner.__init__(self)
        self.klass = self.classname = "slot"
        self.label = label

        # initialise instance properties
        self.widget = None          # Reference to the widget resembling the slot (a wx.Window)
        self.name = "SLOT"
        self.overlapped = False  # for spanning in GridBagSizer
        self.item = None

        # initialise structure
        self.parent = parent
        self.children = None
        self.parent.add_item(self, index)

        # display some help
        self.info = np.DisplayProperty(self._get_tooltip())

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
        if self.overlapped and self.parent._IS_GRIDBAG: return
        style = wx.FULL_REPAINT_ON_RESIZE
        if self.parent.CHILDREN in (-1, 1):  # e.g. Panel in a Frame
            size = self.parent.widget.GetClientSize()
        else:
            size = (20, 20)
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

    def recursive_create_widgets(self, level):
        if self.overlapped: return
        self.create_widget()
        self.finish_widget_creation(level)
        self.parent.child_widget_created(self, level)

    def is_visible(self):
        return False

    def on_enter(self, event):
        # hack. definitely. but...
        misc.currently_under_mouse = self.widget
        # set cursor
        if common.adding_widget:
            if self.check_drop_compatibility()[0]:
                self.widget.SetCursor(wx.CROSS_CURSOR)
            else:
                self.widget.SetCursor(wx.StockCursor(wx.CURSOR_NO_ENTRY))
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
                pos = self.index
            else:
                pos = sum( self.sizer._get_row_col(self.index) )
            hatch = compat.BRUSHSTYLE_FDIAGONAL_HATCH  if pos%2 else  compat.BRUSHSTYLE_BDIAGONAL_HATCH
        brush = wx.Brush(color, hatch)
        # draw hatched lines in foreground
        dc.SetBrush(brush)
        size = self.widget.GetClientSize()
        dc.DrawRectangle(0, 0, size.width, size.height)

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
            # calculate row and col of our slot
            row,col = self.parent._get_row_col(self.index)
            menu = wx.Menu(_("Slot %d/%d"%(row+1,col+1)))
        elif self.parent.CHILDREN not in (-1,1):
            menu = wx.Menu(_("Slot %d"%self.index))
        else:
            menu = wx.Menu(_("Slot"))

        # edit: paste
        i = misc.append_menu_item(menu, -1, _('Paste\tCtrl+V'), wx.ART_PASTE)
        misc.bind_menu_item_after(widget, i, clipboard.paste, self)
        if not clipboard.check("widget","sizer"): i.Enable(False)
        menu.AppendSeparator()

        # slot actions
        if self.parent.IS_SIZER or self.parent.CHILDREN==-1:
            if not "cols" in self.parent.properties:
                i = misc.append_menu_item(menu, -1, _('Remove Slot\tDel'), wx.ART_DELETE)
                misc.bind_menu_item_after(widget, i, self.remove)
                if self.parent.IS_SIZER and len(self.parent.children)<=1: i.Enable(False)
            else:
                # if inside a grid sizer: allow removal of empty rows/cols
                # check whether all slots in same row/col are empty
                row_is_empty = col_is_empty = True
                for index,child in enumerate(self.parent.children):
                    child_row, child_col = self.parent._get_row_col(index)
                    if child_row==row and not child.IS_SLOT:
                        row_is_empty = False
                    if child_col==col and not child.IS_SLOT:
                        col_is_empty = False

                # allow removal of empty row
                i = misc.append_menu_item(menu, -1, _('Remove Row %d'%(row+1)) )
                misc.bind_menu_item_after(widget, i, self.parent.remove_row, row)
                if not row_is_empty or rows<=1: i.Enable(False)

                # allow removal of empty col
                i = misc.append_menu_item(menu, -1, _('Remove Column %d'%(col+1)) )
                misc.bind_menu_item_after(widget, i, self.parent.remove_col, col)
                if not col_is_empty or cols<=1: i.Enable(False)
                menu.AppendSeparator()

        if hasattr(self.parent, "_add_parent_popup_menu_items"):
            self.parent._add_parent_popup_menu_items(menu, self, widget)

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
    def remove(self, user=True):
        # entry point from GUI
        i = self.index
        EditBase.remove(self, focus=False, user=user)
        if i >= len(self.parent.children): i = len(self.parent.children)-1
        # set focused widget
        if i>=0:
            misc.set_focused_widget( self.parent.children[i] )
        else:
            misc.set_focused_widget( self.parent )

    def on_drop_widget(self, event, reset=None):
        """replaces self with a widget. This method is called to add every non-toplevel
        widget or sizer, and in turn calls the appropriate builder function
        (found in the 'common.widgets' dict)."""
        if not common.adding_widget:  # widget focused/selected
            misc.set_focused_widget(self)
            if self.widget:
                self.widget.Refresh()
                self.widget.SetFocus()
            return
        if not self.check_drop_compatibility()[0]:
            return
        if self.widget:
            self.widget.SetCursor(wx.NullCursor)
        common.adding_window = event and event.GetEventObject().GetTopLevelParent() or None
        common.history.widget_adding(self)
        # call the appropriate builder
        new_widget = common.widgets[common.widget_to_add](self.parent, self.index)
        if new_widget is None: return
        misc.rebuild_tree(new_widget)
        if reset is False: return
        if event is None or not misc.event_modifier_copy(event):
            common.adding_widget = common.adding_sizer = False
            common.widget_to_add = None
        if event is not None and new_widget.widget:
            # set focus; required mainly on macOS to receive keys
            widget = None
            if new_widget.IS_WINDOW:
                widget = new_widget.widget
            elif new_widget.IS_SIZER:
                widget = new_widget.toplevel_parent_window.widget
            if hasattr(widget, "SetFocus"):
                widget.SetFocus()
        common.history.widget_added(new_widget)

    def check_drop_compatibility(self):
        if common.adding_sizer and self.parent.IS_CONTAINER:
            return (False, "No sizer can be added here")
        if self.parent.WX_CLASS in ("wxPanel", "wxScrolledWindow", ):
            # no containers like splitter or notebook in a panel
            if common.widget_to_add in ("EditSplitterWindow", "EditNotebook"):
                return (False, "No container can be added here")
        return (True,None)

    # clipboard handling ###############################################################################################
    def check_compatibility(self, widget, typename=None):
        "check whether widget can be pasted here"
        if self.parent.CHILDREN == -1:
            # single or no child: no sizer but a panel or frame
            return self.parent.check_compatibility(widget, typename)
        if typename is not None:
            if typename=="sizer" and self.parent.CHILDREN != 1:
                return (False, "No sizer can be pasted here")
            if typename=="window":
                return (False, "No toplevel object can be pasted here.")
            return (True,None)

        if widget.IS_TOPLEVEL:
            return (False, "No toplevel object can be pasted here.")
        if self.parent.IS_CONTAINER and widget.IS_SIZER:
            # e.g. a sizer dropped on a splitter window slot; instead, a panel would be required
            return (False, "No sizer can be pasted here")
        return (True,None)

    def clipboard_paste(self, clipboard_data):
        "Insert a widget from the clipboard to the current destination"
        if self.parent.CHILDREN==-1:
            # e.g. Panel has special treatment
            return self.parent.clipboard_paste(clipboard_data)
        return clipboard._paste(self.parent, self.index, clipboard_data)

    def on_select_and_paste(self, *args):
        "Middle-click event handler: selects the slot and, if the clipboard is not empty, pastes its content here"
        misc.focused_widget = self
        self.widget.SetFocus()
        clipboard.paste(self)
    ####################################################################################################################

    def destroy_widget(self, level):
        if self.widget is None: return
        if misc.currently_under_mouse is self.widget:
            misc.currently_under_mouse = None

        self.widget.Hide()

        if wx.VERSION_STRING!="2.8.12.0":
            # unbind events to prevent new created (and queued) events
            self.widget.Bind(wx.EVT_PAINT, None)
            self.widget.Bind(wx.EVT_RIGHT_DOWN, None)
            self.widget.Bind(wx.EVT_LEFT_DOWN, None)
            self.widget.Bind(wx.EVT_MIDDLE_DOWN, None)
            self.widget.Bind(wx.EVT_ENTER_WINDOW, None)
            self.widget.Bind(wx.EVT_LEAVE_WINDOW, None)
            self.widget.Bind(wx.EVT_KEY_DOWN, None)
        compat.DestroyLater(self.widget)
        self.widget = None

        if misc.focused_widget is self:
            misc.set_focused_widget(None)

    def write(self, output, tabs):
        if self.parent.CHILDREN==-1:
            output.extend( common.format_xml_tag( u'object', '', tabs, **{'class': 'slot'}) )

    # for tree and help display ########################################################################################
    def _get_tree_label(self):
        if self.label: return str(self.label)
        if self.parent.CHILDREN in (1,-1): return "SLOT"
        index = self.index
        if hasattr(self.parent, "_get_slot_label"):
            return self.parent._get_slot_label(index)
        if self.parent.IS_SIZER and "cols" in self.parent.properties:
            # grid sizer: display row/col
            rows, cols = self.parent._get_actual_rows_cols()
            row = index // cols + 1  # 1 based at the moment
            col = index %  cols + 1
            return "SLOT  %d/%d"%(row, col)
        return "SLOT %d"%(index)

    def _get_tree_image(self):
        "Get an image name for tree display"
        if self.parent.WX_CLASS=="wxSplitterWindow":
            return 'EditSplitterSlot-%s'%self.parent._get_label(self.index)  # 'Left', 'Right', 'Top', 'Bottom'

        if not self.parent.IS_SIZER: return "EditSlot"
        name = "EditSizerSlot"
        if "rows" in self.parent.properties:
            rows, cols = self.parent._get_actual_rows_cols()
            row, col = self.parent._get_row_col(self.index, cols)
            posh = posv = 1  # default
            if row==0:        posv = 0
            elif row+1==rows: posv = 2
            if col==0:        posh = 0
            elif col+1==cols: posh = 2
            name = "EditGridSizerSlot-%s%s"%(posh, posv)
            if self.overlapped: name += "-Disabled"
        elif "orient" in self.parent.properties:
            sizer_orient = self.parent.orient
            if sizer_orient is not None:
                if sizer_orient==wx.VERTICAL:
                    name = "EditVerticalSizerSlot"
                elif sizer_orient==wx.HORIZONTAL:
                    name = "EditHorizontalSizerSlot"
        return name

    def _get_tooltip(self):
        if self.parent.WX_CLASS in ("wxPanel", "wxScrolledWindow"):
            return "Add a sizer here."
        if self.parent.WX_CLASS in ("wxFrame",):
            return "Add a control or container or sizer here, e.g. a panel, a panel plus sizer, a notebook or a sizer."
        if self.parent.WX_CLASS in ("wxDialog",):
            return "Add a sizer or a control here."
        return "Add a control or container here, e.g. a panel, a panel plus sizer or a notebook."
