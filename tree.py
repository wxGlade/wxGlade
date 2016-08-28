"""\
Classes to handle and display the structure of a wxGlade app

@copyright: 2002-2007 Alberto Griggio
@copyright_ 2014-2016 Carsten Grohmann
@copyright_ 2016      Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import logging, os.path, sys, time
import wx
import misc, common, compat, config
import edit_sizers, application


class Node(object):
    __empty_win = None

    def __init__(self, widget=None, children=None):
        self.widget = widget      # e.g. EditPanel or EditBoxSizer
        self.children = children  # list of Node or SlotNode instances
        self.parent = None        # parent node; will be set in Tree.add/insert

    @classmethod
    def remove_rec(cls, node):
        "recursively remove node and it's children"
        for child_node in (node.children or []):
            cls.remove_rec(child_node)
        node.children = None
        if node.widget is not None:
            if node.widget.property_window is not None:  # replace the just destroyed notebook with an empty window
                pw = node.widget.property_window
                pw.SetTitle(_('Properties - <%s>') % '')
                if Node.__empty_win is None:
                    Node.__empty_win = wx.Window(pw, -1)
                compat.SizerItem_SetWindow( pw.GetSizer().GetChildren()[0], Node.__empty_win )
            # call the widget's ``destructor''
            node.widget.delete()
        node.widget = None

    def remove(self):
        self.remove_rec(self)
        try:
            self.parent.children.remove(self)
        except:
            pass

    def has_ancestor(self, node):
        # returns True if node is parent or parents parent ...
        parent = self.parent
        while True:
            if node is parent: return True
            if parent.parent is None: return False
            parent = parent.parent

    def __repr__(self):
        try: return self.widget.name
        except AttributeError: return repr(self.widget)

    def write(self, outfile, tabs, class_names=None):
        "Writes the xml code for the widget to the given output file"
        # XXX move this to the widget
        import edit_sizers
        fwrite = outfile.write
        assert self.widget is not None
        classname = getattr(self.widget, '_classname', self.widget.__class__.__name__)
        # to disable custom class code generation (for panels...)
        if getattr(self.widget, 'no_custom_class', False):
            no_custom = u' no_custom_class="1"'
        else:
            no_custom = ""
        outer_tabs = u'    ' * tabs
        fwrite(u'%s<object %s %s %s%s>\n' % ( outer_tabs,
                                              common.format_xml_attrs(**{'class': self.widget.klass}),
                                              common.format_xml_attrs(name=self.widget.name),
                                              common.format_xml_attrs(base=classname),
                                              no_custom) )

        for prop in self.widget.properties:
            if not getattr(self.widget.properties[prop], '_omitter', None):
                self.write_by_omitter(self.widget, prop, outfile, tabs+1)

        if class_names is not None and self.widget.__class__.__name__ != 'CustomWidget':
            class_names.add(self.widget.klass)

        if isinstance(self.widget, edit_sizers.SizerBase):
            for child in self.children or []:
                if not isinstance(child, SlotNode):# hasattr(child, 'widget'):
                    inner_xml = compat.StringIO()
                    for prop in child.widget.sizer_properties.values():
                        prop.write(inner_xml, tabs+2)
                    child.write(inner_xml, tabs+2, class_names)
                    stmt = common.format_xml_tag( u'object', inner_xml.getvalue(), tabs+1,
                                                  is_xml=True, **{'class': 'sizeritem'} )
                    fwrite(stmt)
                else:
                    child.write(outfile, tabs + 1)
        elif self.children is not None:
            for child in self.children:
                child.write(outfile, tabs + 1, class_names)
        fwrite(u'%s</object>\n' % outer_tabs)

    def write_by_omitter(self, widget, name, file, tabs):
        """Write to given output file:
        - value of property
        - values of properties that previous can block
        - any omitter can be blocked as well"""
        widget.properties[name].write(file, tabs)
        if getattr(widget, 'get_property_blocking', None):
            items = widget.get_property_blocking(name)
            if items:
                for name in items:
                    self.write_by_omitter(widget, name, file, tabs)



class SlotNode(Node):
    "Placeholder for an empty sizer slot"
    def write(self, outfile, tabs):
        stmt = common.format_xml_tag( u'object', '', tabs, **{'class': 'sizerslot'})
        outfile.write(stmt)


class Tree(object):
    "A class to represent a hierarchy of widgets"

    def __init__(self, root=None, app=None):
        # initialise instance logger
        self._logger = logging.getLogger(self.__class__.__name__)

        # initialise instance
        self.root = root
        if self.root is None: self.root = Node()
        self.current = self.root
        self.app = app   # reference to the app properties
        self.names = {}  # dictionary of names of the widgets: each entry is a dictionary, one for each toplevel widget

    def _find_toplevel(self, node):
        assert node is not None, _("None node in _find_toplevel")
        if node.parent is self.root:
            return node
        while node.parent is not self.root:
            node = node.parent
        return node

    def has_name(self, name, node=None):
        if node is None:
            for n in self.names:
                if name in self.names[n]:
                    return True
            return False
        else:
            node = self._find_toplevel(node) # node is a name
            return name in self.names[node]

    def add(self, child, parent=None):
        if parent is None: parent = self.root
        if parent.children is None: parent.children = []
        parent.children.append(child)
        child.parent = parent
        self.current = child
        self.names.setdefault(self._find_toplevel(child), {})[str(child.widget.name)] = 1
        if parent is self.root and getattr(child.widget.__class__, '_is_toplevel', False):
            self.app.add_top_window(child.widget.name)

    def insert(self, child, parent, index):
        # if at index there is a SlotNode, replace this, otherwise insert
        if parent is None: parent = self.root
        if parent.children is None:
            parent.children = []
        parent.children.insert(index, child)
        child.parent = parent
        self.current = child
        self.names.setdefault(self._find_toplevel(child), {})[str(child.widget.name)] = 1
        if parent is self.root:
            self.app.add_top_window(child.widget.name)

    def clear_name_rec(self, n):
        try:
            del self.names[self._find_toplevel(n)][str(n.widget.name)]
        except (KeyError, AttributeError):
            pass

        for c in (n.children or []):
            self.clear_name_rec(c)

    def remove(self, node=None):
        if node is not None:
            self.clear_name_rec(node)
            if node.parent is self.root:
                self.app.remove_top_window(node.widget.name)
            node.remove()
        elif self.root.children:
            for n in self.root.children:
                n.remove()
            self.root.children = None
            self.names = {}

    def write(self, outfile=None, tabs=0):
        """Writes the xml equivalent of this tree to the given output file.
        This function writes unicode to the outfile."""
        if outfile is None:
            outfile = sys.stdout
        outfile.write( u'<?xml version="1.0"?>\n'
                       u'<!-- generated by wxGlade %s on %s -->\n\n' % (config.version, time.asctime()) )
        outpath = os.path.normpath( os.path.expanduser(self.app.output_path.strip()) )
        name = self.app.get_name()
        klass = self.app.get_class()
        option = self._to_digit_string(self.app.multiple_files)
        top_window = self.app.get_top_window()
        language = self.app.get_language()
        encoding = self.app.get_encoding()
        use_gettext = self._to_digit_string(self.app.use_gettext)
        is_template = self._to_digit_string(self.app.is_template)
        overwrite = self._to_digit_string(self.app.overwrite)
        indent_amount = self._to_digit_string(self.app.indent_amount)
        indent_symbol = self._to_digit_string(self.app.indent_mode)
        if indent_symbol == '0':
            indent_symbol = u"tab"
        else:
            indent_symbol = u"space"
        source_ext = '.' + self.app.source_ext
        header_ext = '.' + self.app.header_ext
        for_version = str(self.app.for_version)

        attrs = {
            'path': outpath,
            'name': name,
            'class': klass,
            'option': option,
            'language': language,
            'top_window': top_window,
            'encoding': encoding,
            'use_gettext': use_gettext,
            'overwrite': overwrite,
            'use_new_namespace': 1,
            'for_version': for_version,
            'is_template': is_template,
            'indent_amount': indent_amount,
            'indent_symbol': indent_symbol,
            'source_extension': source_ext,
            'header_extension': header_ext,
        }

        inner_xml = compat.StringIO()

        if self.app.is_template and getattr(self.app, 'template_data', None):
            self.app.template_data.write(inner_xml, tabs+1)

        class_names = set()
        if self.root.children is not None:
            for c in self.root.children:
                c.write(inner_xml, tabs+1, class_names)

        stmt = common.format_xml_tag( u'application', inner_xml.getvalue(), is_xml=True, **attrs )
        outfile.write(stmt)

        return class_names

    def change_node(self, node, widget):
        "Changes the node 'node' so that it refers to 'widget'"
        try:
            del self.names[self._find_toplevel(node)][node.widget.name]
        except KeyError:
            pass
        node.widget = widget
        self.names.setdefault(self._find_toplevel(node), {})[widget.name] = 1

    def change_node_pos(self, node, new_pos, index=None):
        if index is None: index = node.parent.children.index(node)
        if index >= new_pos:
            node.parent.children.insert(new_pos, node)
            del node.parent.children[index + 1]
        else:
            del node.parent.children[index]
            node.parent.children.insert(new_pos+1, node)

    def _to_digit_string(self, value):
        "Convert the given str, int or bool value to a digit string"
        if isinstance(value, bool):
            return '1' if value else '0'
        elif isinstance(value, int):
            return '%s'%value
        elif isinstance(value, basestring):
            if value.isdigit():
                return value
        # log failures
        logging.warning( _('Failed to convert "%s" (type: %s) to a digit string'), value, type(value) )
        return '%s' % value



class WidgetTree(wx.TreeCtrl, Tree):
    "Tree with the ability to display the hierarchy of widgets"
    images = {} # Dictionary of icons of the widgets displayed
    _logger = None # Class specific logging instance
    def __init__(self, parent, application):
        self._logger = logging.getLogger(self.__class__.__name__)
        id = wx.NewId()
        style = wx.TR_DEFAULT_STYLE|wx.TR_HAS_VARIABLE_ROW_HEIGHT
        style |= wx.TR_EDIT_LABELS
        if wx.Platform == '__WXGTK__':
            style |= wx.TR_NO_LINES|wx.TR_FULL_ROW_HIGHLIGHT
        elif wx.Platform == '__WXMAC__':
            style &= ~wx.TR_ROW_LINES
        wx.TreeCtrl.__init__(self, parent, id, style=style)
        root_node = Node(application)
        self.cur_widget = None  # reference to the selected widget
        Tree.__init__(self, root_node, application)
        image_list = wx.ImageList(21, 21)
        image_list.Add(wx.Bitmap(os.path.join(config.icons_path, 'application.xpm'), wx.BITMAP_TYPE_XPM))
        for w in WidgetTree.images:
            WidgetTree.images[w] = image_list.Add(misc.get_xpm_bitmap(WidgetTree.images[w]))
        self.AssignImageList(image_list)
        root_node.item = self.AddRoot(_('Application'), 0)
        self.SetPyData(root_node.item, root_node)
        self.skip_select = 0  # necessary to avoid an infinite loop on win32, as SelectItem fires an
                              # EVT_TREE_SEL_CHANGED event
        self.title = ' '
        self.set_title(self.title)

        self.auto_expand = True  # this control the automatic expansion of  nodes: it is set to False during xml loading
        self.Bind(wx.EVT_TREE_SEL_CHANGED, self.on_change_selection)
        self.Bind(wx.EVT_RIGHT_DOWN, self.popup_menu)
        self.Bind(wx.EVT_LEFT_DCLICK, self.show_toplevel)
        self.Bind(wx.EVT_LEFT_DOWN, self.on_left_click) # allow direct placement of widgets
        self.Bind(wx.EVT_MENU, self.show_toplevel)
        self.Bind(wx.EVT_TREE_BEGIN_DRAG, self.begin_drag)
        self.Bind(wx.EVT_TREE_END_DRAG, self.end_drag)
        
        self.Bind(wx.EVT_TREE_BEGIN_LABEL_EDIT, self.begin_edit_label)
        self.Bind(wx.EVT_TREE_END_LABEL_EDIT, self.end_edit_label)
        self.Bind(wx.EVT_KEY_DOWN, misc.on_key_down_event)

    def begin_drag(self, evt):
        # inhibit drag start or remember the dragged item
        item = evt.GetItem()
        if isinstance(self.GetPyData(item), SlotNode):
            return
        evt.Allow()
        self._dragged_item = item
        msg = "Move control/sizer to empty or populated slot to insert, to a sizer to append; hold Ctrl to copy"
        common.palette.user_message( msg )
        return True

    def end_drag(self, evt):
        # move or copy a node including children; re-uses clipboard functionality
        copy = wx.GetKeyState(wx.WXK_CONTROL)  # if Ctrl key is pressed, we will copy

        if self.cur_widget:
            self.cur_widget.update_view(False)

        src = self._dragged_item  # was set in begin_drag
        dst = evt.GetItem()
        self._dragged_item = None
        if src is dst: return
        # from tree items to nodes to widgets
        src_node = self.GetPyData( src )
        dst_node = self.GetPyData( dst )
        if dst_node is None: return
        src_widget = src_node.widget
        dst_widget = dst_node.widget
        if src_widget==dst_widget: return

        # do some checks before cutting ################################################################################
        # avoid dragging of an item on it's child
        if dst_node.has_ancestor(src_node):
            return
        # possible to paste here?
        compatible = dst_widget.check_compatibility(src_widget)
        if not compatible:
            return
        if compatible=="AddSlot":
            # dropped on a sizer -> add slot
            dst_widget.add_slot(force_layout=True)
            dst_widget = dst_widget.children[-1].item # the slot
        elif compatible=="Slot":
            # insert a slot
            pos = dst_widget.pos
            dst_widget.sizer.insert_slot( pos, force_layout=False )
            dst_widget = dst_widget.sizer.children[pos].item # the slot

        if not hasattr(dst_widget, "clipboard_paste"):
            return

        # use cut and paste functionality from clipboard to do the actual work #########################################
        import clipboard

        data = clipboard.get_copy(src_widget)
        if not copy:
            src_widget.remove()

        dst_widget.clipboard_paste(None, data)
        self.expand(dst_node)

    def begin_edit_label(self, evt):
        # Begin editing a label. This can be prevented by calling Veto()
        pass

    def end_edit_label(self, evt):
        # Finish editing a label. This can be prevented by calling Veto()
        if evt.IsEditCancelled(): return
        item = evt.Item
        node = self.GetPyData( evt.Item )
        widget = node.widget
        # XXX split user input into name and label/title (reverse of self._build_label)
        old_label = self.GetItemText(item)
        widget.set_name(evt.Label)
        if self.GetItemText(item) == old_label:
            evt.Veto()

    def _build_label(self, node):
        import edit_sizers
        if isinstance(node.widget, edit_sizers.SizerSlot):
            return "SLOT"
        s = node.widget.name
        if node.widget.klass != node.widget.base and node.widget.klass != 'wxScrolledWindow':
            # special case...
            s += ' (%s)' % node.widget.klass
        if isinstance(node.widget, edit_sizers.edit_sizers.EditStaticBoxSizer):
            # include label of static box sizer
            s += ': "%s"'%node.widget.label
        return s

    def get_image(self, child, image=None):
        if image is not None:
            return image

        #if child.widget:
        if isinstance(child, Node):
            name = child.widget.__class__.__name__
            widget = child.widget
            if name=="SizerSlot":
                sizer_orient = getattr(child.parent.widget, "orient", None)
        else:
            name = child.__class__.__name__
            widget = child
            if name=="SizerSlot":
                sizer_orient = getattr(child.sizer, "orient", None)

        if name=="SizerSlot":
            if sizer_orient==wx.VERTICAL:
                name = "EditVerticalSizerSlot"
            elif sizer_orient==wx.HORIZONTAL:
                name = "EditHorizontalSizerSlot"
            else:
                name = "EditSizerSlot"
        elif name in ("EditStaticBoxSizer", "EditBoxSizer"):
            # with or without label, horizontal/vertical
            if widget.orient & wx.VERTICAL:
                name = "EditVerticalSizer"
            elif widget.orient & wx.HORIZONTAL:
                name = "EditHorizontalSizer"
            else:
                name = "EditSpacer"
        index = WidgetTree.images.get(name, -1)
        return index

    def add(self, child, parent=None, image=None):  # is image still used?
        "appends child to the list of parent's children"
        assert isinstance(child, Node)

        Tree.add(self, child, parent)
        image = self.get_image(child, image)
        if parent is None: parent = parent.item = self.GetRootItem()
        child.item = self.AppendItem(parent.item, self._build_label(child), image)
        self.SetPyData(child.item, child)
        if self.auto_expand:
            self.Expand(parent.item)
            self.select_item(child)

        if not isinstance(child.widget, edit_sizers.SizerSlot):
            self.app.check_codegen(child.widget)

    def insert(self, child, parent, index, image=None):
        "inserts child to the list of parent's children, before pos"
        assert isinstance(child, Node)
        # pos is index
        if parent is None:
            # XXX check whether this is called at all
            parent = self.GetPyData( self.GetRootItem() )
        assert isinstance(parent, Node)

        # parent is a Node, i.e. Dummy or Button are not in parent.children
        if parent.children is None or index>=len(parent.children):
            self.add(child, parent, image)
            return

        if isinstance( parent.children[index], SlotNode ):
            if not isinstance(child, SlotNode):
                self.remove( parent.children[index] )

        Tree.insert(self, child, parent, index)
        image = self.get_image(child, image)
        child.item = compat.wx_Tree_InsertItemBefore(self, parent.item, index, self._build_label(child), image)
        self.SetPyData(child.item, child)
        if self.auto_expand:
            self.Expand(parent.item)
            self.select_item(child)
        if not isinstance(child.widget, edit_sizers.SizerSlot):
            child.widget.show_properties()
            self.app.check_codegen(child.widget)

    def remove(self, node=None, delete=True):
        self.app.saved = False  # update the status of the app
        Tree.remove(self, node)
        if node is not None:
            try:
                self.cur_widget = None
                self.SelectItem(node.parent.item)
            except: self.SelectItem(self.GetRootItem())
            if delete:
                self.Delete(node.item)
        else:
            wx.TreeCtrl.Destroy(self)

    def clear(self):
        self.app.reset()
        self.skip_select = True
        if self.root.children:
            while self.root.children:
                c = self.root.children[-1]
                if c.widget: c.widget.remove()
            self.root.children = None
        self.skip_select = False
        app = self.GetPyData(self.GetRootItem())
        app.widget.show_properties()

    def refresh_name(self, node, oldname=None):  # , name=None):
        if oldname is not None:
            try:
                del self.names[self._find_toplevel(node)][oldname]
            except KeyError:
                pass
        self.names.setdefault(self._find_toplevel(node), {})[node.widget.name] = 1
        self.SetItemText(node.item, self._build_label(node))

    def select_item(self, node):
        self.skip_select = True
        self.SelectItem(node.item)
        self.skip_select = False
        if self.cur_widget: self.cur_widget.update_view(False)
        self.cur_widget = node.widget
        self.cur_widget.update_view(True)
        self.cur_widget.show_properties()
        misc.focused_widget = self.cur_widget

    def on_change_selection(self, event):
        if not self.skip_select:
            item = event.GetItem()
            try:
                if self.cur_widget:
                    self.cur_widget.update_view(selected=False)
                self.cur_widget = self.GetPyData(item).widget
                misc.focused_widget = self.cur_widget
                self.skip_select = True
                self.cur_widget.show_properties()
                self.cur_widget.update_view(True)
            except AttributeError:
                if 'WINGDB_ACTIVE' in os.environ: raise
                pass
            except Exception:
                self._logger.exception(_('Internal Error'))
            self.skip_select = False

    def on_left_click(self, event):
        if not common.adding_widget:
            event.Skip()
            return
        node = self._find_item_by_pos(*event.GetPosition())
        if not node:
            event.Skip()
            return
        item = node.widget
        import edit_sizers
        if not isinstance(item, edit_sizers.SizerSlot):
            event.Skip()
            return
        item.on_drop_widget(None)

    def popup_menu(self, event):
        node = self._find_item_by_pos(*event.GetPosition())
        if not node:
            return
        self.select_item(node)
        item = node.widget
        item.popup_menu(event)

    def expand(self, node=None, yes=True):
        "expands or collapses the given node"
        if node is None: node = self.root
        if yes: self.Expand(node.item)
        else: self.Collapse(node.item)

    def set_title(self, value):
        if value is None: value = ""
        self.title = value
        try:
            self.GetParent().SetTitle(_('wxGlade: Tree %s') % value)
        except:
            pass

    def get_title(self):
        if not self.title: self.title = ' '
        return self.title

    def show_widget(self, node):
        "Shows the widget of the given node and all its children"
        # go up all the tree, if there are notebooks, select the appropriate page
        # XXX this should be centralized
        n = node
        while True:
            if n.parent.widget and hasattr(n.parent.widget, "virtual_sizer") and n.parent.widget.widget:
                # a notebook or splitter window; only these have a virtual_sizer
                if hasattr(n.parent.widget.widget, "GetSelection"):
                    selected_pos = n.parent.widget.widget.GetSelection() + 1
                    if selected_pos!=n.widget.pos:
                        n.parent.widget.widget.SetSelection(n.widget.pos-1)
            n = n.parent
            if n.parent is None: break
        # show the widget
        self._show_widget(node)

    def _show_widget(self, node):
        # creates/shows node's widget, expand node; do the same recursively on its children
        node.widget.show_widget(True)
        self.expand(node, True)
        if node.children:
            for c in node.children:
                self._show_widget(c)
        node.widget.post_load()

    def _show_widget_toplevel(self, node):
        # creates/shows the widget of the given toplevel node and all its children
        if not wx.IsBusy(): wx.BeginBusyCursor()
        if not node.widget.widget:
            node.widget.create_widget()
            node.widget.finish_widget_creation()
        if node.children:
            for c in node.children:
                self._show_widget(c)
        node.widget.post_load()
        node.widget.show_widget(True)
        node.widget.show_properties()
        node.widget.widget.Raise()
        # set the best size for the widget (if no one is given)
        props = node.widget.properties
        if 'size' in props and not props['size'].is_active() and node.widget.sizer:
            node.widget.sizer.fit_parent()
        if wx.IsBusy(): wx.EndBusyCursor()

    def show_toplevel(self, event):
        "Event handler for left double-clicks: if the click is above a toplevel widget and this is hidden, shows it"
        try: x, y = event.GetPosition()
        except AttributeError:
            # if we are here, event is a CommandEvent and not a MouseEvent
            node = self.GetPyData(self.GetSelection())
            self.expand(node)  # if we are here, the widget must be shown
        else:
            node = self._find_item_by_pos(x, y, True)

        if node is not None:
            if not node.widget.is_visible():
                # added by rlawson to expand node on showing top level widget
                self.expand(node)
                self._show_widget_toplevel(node)
            else:
                node.widget.show_widget(False)
                #self.select_item(self.root)
                # added by rlawson to collapse only the toplevel node, not collapse back to root node
                self.select_item(node)
                self.app.show_properties()
                event.Skip()
        #event.Skip()

    def _find_item_by_pos(self, x, y, toplevels_only=False):
        """Finds the node which is displayed at the given coordinates. Returns None if there's no match.
        If toplevels_only is True, scans only root's children"""
        item, flags = self.HitTest((x, y))
        if item and flags & (wx.TREE_HITTEST_ONITEMLABEL | wx.TREE_HITTEST_ONITEMICON):
            node = self.GetPyData(item)
            if not toplevels_only or node.parent is self.root:
                return node
        return None

    def change_node(self, node, widget, new_node=None):
        if new_node is not None:
            parent = new_node.parent = node.parent
            index = parent.children.index(node)
            parent.children[index] = new_node
            new_node.item = node.item
            self.SetPyData(node.item, new_node)
            self.remove(node, delete=False)
            node = new_node
            self.names.setdefault(Tree._find_toplevel(self, node), {})[str(node.widget.name)] = 1
        Tree.change_node(self, node, widget)
        self.SetItemImage(node.item, self.get_image(widget) )
        self.SetItemText(node.item, self._build_label(node))  # widget.name)

    def _append_rec(self, parent, node):
        # helper for the next method
        idx = WidgetTree.images.get(node.widget.__class__.__name__, -1)
        node.item = self.AppendItem( parent.item, self._build_label(node), idx )
        self.SetPyData(node.item, node)
        if node.children:
            for c in node.children:
                self._append_rec(node, c)
        self.Expand(node.item)

    def change_node_pos(self, node, new_pos):
        if new_pos >= self.GetChildrenCount(node.parent.item, False):
            return
        index = node.parent.children.index(node)
        Tree.change_node_pos(self, node, new_pos, index)
        old_item = node.item
        image = self.GetItemImage(node.item)
        self.Freeze()
        if index >= new_pos:
            node.item = compat.wx_Tree_InsertItemBefore( self, node.parent.item, new_pos, self._build_label(node), image )
        else:
            node.item = compat.wx_Tree_InsertItemBefore( self, node.parent.item, new_pos+1, self._build_label(node), image )
        self.SetPyData(node.item, node)

        if node.children:
            for c in node.children:
                self._append_rec(node, c)
        self.Expand(node.item)
        self.Delete(old_item)
        self.Thaw()

    def get_selected_path(self):
        """returns a list of widget names, from the toplevel to the selected one
        Example: ['frame_1', 'sizer_1', 'panel_1', 'sizer_2', 'button_1']
                 if button_1 is the currently selected widget"""
        ret = []
        w = self.cur_widget
        oldw = None
        while w is not None:
            oldw = w
            ret.append(w.name)
            sizer = getattr(w, 'sizer', None)
            if getattr(w, 'parent', "") is None:
                w = w.parent
            elif sizer is not None and not sizer.is_virtual():
                w = sizer
            else:
                if isinstance(w, edit_sizers.SizerBase):
                    w = w.window
                elif isinstance(w, application.Application):
                    w = None
                else:
                    w = w.parent
        ret.reverse()
        # ALB 2007-08-28: remember also the position of the toplevel window in the selected path
        if oldw is not None and oldw.widget is not None:
            assert oldw.widget
            pos = misc.get_toplevel_parent(oldw.widget).GetPosition()
            ret[0] = (ret[0], pos)
        return ret

    def select_path(self, path):
        "sets the selected widget from a path_list, which should be in the form returned by get_selected_path"
        index = 0
        item, cookie = self._get_first_child(self.GetRootItem())
        itemok = None
        parent = None
        pos = None
        while item.Ok() and index < len(path):
            widget = self.GetPyData(item).widget
            name = path[index]
            if index == 0 and isinstance(name, tuple):
                name, pos = name
            if misc.streq(widget.name, name):
                #self.EnsureVisible(item)
                itemok = item
                if parent is None:
                    parent = self.GetPyData(itemok)
                self.cur_widget = widget
                item, cookie = self._get_first_child(item)
                index += 1
            else:
                item = self.GetNextSibling(item)
        if itemok is not None:
            node = self.GetPyData(itemok)
            if parent is not None:
                self._show_widget_toplevel(parent)
                if pos is not None:
                    misc.get_toplevel_parent(parent.widget).SetPosition(pos)
            self.select_item(node)

    def _get_first_child(self, item):
        return self.GetFirstChild(item)
