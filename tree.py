# tree.py: classes to handle and display the structure of a wxGlade app
# $Id: tree.py,v 1.32 2003/10/29 22:29:39 lawson89 Exp $
# 
# Copyright (c) 2002-2003 Alberto Griggio <albgrig@tiscalinet.it>
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

from wxPython.wx import *
from xml.sax.saxutils import quoteattr
import misc, common, os.path

class Tree:
    """\
    A class to represent a hierarchy of widgets.
    """
    class Node:
        __empty_win = None
        def __init__(self, widget=None, children=None):
            self.widget = widget
            self.children = children
            self.parent = None

        def remove(self):
            def remove_rec(node):
                if node.children is not None:
                    map(remove_rec, node.children)
                node.children = None
                if node.widget is not None:
                    # replace the just destroyed notebook with an empty window
                    pw = node.widget.property_window
                    pw.SetTitle('Properties - <>')
                    if Tree.Node.__empty_win is None:
                        Tree.Node.__empty_win = wxWindow(pw, -1)
                    pw.GetSizer().GetChildren()[0].SetWindow(
                        Tree.Node.__empty_win)
                    #wxNotebook(node.widget.property_window, -1))
                    # call the widget's ``destructor''
                    node.widget.delete()
                node.widget = None
            remove_rec(self)
            try: self.parent.children.remove(self)
            except: pass
                    
        def __repr__(self):
            try: return self.widget.name
            except AttributeError: return repr(self.widget)

        def write(self, outfile, tabs):
            """\
            Writes the xml code for the widget to the given output file
            """
            import edit_sizers
            fwrite = outfile.write
            assert self.widget is not None
            w = self.widget
            classname = getattr(w, '_classname', w.__class__.__name__)
            fwrite('    ' * tabs + '<object class=%s name=%s base=%s>\n'
                   % (quoteattr(w.klass), quoteattr(w.name),
                      quoteattr(classname)))
            for p in w.properties:
                w.properties[p].write(outfile, tabs+1)

            if isinstance(w, edit_sizers.SizerBase):
                if self.children is not None:
                    for c in self.children:
                        fwrite('    ' * (tabs+1) +
                               '<object class="sizeritem">\n')
                        sp = c.widget.sizer_properties
                        for p in sp: sp[p].write(outfile, tabs+2)
                        c.write(outfile, tabs+2)
                        fwrite('    ' * (tabs+1) + '</object>\n')
            elif self.children is not None:
                for c in self.children: c.write(outfile, tabs+1)
            fwrite('    ' * tabs + '</object>\n')

    # end of class Node

    def __init__(self, root=None, app=None):
        self.root = root
        if self.root is None: self.root = Tree.Node()
        self.current = self.root
        self.app = app # reference to the app properties
        self.names = {} # dictionary of names of the widgets 

    def has_name(self, name):
        return self.names.has_key(name)

    def add(self, child, parent=None):
        if parent is None: parent = self.root 
        if parent.children is None: parent.children = []
        parent.children.append(child)
        child.parent = parent
        self.current = child
        self.names[str(child.widget.name)] = 1
        if parent is self.root and \
               getattr(child.widget.__class__, '_is_toplevel', False):
            self.app.add_top_window(child.widget.name)

    def insert(self, child, parent, index):
        if parent is None: parent = self.root
        if parent.children is None:
            parent.children = []
        parent.children.insert(index, child)
        child.parent = parent
        self.current = child
        self.names[str(child.widget.name)] = 1
        if parent is self.root:
            self.app.add_top_window(child.widget.name)

    def remove(self, node=None):
        if node is not None:
            try: del self.names[str(node.widget.name)]
            except (KeyError, AttributeError): pass
            if node.parent is self.root:
                self.app.remove_top_window(node.widget.name)
            node.remove()
        elif self.root.children:
            [n.remove() for n in self.root.children]
            self.root.children = None
            self.names = {}

    def write(self, outfile=None, tabs=0):
        """\
        Writes the xml equivalent of this tree to the given output file
        """
        if outfile is None:
            import sys
            outfile = sys.stdout
        from time import asctime
        import common
        outfile.write('<?xml version="1.0"?>\n<!-- generated by wxGlade %s ' \
                      'on %s -->\n\n' % (common.version, asctime()))
        outpath = self.app.output_path.strip()
        name = self.app.get_name()
        klass = self.app.get_class()
        option = str(self.app.codegen_opt)
        top_window = self.app.get_top_window()
        language = self.app.get_language()
        encoding = self.app.get_encoding()
        use_gettext = str(int(self.app.use_gettext))
        overwrite = str(int(self.app.overwrite))
        outfile.write('<application path=%s name=%s class=%s option=%s ' \
                      'language=%s top_window=%s encoding=%s ' \
                      'use_gettext=%s overwrite=%s>\n' \
                      % tuple(map(quoteattr,
                                  [outpath, name, klass, option, language,
                                   top_window, encoding, use_gettext,
                                   overwrite]))
                      )
        if self.root.children is not None:
            for c in self.root.children:
                c.write(outfile, tabs+1)
        outfile.write('</application>\n')

    def change_node(self, node, widget):
        """\
        Changes the node 'node' so that it refers to 'widget'
        """
        try: del self.names[node.widget.name]
        except KeyError: pass
        node.widget = widget
        self.names[widget.name] = 1

    def change_node_pos(self, node, new_pos, index=None):
        if index is None: index = node.parent.children.index(node)
        if index > new_pos:
            node.parent.children.insert(new_pos, node)
            del node.parent.children[index+1]
        else:
            del node.parent.children[index]
            node.parent.children.insert(new_pos+1, node)

# end of class Tree


class WidgetTree(wxTreeCtrl, Tree):
    """\
    Tree with the ability to display the hierarchy of widgets
    """
    images = {} # dictionary of icons of the widgets displayed
    def __init__(self, parent, application):
        id = wxNewId()
        wxTreeCtrl.__init__(self, parent, id)
        root_node = Tree.Node(application)
        self.cur_widget = None # reference to the selected widget
        Tree.__init__(self, root_node, application)
        image_list = wxImageList(21, 21)
        image_list.Add(wxBitmap(os.path.join(common.wxglade_path,
                                             'icons/application.xpm'),
                                wxBITMAP_TYPE_XPM))
        for w in WidgetTree.images:
            WidgetTree.images[w] = image_list.Add(wxBitmap(
                WidgetTree.images[w], wxBITMAP_TYPE_XPM))
        self.AssignImageList(image_list)
        root_node.item = self.AddRoot('Application', 0)
        self.SetPyData(root_node.item, root_node)
        self.skip_select = 0 # necessary to avoid an infinite loop on win32, as
                             # SelectItem fires an EVT_TREE_SEL_CHANGED event
        self.title = ' '
        self.set_title(self.title)
        
        self.auto_expand = True # this control the automatic expansion of
                                # nodes: it is set to False during xml loading
        self._show_menu = misc.wxGladePopupMenu('widget') # popup menu to
                                                          # show toplevel
                                                          # widgets
        SHOW_ID = wxNewId()
        self._show_menu.Append(SHOW_ID, 'Show')
        EVT_TREE_SEL_CHANGED(self, id, self.on_change_selection)
        EVT_RIGHT_DOWN(self, self.popup_menu)
        EVT_LEFT_DCLICK(self, self.show_toplevel)
        EVT_MENU(self, SHOW_ID, self.show_toplevel)
        def on_key_down(event):
            evt_flags = 0
            if event.ControlDown(): evt_flags = wxACCEL_CTRL
            evt_key = event.GetKeyCode()
            for flags, key, function in misc.accel_table:
                if evt_flags == flags and evt_key == key:
                    wxCallAfter(function)
                    break
            event.Skip()
        EVT_KEY_DOWN(self, on_key_down)
        
    def add(self, child, parent=None, image=None): # is image still used?
        """\
        appends child to the list of parent's children
        """
        Tree.add(self, child, parent)
        import common
        name = child.widget.__class__.__name__
        index = WidgetTree.images.get(name, -1)
        if parent is None: parent = parent.item = self.GetRootItem()
        child.item = self.AppendItem(parent.item, child.widget.name, index)
        self.SetPyData(child.item, child)
        if self.auto_expand:
            self.Expand(parent.item)
            self.select_item(child)
        child.widget.show_properties()

    def insert(self, child, parent, pos, image=None):
        """\
        inserts child to the list of parent's children, before index
        """
        if parent.children is None:
            self.add(child, parent, image)
            return
        import common
        name = child.widget.__class__.__name__
        image_index = WidgetTree.images.get(name, -1)
        if parent is None: parent = parent.item = self.GetRootItem()

        index = 0
        item, cookie = self.GetFirstChild(parent.item, 1)
        while item.IsOk():
            item_pos = self.GetPyData(item).widget.pos
            if pos < item_pos: break
            index += 1            
            item, cookie = self.GetNextChild(parent.item, cookie)

        Tree.insert(self, child, parent, index)
        child.item = self.InsertItemBefore(parent.item, index,
                                           child.widget.name, image_index)
        self.SetPyData(child.item, child)
        if self.auto_expand:
            self.Expand(parent.item)
            self.select_item(child)
        child.widget.show_properties()

    def remove(self, node=None):
        self.app.saved = False # update the status of the app
        Tree.remove(self, node)
        if node is not None:
            try:
                self.cur_widget = None
                self.SelectItem(node.parent.item)
            except: self.SelectItem(self.GetRootItem())
            self.Delete(node.item)
        else:
            wxTreeCtrl.Destroy(self)

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

    def set_name(self, node, name):
        try: del self.names[self.GetItemText(node.item)]
        except KeyError: pass
        self.names[name] = 1
        self.SetItemText(node.item, name)

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
                if self.cur_widget: self.cur_widget.update_view(False)
                self.cur_widget = self.GetPyData(item).widget
                misc.focused_widget = self.cur_widget
                self.cur_widget.show_properties(None)
                self.cur_widget.update_view(True)
            except AttributeError:
                pass
            except Exception:
                import traceback
                traceback.print_exc()

    def popup_menu(self, event):
        node = self._find_item_by_pos(*event.GetPosition())
        if not node: return
        else:
            self.select_item(node)
            item = node.widget
        if not item.widget or not item.is_visible():
            import edit_windows
            #if isinstance(item, edit_windows.TopLevelBase):
            if node.parent is self.root:
                self._show_menu.SetTitle(item.name)
                self.PopupMenu(self._show_menu, event.GetPosition())
            return
        try:
            x, y = self.ClientToScreen(event.GetPosition())
            x, y = item.widget.ScreenToClient((x, y))
            event.m_x, event.m_y = x, y
            item.popup_menu(event)
        except AttributeError:
            import traceback; traceback.print_exc()

    def expand(self, node=None, yes=True):
        """\
        expands or collapses the given node
        """
        if node is None: node = self.root
        if yes: self.Expand(node.item)
        else: self.Collapse(node.item)

    def set_title(self, value):
        self.title = value
        try: self.GetParent().SetTitle('wxGlade: Tree %s' % value)
        except: pass

    def get_title(self):
        if not self.title: self.title = ' '
        return self.title

    def show_widget(self, node, toplevel=False):
        """\
        Shows the widget of the given node and all its children
        """
        if toplevel:
            if not node.widget.widget:
                node.widget.create_widget()
                node.widget.finish_widget_creation()
            if node.children:
                for c in node.children: self.show_widget(c)
##             # set the best size for the widget (if no one is given) before
##             # showing it
##             if not node.widget.properties['size'].is_active() and \
##                    node.widget.sizer:
##                 node.widget.sizer.fit_parent() #node.widget)
            node.widget.post_load()
            node.widget.show_widget(True)
            node.widget.show_properties()
            node.widget.widget.Raise()


        else:
            import edit_sizers
            def show_rec(node):
                node.widget.show_widget(True)
                self.expand(node, True)
                if node.children:
                    for c in node.children: show_rec(c)
                node.widget.post_load()
##                 w = node.widget
##                 if isinstance(w, edit_sizers.SizerBase): return
##                 elif not w.properties['size'].is_active() and \
##                          w.sizer and w.sizer.toplevel:
##                     w.sizer.fit_parent()
                    
            show_rec(node)

    def show_toplevel(self, event):
        """\
        Event handler for left double-clicks: if the click is above a toplevel
        widget and this is hidden, shows it
        """
        try: x, y = event.GetPosition()
        except AttributeError:
            # if we are here, event is a CommandEvent and not a MouseEvent
            node = self.GetPyData(self.GetSelection())
            self.expand(node) # if we are here, the widget must be shown
        else:
            node = self._find_item_by_pos(x, y, True)
        if node is not None:
            if not node.widget.is_visible():
                # added by rlawson to expand node on showing top level widget
                self.expand(node)
                self.show_widget(node, True)
            else:
                node.widget.show_widget(False)
                #self.select_item(self.root)
                # added by rlawson to collapse only the toplevel node, not collapse back to root node
                self.select_item(node)
                self.app.show_properties()
                event.Skip()
        #event.Skip()

    def _find_item_by_pos(self, x, y, toplevels_only=False):
        """\
        Finds the node which is displayed at the given coordinates.
        Returns None if there's no match.
        If toplevels_only is True, scans only root's children
        """
        item, flags = self.HitTest((x, y))
        if item and flags & (wxTREE_HITTEST_ONITEMLABEL |
                             wxTREE_HITTEST_ONITEMICON):
            node = self.GetPyData(item)
            if not toplevels_only or node.parent is self.root:
                return node
        return None

    def change_node(self, node, widget):
        Tree.change_node(self, node, widget)
        self.SetItemImage(node.item, self.images.get(
            widget.__class__.__name__, -1))
        self.SetItemText(node.item, widget.name)

    def change_node_pos(self, node, new_pos):
        if new_pos >= self.GetChildrenCount(node.parent.item, False):
            return
        index = node.parent.children.index(node)
        Tree.change_node_pos(self, node, new_pos, index)
        old_item = node.item
        image = self.GetItemImage(node.item)
        self.Freeze()
        if index > new_pos:
            node.item = self.InsertItemBefore(node.parent.item, new_pos,
                                              node.widget.name, image)
        else:
            node.item = self.InsertItemBefore(node.parent.item, new_pos+1,
                                              node.widget.name, image)
        self.SetPyData(node.item, node)
        def append(parent, node):
            idx = WidgetTree.images.get(node.widget.__class__.__name__, -1)
            node.item = self.AppendItem(parent.item, node.widget.name, idx)
            self.SetPyData(node.item, node)
            if node.children:
                for c in node.children:
                    append(node, c)
            self.Expand(node.item)
        if node.children:
            for c in node.children:
                append(node, c)
        self.Expand(node.item)
        self.Delete(old_item)
        self.Thaw()

# end of class WidgetTree

