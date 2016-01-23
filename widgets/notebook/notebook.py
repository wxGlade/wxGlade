"""
wxNotebook objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
import common
import compat
import wcodegen
import misc
from tree import Tree
from widget_properties import *
from edit_windows import ManagedBase, EditStylesMixin
from edit_sizers.edit_sizers import Sizer, SizerSlot
from wcodegen.taghandler import BaseXmlBuilderTagHandler
from xml_parse import XmlParsingError

try:
    from panel import EditPanel
    _has_panel = True
except ImportError:
    _has_panel = False


class NotebookVirtualSizer(Sizer):
    """\
    "Virtual sizer" responsible for the management of the pages of a Notebook.
    """

    def __init__(self, *args, **kwds):
        Sizer.__init__(self, *args, **kwds)
        self._itempos = 0

    def set_item(self, pos, option=None, flag=None, border=None, size=None,
                 force_layout=True):
        """\
        Updates the layout of the item at the given pos.
        """
        if not self.window.widget:
            return
        pos -= 1
        label, item = self.window.tabs[pos]
        if not item or not item.widget:
            return
        if not (pos < self.window.widget.GetPageCount()):
            self.window.widget.AddPage(item.widget, label)
        elif self.window.widget.GetPage(pos) is not item.widget:
            #self.window.widget.RemovePage(pos)
            self.window.widget.DeletePage(pos)
            self.window.widget.InsertPage(pos, item.widget, label)
            self.window.widget.SetSelection(pos)
            try:
                wx.CallAfter(item.sel_marker.update)
            except AttributeError:
                #self._logger.exception(_('Internal Error'))
                pass
        if self.window.sizer is not None:
            self.window.sizer.set_item(
                self.window.pos, size=self.window.widget.GetBestSize())

    def add_item(self, item, pos=None, option=0, flag=0, border=0, size=None,
                 force_layout=True):
        """\
        Adds an item to self.window.
        """
        #self._logger.debug('pos: %s, item.name: %s', pos, item.name)
        try:
            self.window.tabs[pos - 1][1] = item
        except IndexError:
            raise XmlParsingError(
                _('Notebook widget "%s" does not have any tabs!') %
                self.window.name
                )
        item._dont_destroy = True

    def free_slot(self, pos, force_layout=True):
        """\
        Replaces the element at pos with an empty slot
        """
        if self.window._is_removing_pages or not self.window.widget:
            return
        slot = SizerSlot(self.window, self, pos)
        #self._logger.debug('free: %s, %s, %s', slot, slot.pos, pos)
        slot.show_widget(True)
        pos -= 1
        label, item = self.window.tabs[pos]
        self.window.widget.RemovePage(pos)
        self.window.widget.InsertPage(pos, slot.widget, label)
        self.window.widget.SetSelection(pos)

    def get_itempos(self, attrs):
        """\
        Get position of sizer item (used in xml_parse)
        """
        self._itempos += 1
        return self._itempos

    def is_virtual(self):
        return True

# end of class NotebookVirtualSizer


class NotebookPagesProperty(GridProperty):
    def write(self, outfile, tabs):
        inner_xml = u''
        value = self.get_value()
        for i in range(len(value)):
            val = value[i]
            window = None
            try:
                t = self.owner.tabs[i]
                if t[0] == val[0]:
                    window = t[1]
            except:
                pass
            if window:
                inner_xml += common.format_xml_tag(
                    u'tab', val[0], tabs + 1, window=window.name)
        stmt = common.format_xml_tag(
            u'tabs', inner_xml, tabs, is_xml=True)
        outfile.write(stmt)

# end of class NotebookPagesProperty


class TabsHandler(BaseXmlBuilderTagHandler):

    def __init__(self, parent):
        super(TabsHandler, self).__init__()
        self.parent = parent
        self.tab_names = []

    def end_elem(self, name):
        if name == 'tabs':
            self.parent.tabs = [[misc.wxstr(name), None] for name in
                                self.tab_names]
            self.parent.properties['tabs'].set_value([[name] for name in
                                                      self.tab_names])
            return True
        elif name == 'tab':
            char_data = self.get_char_data()
            self.tab_names.append(char_data)
        return False

# end of class TabsHandler


class EditNotebook(ManagedBase, EditStylesMixin):
    """\
    Class to handle wxNotebook objects
    """

    _custom_base_classes = True

    notebook_number = 1
    """\
    @cvar: Next free number for notebook names. The number is continuously.

    Each notebook gets an own number. It's very useful for labeling
    panes. Deleting notebooks won't decrease this number.

    @see: L{self.next_notebook_name()}
    @type: Integer
    @note: Use only C{+=1} for increasing!
    """

    pane_number = 1
    """\
    @ivar: Free number for pane names. This number is is continuously.

    Each pane get an own number. It's very useful for labeling. Deleting
    ones won't decrease this number.

    @see: L{self.next_pane_name()}
    @type: Integer
    """

    update_widget_style = False

    def __init__(self, name, parent, id, style, sizer, pos,
                 property_window, show=True):
        # create new and (still) unused notebook name
        if not name:
            name = self.next_notebook_name()

        # increase number of created notebooks
        EditNotebook.notebook_number += 1

        # initialise parent classes
        ManagedBase.__init__(self, name, 'wxNotebook', parent, id, sizer,
                             pos, property_window, show=show)
        EditStylesMixin.__init__(self)

        # initialise instance variables
        self.set_style(style)
        self.virtual_sizer = NotebookVirtualSizer(self)
        self._is_removing_pages = False

        # initialise properties remaining staff
        self.tabs = [['tab1', None]]  # list of pages of this notebook
                                      # (actually a list of
                                      #  2-list label, window)
        self.access_functions['style'] = (self.get_style, self.set_style)
        self.properties['style'] = CheckListProperty(
            self, 'style', self.widget_writer)
        self.access_functions['tabs'] = (self.get_tabs, self.set_tabs)
        tab_cols = [('Tab label', GridProperty.STRING)]
        self.properties['tabs'] = NotebookPagesProperty(
            self, 'tabs', None, tab_cols, label=_("Tabs"),
            can_remove_last=False)
        del tab_cols
        self.nb_sizer = None
        self._create_slots = False

        self.no_custom_class = False
        self.access_functions['no_custom_class'] = (self.get_no_custom_class,
                                                    self.set_no_custom_class)
        self.properties['no_custom_class'] = CheckBoxProperty(
            self, 'no_custom_class',
            label=_("Don't generate code for this class"))

        # first pane should have number 1
        self.pane_number = 1

    def create_widget(self):
        self.widget = wx.Notebook(
            self.parent.widget, self.id, style=self.get_int_style())

    def show_widget(self, yes):
        ManagedBase.show_widget(self, yes)
        if self._create_slots:
            self._create_slots = False
            for i in range(len(self.tabs)):
                if self.tabs[i][1] is None:
                    self.tabs = self.tabs[:i]
                    self.properties['tabs'].set_value(self.get_tabs())

    def finish_widget_creation(self):
        ManagedBase.finish_widget_creation(self)
        # replace 'self' with 'self.nb_sizer' in 'self.sizer'

    def create_properties(self):
        ManagedBase.create_properties(self)
        panel = wx.ScrolledWindow(
            self.notebook,
            wx.ID_ANY,
            style=wx.TAB_TRAVERSAL,
            )
        self.properties['no_custom_class'].display(panel)
        self.properties['style'].display(panel)
        self.properties['tabs'].display(panel)
        sizer = wx.BoxSizer(wx.VERTICAL)
        sizer.Add(self.properties['no_custom_class'].panel, 0,
                  wx.ALL | wx.EXPAND, 3)
        sizer.Add(self.properties['style'].panel, 0, wx.EXPAND)
        sizer.Add(self.properties['tabs'].panel, 1, wx.ALL | wx.EXPAND, 3)
        panel.SetAutoLayout(True)
        compat.SizerItem_SetSizer(panel, sizer)
        sizer.Fit(panel)
        self.notebook.AddPage(panel, _('Widget'))
        self.properties['tabs'].set_col_sizes([-1])

    def on_set_focus(self, event):
        self.show_properties()
        event.Skip()

    def _add_tab(self, window, pos):
        if window is None:
            window = SizerSlot(self, self.virtual_sizer, pos)
            self.tabs[pos - 1][1] = window
        else:
            window._dont_destroy = True
            node = Tree.Node(window)
            window.node = node
            common.app_tree.add(node, self.node)
        if self.widget:
            window.show_widget(True)
            self.virtual_sizer.set_item(pos)
            try:
                wx.CallAfter(window.sel_marker.update)
            except AttributeError:
                #self._logger.exception(_('Internal Error'))
                pass

    def get_tabs(self):
        return [[n] for n, w in self.tabs]

    def set_tabs(self, tabs):
        delta = len(self.tabs) - len(tabs)
        if delta > 0:
            self._is_removing_pages = True
            # we have to remove some pages
            i = len(tabs)
            if self.widget:
                for n, window in self.tabs[i:]:
                    self.widget.RemovePage(i)
                    window.remove(False)
            del self.tabs[i:]
            if self.widget:
                self.widget.SetSelection(0)
            self._is_removing_pages = False
        elif delta < 0:
            # we have to add some pages
            pos = len(self.tabs)
            for i in range(-delta):
                self.tabs.append(['', None])
                pos += 1
                if _has_panel:
                    window = EditPanel(
                        self.next_pane_name(),
                        self,
                        wx.ID_ANY,
                        self.virtual_sizer,
                        pos,
                        self.property_window,
                        )
                    self._add_tab(window, pos)
                else:
                    self._add_tab(None, pos)
            if self.widget:
                self.widget.SetSelection(self.widget.GetPageCount() - 1)
        # finally, we must update the labels of the tabs
        for i in range(len(tabs)):
            tt = misc.wxstr(tabs[i][0])
            if self.widget:
                self.widget.SetPageText(i, tt)
            self.tabs[i][0] = tt

    def delete(self):
        if self.widget:
            self.widget.DeleteAllPages()
        ManagedBase.delete(self)

    def get_property_handler(self, name):
        if name == 'tabs':
            return TabsHandler(self)
        return ManagedBase.get_property_handler(self, name)

    def find_page(self, page):
        """\
        returns the index of the given page in the notebook, or -1 if the page
        cannot be found
        """
        if not self.widget:
            return -1
        for i in range(len(self.tabs)):
            if self.tabs[i][1] is page:
                if i < self.widget.GetPageCount():
                    return i
                else:
                    return -1
        return -1

    def get_no_custom_class(self):
        return self.no_custom_class

    def set_no_custom_class(self, value):
        self.no_custom_class = bool(int(value))

    def next_notebook_name(self):
        """\
        Return new and (still) unused notebook name

        @see: L{self.notebook_number}
        """
        while True:
            name = 'notebook_%d' % EditNotebook.notebook_number
            if not common.app_tree.has_name(name):
                break
            EditNotebook.notebook_number += 1
        return name

    def next_pane_name(self):
        """\
        Return new and (still) unused pane name

        @see: L{self.pane_number}
        """
        while True:
            pane_name = "%s_pane_%d" % (self.name, self.pane_number)
            self.pane_number += 1
            if not common.app_tree.has_name(pane_name):
                break
        return pane_name

# end of class EditNotebook


editor_class = EditNotebook
editor_icon = 'notebook.xpm'
editor_name = 'EditNotebook'
editor_style = ''

dlg_title = _('wxNotebook')
box_title = _('Orientation')
choices = 'wx.NB_TOP|wx.NB_BOTTOM|wx.NB_LEFT|wx.NB_RIGHT'
tmpl_label = 'notebook'


def builder(parent, sizer, pos, number=[1]):
    """\
    Factory function for editor objects from GUI.
    """
    dialog = wcodegen.WidgetStyleSelectionDialog(
            dlg_title, box_title, choices)
    res = dialog.ShowModal()
    style = dialog.get_selection()
    dialog.Destroy()
    if res != wx.ID_OK:
        return

    widget = editor_class(None, parent, wx.ID_ANY, style, sizer, pos,
                          common.property_panel, show=False)
    if _has_panel:
        pane1 = EditPanel(widget.next_pane_name(), widget, wx.ID_ANY,
                          widget.virtual_sizer, 1, common.property_panel)

    node = Tree.Node(widget)
    widget.node = node
    widget.virtual_sizer.node = node

    widget.set_option(1)
    widget.set_style("wxEXPAND")
    widget.show_widget(True)
    common.app_tree.insert(node, sizer.node, pos - 1)

    if _has_panel:
        widget._add_tab(pane1, 1)

    sizer.set_item(widget.pos, 1, wx.EXPAND)


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    """\
    Factory to build editor objects from a XML file
    """
    try:
        name = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    if sizer is None or sizeritem is None:
        raise XmlParsingError(_("sizer or sizeritem object cannot be None"))
    widget = editor_class(name, parent, wx.ID_ANY, editor_style, sizer,
                          pos, common.property_panel)
    sizer.set_item(widget.pos, option=sizeritem.option,
                   flag=sizeritem.flag, border=sizeritem.border)
    node = Tree.Node(widget)
    widget.node = node
    if pos is None:
        common.app_tree.add(node, sizer.node)
    else:
        common.app_tree.insert(node, sizer.node, pos - 1)
    return widget


def initialize():
    """\
    initialization function for the module: returns a wxBitmapButton to be
    added to the main palette.
    """
    common.widgets[editor_name] = builder
    common.widgets_from_xml[editor_name] = xml_builder
    return common.make_object_button(editor_name, editor_icon)
