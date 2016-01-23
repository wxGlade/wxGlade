"""\
wxFrame and wxStatusBar objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
import common
import compat
import misc
from tree import Tree
from wcodegen.taghandler import BaseXmlBuilderTagHandler
from widget_properties import *
from edit_windows import EditBase, EditStylesMixin


class FieldsHandler(BaseXmlBuilderTagHandler):
    """\
    Custom Property handler for statusbar fields.
    """

    def __init__(self, owner):
        super(FieldsHandler, self).__init__()
        self.owner = owner
        self.width = -1

    def start_elem(self, name, attrs):
        if name == 'fields':
            self.fields = []
        else:  # name == 'field'
            self._content = []
            self.width = attrs.get('width', '-1')

    def end_elem(self, name):
        if name == 'field':
            char_data = self.get_char_data()
            self.fields.append([char_data, self.width])
        else:  # name == 'fields'
            self.owner.fields = self.fields
            self.owner.set_fields(self.owner.fields)
            self.owner.properties['fields'].set_value(self.owner.fields)
            return True


class EditStatusBar(EditBase, EditStylesMixin):

    _hidden_frame = None
    update_widget_style = False

    def __init__(self, name, klass, parent, property_window):
        # Initialise parent classes
        EditBase.__init__(self, name, klass, parent, wx.NewId(),
                          property_window, custom_class=False, show=False)
        EditStylesMixin.__init__(self)

        # initialise instance variables
        self.node = Tree.Node(self)
        self.add = common.app_tree.add(self.node, parent.node)

        # initialise properties remaining staff
        self.access_functions['style'] = (self.get_style, self.set_style)
        self.properties['style'] = CheckListProperty(
            self, 'style', self.widget_writer)

        self.fields = [[self.name, "-1"]]   # list of 2-lists label, size
        # for the statusbar fields
        self.access_functions['fields'] = (self.get_fields, self.set_fields)
        prop = self.properties['fields'] = GridProperty(
            self, 'fields', None,
            [("Text", GridProperty.STRING), ("Size", GridProperty.INT)])

        # replace the default 'write' method of 'prop' with a custom one
        def write_prop(outfile, tabs):
            inner_xml = u''
            for label, width in self.fields:
                inner_xml += common.format_xml_tag(
                    u'field', label, tabs + 1, width=width)
            stmt = common.format_xml_tag(
                u'fields', inner_xml, tabs, is_xml=True)
            outfile.write(stmt)

        prop.write = write_prop

    def create_widget(self):
        self.widget = wx.StatusBar(self.parent.widget, wx.NewId())
        wx.EVT_LEFT_DOWN(self.widget, self.on_set_focus)
        self.set_fields(self.fields)
        if self.parent.widget:
            self.parent.widget.SetStatusBar(self.widget)

    def create_properties(self):
        EditBase.create_properties(self)
        page = self._common_panel
        self.properties['style'].display(page)
        prop = self.properties['fields']
        prop.display(page)
        sizer = page.GetSizer()
        if not sizer:
            sizer = wx.BoxSizer(wx.VERTICAL)
            sizer.Add(self.name_prop.panel, 0, wx.EXPAND)
            sizer.Add(self.klass_prop.panel, 0, wx.EXPAND)
            page.SetAutoLayout(1)
            compat.SizerItem_SetSizer(page, sizer)
        sizer.Add(self.properties['style'].panel, 0, wx.EXPAND)
        sizer.Add(prop.panel, 1, wx.ALL | wx.EXPAND, 3)
        sizer.Fit(page)
        page.SetSize(self.notebook.GetClientSize())
        sizer.Layout()
        self.notebook.AddPage(page, _("Common"))
        self.property_window.Layout()
        prop.set_col_sizes([190, 0])

    def set_fields(self, values):
        # values is a list of lists
        self.fields = []
        if self.widget:
            self.widget.SetFieldsCount(len(values))
        for i in range(len(values)):
            try:
                v = int(values[i][1])
            except:
                v = 0
            s = misc.wxstr(values[i][0])
            self.fields.append([s, str(v)])
            if self.widget:
                self.widget.SetStatusText(s, i)
        if self.widget:
            self.widget.SetStatusWidths([int(i[1]) for i in self.fields])

    def get_fields(self):
        return self.fields

    def __getitem__(self, key):
        return self.access_functions[key]

    def remove(self, *args, **kwds):
        if not kwds.get('do_nothing', False):
            if self.parent.widget:
                self.parent.widget.SetStatusBar(None)
            try:
                self.parent.properties['statusbar'].set_value(0)
            except KeyError:
                pass
            if self.widget:
                self.widget.Hide()
            EditBase.remove(self)
        else:
            if EditStatusBar._hidden_frame is None:
                EditStatusBar._hidden_frame = wx.Frame(None, -1, "")
            if self.widget is not None:
                self.widget.Reparent(EditStatusBar._hidden_frame)
            self.widget = None

    def popup_menu(self, *args):
        pass  # to avoid strange segfault :)

    def get_property_handler(self, name):
        if name == 'fields':
            return FieldsHandler(self)
        return None

# end of class EditStatusBar


def builder(parent, sizer, pos, number=[0]):
    """\
    factory function for EditToolBar objects.
    """
    class Dialog(wx.Dialog):
        def __init__(self):
            wx.Dialog.__init__(self, None, -1, _('Select toolbar class'))
            if common.app_tree.app.get_language().lower() == 'xrc':
                self.klass = 'wxToolBar'
            else:
                if not number[0]: self.klass = 'MyToolBar'
                else: self.klass = 'MyToolBar%s' % number[0]
                number[0] += 1
            klass_prop = TextProperty(self, 'class', self)
            szr = wx.BoxSizer(wx.VERTICAL)
            szr.Add(klass_prop.panel, 0, wx.EXPAND)
            sz2 = wx.BoxSizer(wx.HORIZONTAL)
            sz2.Add(wx.Button(self, wx.ID_OK, _('OK')), 0, wx.ALL, 3)
            sz2.Add(wx.Button(self, wx.ID_CANCEL, _('Cancel')), 0, wx.ALL, 3)
            szr.Add(sz2, 0, wx.ALL|wx.ALIGN_CENTER, 3)
            self.SetAutoLayout(True)
            self.SetSizer(szr)
            szr.Fit(self)

        def __getitem__(self, value):
            if value == 'class':
                def set_klass(c): self.klass = c
                return lambda : self.klass, set_klass
    # end of inner class

    dialog = Dialog()
    res = dialog.ShowModal()
    klass = dialog.klass
    dialog.Destroy()
    if res != wx.ID_OK:
        if number[0] > 0:
            number[0] -= 1
        return

    name = 'statusbar_%d' % (number[0] or 1)
    while common.app_tree.has_name(name):
        number[0] += 1
        name = 'statusbar_%d' % number[0]

    widget = EditStatusBar(name, klass, parent, common.property_panel)
    widget.node = Tree.Node(widget)
    common.app_tree.add(widget.node)
    widget.show_widget(True)
    widget.show_properties()


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    """\
    factory to build EditStatusBar objects from a XML file
    """
    name = attrs.get('name')
    if parent:
        if name:
            parent.statusbar.set_name(name)
            parent.statusbar.name_prop.set_value(name)
        return parent.statusbar
    else:
        widget = EditStatusBar(name, attrs.get('class', 'wxStatusBar'),
                               None, common.property_panel)
        widget.node = Tree.Node(widget)
        common.app_tree.add(widget.node)
        return widget


def initialize():
    """\
    initialization function for the module: returns a wxBitmapButton to be
    added to the main palette.
    """
    common.widgets_from_xml['EditStatusBar'] = xml_builder
    common.widgets['EditStatusBar'] = builder
    return common.make_object_button('EditStatusBar', 'statusbar.xpm')
