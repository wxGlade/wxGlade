"""\
wxFrame and wxStatusBar objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016-2019 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
import common, misc
from tree import Node, WidgetTree
from wcodegen.taghandler import BaseXmlBuilderTagHandler
import new_properties as np
from edit_windows import EditBase, EditStylesMixin


class FieldsHandler(BaseXmlBuilderTagHandler):
    "Custom Property handler for statusbar fields"

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
            self.owner.properties["fields"].set(self.fields)
            self.owner.properties_changed(["fields"])
            return True


class FieldsProperty(np.GridProperty):
    # replace the default 'write' method of 'prop' with a custom one
    def __init__(self, value):
        cols = [("Text", np.GridProperty.STRING), ("Size", np.GridProperty.INT)]
        col_sizes = [190, 0]
        np.GridProperty.__init__(self, value, cols, col_sizes=col_sizes)

    def write(self, output, tabs):
        inner_xml = []
        for label, width in self.value:
            inner_xml += common.format_xml_tag( u'field', label, tabs+1, width=width )
        output.extend( common.format_xml_tag( u'fields', inner_xml, tabs, is_xml=True ) )


class EditStatusBar(EditBase, EditStylesMixin):
    _hidden_frame = None
    update_widget_style = False  # updating does not seem to have an effect

    _PROPERTIES = ["Widget", "style", "fields"]
    PROPERTIES = EditBase.PROPERTIES + _PROPERTIES + EditBase.EXTRA_PROPERTIES

    def __init__(self, name, klass, parent):
        EditBase.__init__( self, name, klass, parent, wx.NewId(), custom_class=False )
        EditStylesMixin.__init__(self)

        # for the statusbar fields
        fields = [[self.name, "-1"]]  # list of 2-lists label, size
        self.fields = FieldsProperty(fields)
        self.window_id = None  # just a dummy for code generation

        self.node = Node(self)
        common.app_tree.add(self.node, parent.node)

    def create_widget(self):
        self.widget = wx.StatusBar(self.parent.widget, -1)
        self.widget.Bind(wx.EVT_LEFT_DOWN, self.on_set_focus)
        self._set_fields()
        if self.parent.widget:
            self.parent.widget.SetStatusBar(self.widget)

    def _set_fields(self):
        if not self.widget: return
        fields = self.fields  # values is a list of lists
        self.widget.SetFieldsCount(len(fields))
        widths = []
        for i, (s,width) in enumerate(fields):
            try:
                widths.append( int(width) )
            except:
                widths.append(0)
            self.widget.SetStatusText(s, i)

        self.widget.SetStatusWidths(widths)

    def remove(self, *args, **kwds):
        if not kwds.get('do_nothing', False):
            self.parent.properties['statusbar'].set(False)
            if self.parent.widget:
                self.parent.widget.SetStatusBar(None)
            try:
                self.parent._statusbar = None
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

    def properties_changed(self, modified):
        if not modified or "fields" in modified:
            self._set_fields()
        EditStylesMixin.properties_changed(self, modified)
        EditBase.properties_changed(self, modified)

    def check_compatibility(self, widget, typename=None, report=False):
        return (False,"No pasting possible here.")
    def check_drop_compatibility(self):
        return (False,"Edit fields in Properties -> Widget")


_NUMBER = 0

class Dialog(wx.Dialog):
    def __init__(self):
        global _NUMBER
        wx.Dialog.__init__(self, None, -1, _('Select toolbar class'))
        
        if common.app_tree.app.language.lower() == 'xrc':
            klass = 'wxToolBar'
        else:
            klass = 'MyToolBar%s' % (_NUMBER or "")
            _NUMBER += 1

        # class
        self.klass = wx.TextCtrl(self, -1, klass)
        self.klass.Bind(wx.EVT_TEXT, self.on_text)
        # layout
        szr = wx.BoxSizer(wx.VERTICAL)
        szr.Add(klass_prop.panel, 0, wx.EXPAND)
        sz2 = wx.BoxSizer(wx.HORIZONTAL)
        sz2.Add(wx.Button(self, wx.ID_OK, _('OK')), 0, wx.ALL, 3)
        sz2.Add(wx.Button(self, wx.ID_CANCEL, _('Cancel')), 0, wx.ALL, 3)
        szr.Add(sz2, 0, wx.ALL|wx.ALIGN_CENTER, 3)
        self.SetAutoLayout(True)
        self.SetSizer(szr)
        szr.Fit(self)


def builder(parent, sizer, pos):
    "factory function for EditToolBar objects"

    dialog = Dialog()
    with misc.disable_stay_on_top(common.adding_window or parent):
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

    with parent.frozen():
        widget = EditStatusBar(name, klass, parent)
        widget.node = Node(widget)
        common.app_tree.add(widget.node)
        if parent.widget: widget.create()



def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    "factory to build EditStatusBar objects from a XML file"
    name = attrs.get('name')
    if parent:
        if name:
            parent._statusbar.properties["name"].set(name)
            parent._statusbar.properties_changed(["name"])
        return parent._statusbar
    else:
        widget = EditStatusBar(name, attrs.get('class', 'wxStatusBar'), None)
        widget.node = Node(widget)
        common.app_tree.add(widget.node)
        return widget


def initialize():
    "initialization function for the module: returns a wxBitmapButton to be added to the main palette."
    common.widgets_from_xml['EditStatusBar'] = xml_builder
    common.widgets['EditStatusBar'] = builder
    #return common.make_object_button('EditStatusBar', 'statusbar.xpm')
    # no standalone status bar any more
    import config, os
    WidgetTree.images['EditStatusBar'] = os.path.join(config.icons_path, 'statusbar.xpm')
    return []
