"""\
Custom wxWindow objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2017-2019 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
import common, misc
from tree import Node
from wcodegen.taghandler import BaseXmlBuilderTagHandler
import new_properties as np
from edit_windows import ManagedBase


class ArgumentsProperty(np.GridProperty):
    SKIP_EMPTY = True
    def __init__(self, arguments, cols):
        np.GridProperty.__init__( self, arguments, cols, immediate=True)
    def write(self, output, tabs):
        arguments = self.get()
        if arguments:
            inner_xml = []
            for argument in arguments:
                inner_xml += common.format_xml_tag(u'argument', argument, tabs+1)
            output.extend( common.format_xml_tag( u'arguments', inner_xml, tabs, is_xml=True) )
    def get(self):
        "get the value, or the default value if deactivated; usually not used directly, as owner.property will call it"
        ret = np.GridProperty.get(self) or []
        return [row[0] for row in ret]


class ArgumentsHandler(BaseXmlBuilderTagHandler):
    def __init__(self, parent):
        super(ArgumentsHandler, self).__init__()
        self.parent = parent
        self.arguments = []

    def end_elem(self, name):
        if name == 'arguments':
            self.parent.properties['arguments'].set(self.arguments)
            self.parent.properties_changed(["arguments"])
            return True
        elif name == 'argument':
            char_data = self.get_char_data()
            self.arguments.append([char_data])
        return False



class CustomWidget(ManagedBase):
    """Class to handle custom widgets

    arguments: Constructor arguments
    custom_ctor: if not empty, an arbitrary piece of code that will be used instead of the constructor name"""

    _PROPERTIES = ["Widget", "custom_ctor", "arguments"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES

    _PROPERTY_LABELS = { 'custom_constructor':'Custom constructor' }
    _PROPERTY_HELP   = { 'custom_constructor':'Specify a custom constructor like a factory method' }

    def __init__(self, name, klass, parent, id, sizer, pos):
        ManagedBase.__init__(self, name, klass, parent, id, sizer, pos)

        # initialise instance properties
        cols      = [('Arguments', np.GridProperty.STRING)]
        self.arguments   = ArgumentsProperty( [], cols )
        self.custom_ctor = np.TextPropertyD("", name="custom_constructor", strip=True, default_value="")

    def create_widget(self):
        self.widget = wx.Window( self.parent.widget, self.id, style=wx.BORDER_SUNKEN | wx.FULL_REPAINT_ON_RESIZE)
        self.widget.Bind(wx.EVT_PAINT, self.on_paint)
        self.widget.Bind(wx.EVT_ERASE_BACKGROUND, lambda event:None)

    def finish_widget_creation(self):
        ManagedBase.finish_widget_creation(self, sel_marker_parent=self.widget)

    def on_paint(self, event):
        dc = wx.PaintDC(self.widget)
        dc.SetBrush(wx.WHITE_BRUSH)
        dc.SetPen(wx.BLACK_PEN)
        dc.SetBackground(wx.WHITE_BRUSH)
        dc.Clear()
        w, h = self.widget.GetClientSize()
        dc.DrawLine(0, 0, w, h)
        dc.DrawLine(w, 0, 0, h)
        text = _('Custom Widget: %s') % self.klass
        tw, th = dc.GetTextExtent(text)
        x = (w - tw)//2
        y = (h - th)//2
        dc.SetPen(wx.ThePenList.FindOrCreatePen(wx.BLACK, 0, wx.TRANSPARENT))
        dc.DrawRectangle(x-1, y-1, tw+2, th+2)
        dc.DrawText(text, x, y)

    def get_property_handler(self, name):
        if name == 'arguments':
            return ArgumentsHandler(self)
        return ManagedBase.get_property_handler(self, name)

    def properties_changed(self, modified):
        if not modified or "klass" in modified:
            if self.widget:
                self.widget.Refresh()
        ManagedBase.properties_changed(self, modified)


class Dialog(wx.Dialog):
    import re
    validation_re  = re.compile(r'^[a-zA-Z_\.]+[\w-]*(\[\w*\])*$')  # does not avoid ".."
    def __init__(self):
        title = _('Enter widget class')
        pos = wx.GetMousePosition()
        wx.Dialog.__init__(self, None, -1, title, pos)
        klass = 'CustomWidget'

        self.classname = wx.TextCtrl(self, -1, klass)
        sizer = wx.BoxSizer(wx.VERTICAL)
        hsizer = wx.BoxSizer(wx.HORIZONTAL)
        hsizer.Add(wx.StaticText(self, -1, _('class')), 0, wx.ALL | wx.ALIGN_CENTER_VERTICAL, 5)
        hsizer.Add(self.classname, 1, wx.ALIGN_CENTER_VERTICAL|wx.TOP|wx.BOTTOM|wx.RIGHT, 3)
        sizer.Add(hsizer, 0, wx.EXPAND)

        # horizontal sizer for action buttons
        hsizer = wx.BoxSizer(wx.HORIZONTAL)
        hsizer.Add( wx.Button(self, wx.ID_CANCEL, _('Cancel')), 1, wx.ALL, 5)
        self.OK_button = btn = wx.Button(self, wx.ID_OK, _('OK') )
        btn.SetDefault()
        hsizer.Add(btn, 1, wx.ALL, 5)
        sizer.Add(hsizer, 0, wx.EXPAND)

        self.SetAutoLayout(True)
        self.SetSizer(sizer)
        sizer.Fit(self)
        w = self.GetTextExtent(title)[0] + 50
        if self.GetSize()[0] < w:
            self.SetSize((w, -1))
        self.classname.Bind(wx.EVT_TEXT, self.validate)

    def validate(self, event):
        class_name = self.classname.GetValue()
        OK = bool( self.validation_re.match(class_name) )
        if ".." in class_name or class_name.endswith("."): OK = False
        self.OK_button.Enable( OK )


def builder(parent, sizer, pos, number=[1]):
    "factory function for CustomWidget objects"

    dialog = Dialog()
    with misc.disable_stay_on_top(common.adding_window or parent):
        res = dialog.ShowModal()
    klass = dialog.classname.GetValue().strip()
    dialog.Destroy()
    if res != wx.ID_OK: return

    name = 'window_%d' % number[0]
    while common.app_tree.has_name(name):
        number[0] += 1
        name = 'window_%d' % number[0]
    with parent.frozen():
        win = CustomWidget(name, klass, parent, wx.NewId(), sizer, pos)
        node = Node(win)
        win.node = node

        win.properties["arguments"].set( [['$parent'], ['$id']] )  # ,['$width'],['$height']]
        win.properties["proportion"].set(1)
        win.properties["flag"].set("wxEXPAND")
        if parent.widget: win.create()

    common.app_tree.insert(node, sizer.node, pos-1)
    #sizer.set_item(win.pos, 1, wx.EXPAND)


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    "factory to build CustomWidget objects from a XML file"
    from xml_parse import XmlParsingError
    try:
        name = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    if not sizer or not sizeritem:
        raise XmlParsingError(_("sizer or sizeritem object cannot be None"))
    klass = attrs.get("class", "CustomWidget")
    win = CustomWidget(name, klass, parent, wx.NewId(), sizer, pos)
    #sizer.set_item(win.pos, proportion=sizeritem.proportion, span=sizeritem.span, flag=sizeritem.flag, border=sizeritem.border)
    node = Node(win)
    win.node = node
    if pos is None: common.app_tree.add(node, sizer.node)
    else: common.app_tree.insert(node, sizer.node, pos-1)
    return win


def initialize():
    "initialization function for the module: returns a wx.BitmapButton to be added to the main palette"
    common.widgets['CustomWidget'] = builder
    common.widgets_from_xml['CustomWidget'] = xml_builder

    return common.make_object_button('CustomWidget', 'custom.xpm', tip='Add a custom widget')

