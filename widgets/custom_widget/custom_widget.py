"""\
Custom wxWindow objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
import common, compat, misc
from tree import Tree, Node
from wcodegen.taghandler import BaseXmlBuilderTagHandler
import new_properties as np
from edit_windows import ManagedBase


class ArgumentsProperty(np.GridProperty):
    def write(self, outfile, tabs):
        arguments = self.get()
        if arguments:
            inner_xml = u''
            for argument in arguments:
                inner_xml += common.format_xml_tag(u'argument', argument[0], tabs+1)
            stmt = common.format_xml_tag( u'arguments', inner_xml, tabs, is_xml=True)
            outfile.write(stmt)



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

    def __init__(self, name, klass, parent, id, sizer, pos, show=True):
        ManagedBase.__init__(self, name, klass, parent, id, sizer, pos, show)

        # initialise instance properties
        arguments = [['$parent'], ['$id']]  # ,['$width'],['$height']]
        cols      = [('Arguments', np.GridProperty.STRING)]
        self.arguments   = ArgumentsProperty( arguments, cols )
        self.custom_ctor = np.TextPropertyD("", name="custom_constructor", strip=True)

    def create_widget(self):
        self.widget = wx.Window( self.parent.widget, self.id, style=wx.BORDER_SUNKEN | wx.FULL_REPAINT_ON_RESIZE)
        wx.EVT_PAINT(self.widget, self.on_paint)

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
    def __init__(self, number=[0]):
        title = _('Select widget class')
        wx.Dialog.__init__(self, None, -1, title)
        self.klass = 'CustomWidget'
        if number[0]:
            self.klass = 'CustomWidget%s' % (number[0] - 1)
        number[0] += 1
        import widget_properties
        klass_prop = widget_properties.TextProperty(self, 'class', self, label=_("class"))
        szr = wx.BoxSizer(wx.VERTICAL)
        szr.Add(klass_prop.panel, 0, wx.ALL | wx.EXPAND, 5)
        szr.Add(wx.Button(self, wx.ID_OK, _('OK')), 0, wx.ALL | wx.ALIGN_CENTER, 5)
        self.SetAutoLayout(True)
        self.SetSizer(szr)
        szr.Fit(self)
        w = self.GetTextExtent(title)[0] + 50
        if self.GetSize()[0] < w:
            self.SetSize((w, -1))
        self.CenterOnScreen()

    def __getitem__(self, value):
        def set_klass(c):
            self.klass = c
        return lambda: self.klass, set_klass


def builder(parent, sizer, pos, number=[1]):
    "factory function for CustomWidget objects"

    dialog = Dialog()
    dialog.ShowModal()
    klass = dialog.klass
    dialog.Destroy()

    name = 'window_%d' % number[0]
    while common.app_tree.has_name(name):
        number[0] += 1
        name = 'window_%d' % number[0]
    win = CustomWidget(name, klass, parent, wx.NewId(), sizer, pos)
    node = Node(win)
    win.node = node

    win.properties["proportion"].set(1)
    win.properties["flag"].set("wxEXPAND")
    win.show_widget(True)

    common.app_tree.insert(node, sizer.node, pos-1)
    sizer.set_item(win.pos, 1, wx.EXPAND)


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    "factory to build CustomWidget objects from a XML file"
    from xml_parse import XmlParsingError
    try:
        name = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    if not sizer or not sizeritem:
        raise XmlParsingError(_("sizer or sizeritem object cannot be None"))
    win = CustomWidget(name, 'CustomWidget', parent, wx.NewId(), sizer, pos, True)
    sizer.set_item(win.pos, proportion=sizeritem.proportion, flag=sizeritem.flag, border=sizeritem.border)
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

