"""\
Custom wxWindow objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2017-2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
import common, misc, compat, config
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
            self.parent.properties['arguments'].load(self.arguments)
            return True
        elif name == 'argument':
            char_data = self.get_char_data()
            self.arguments.append([char_data])
        return False



class CustomWidget(ManagedBase):
    """Class to handle custom widgets

    arguments: Constructor arguments
    custom_ctor: if not empty, an arbitrary piece of code that will be used instead of the constructor name"""

    WX_CLASS = "CustomWidget"
    _PROPERTIES = ["Widget", "custom_ctor", "show_design", "show_preview", "arguments"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES

    _PROPERTY_LABELS = { 'custom_constructor':'Cust. constructor' }
    _PROPERTY_HELP   = { 'custom_constructor':'Specify a custom constructor like a factory method',
                         "instance_class":("The class that should be instantiated, e.g. 'mycontrols.MyCtrl'.\n\n"
                            "You need to ensure that the class is available.\n"
                            "Add required import code to 'Extra (import) code for this widget' on the Code tab."),
                         'show_design':("Highly experimental:\nUse custom class and code already in Design window.\n\n"
                                        "Only available if option 'Allow custom widget code in Design and Preview"
                                        " windows' is checked."),
                         'show_preview':("Highly experimental:\nUse custom class and code already in Preview window.\n\n"
                                        "Only available if option 'Allow custom widget code in Design and Preview"
                                        " windows' is checked.")}

    def __init__(self, name, parent, index, instance_class=None):
        ManagedBase.__init__(self, name, parent, index, instance_class or "wxWindow")
        self.properties["instance_class"].deactivated = None

        # initialise instance properties
        cols      = [('Arguments', np.GridProperty.STRING)]
        self.arguments   = ArgumentsProperty( [], cols )
        self.custom_ctor = np.TextPropertyD("", name="custom_constructor", strip=True, default_value="")

        self.show_design = np.CheckBoxProperty(False, default_value=False)
        self.show_preview = np.CheckBoxProperty(False, default_value=False)
        if not config.preferences.allow_custom_widgets:
            self.properties["show_design"].set_blocked()
            self.properties["show_preview"].set_blocked()
        self._error_message = None  # when there's an error message due to the previous option

    def create_widget(self):
        self._error_message = None
        if self.show_design and common.root.language=="python" and config.preferences.allow_custom_widgets:
            # update path
            import os, sys
            dirname = os.path.dirname( common.root.filename )
            if not dirname in sys.path: sys.path.insert(0, dirname)
            # build code
            code_gen = common.code_writers["python"]
            code_gen.new_project(common.root)
            builder = code_gen.obj_builders["CustomWidget"]

            # replace e.g. "self.%s"%name with "self.widget"
            original_widget_access = builder.format_widget_access(self)
            widget_access = "self.widget"
            original_parent_access = builder.format_widget_access(self.parent_window)
            parent_access = "self.parent_window.widget"
            code_gen.cache(self, "attribute_access", widget_access)
            code_gen.cache(self.parent_window, "attribute_access", parent_access)

            lines = []
            if self.check_prop_truth("extracode"):
                lines.append( self.extracode )
            if self.check_prop_truth("extracode_pre"):
                lines.append( self.extracode_pre )
            lines += builder.get_code(self)[0]
            if self.check_prop_truth("extracode_post"):
                lines.append( self.extracode_post )
            if self.check_prop_truth("extraproperties"):
                lines += code_gen.generate_code_extraproperties(self)
            code = "\n".join(lines)
            if self.check_prop_truth("extracode_pre") or self.check_prop_truth("extracode_post"):
                # replace widget and parent access in manually entered extra code
                code = code.replace(original_widget_access, widget_access)
                code = code.replace(original_parent_access, parent_access)
            # execute code
            before = set(sys.modules.keys())
            try:
                exec(code)
                after = set(sys.modules.keys())
                for modulename in (after - before):
                    if modulename in code: print(modulename)
                return
            except:
                exc_type, exc_value, exc_traceback = sys.exc_info()
                self._error_message = "%s: %s"%(exc_type, exc_value)
                if self.widget: self.widget.Destroy()
        # default / fallback in case of exception
        self.widget = wx.Window(self.parent_window.widget, wx.ID_ANY, style=wx.BORDER_SUNKEN | wx.FULL_REPAINT_ON_RESIZE)
        self.widget.Bind(wx.EVT_PAINT, self.on_paint)
        self.widget.Bind(wx.EVT_ERASE_BACKGROUND, self.on_erase_background)
        if self._error_message: compat.SetToolTip( self.widget, self._error_message )

    def finish_widget_creation(self, level):
        ManagedBase.finish_widget_creation(self, level, sel_marker_parent=self.widget)

    def on_erase_background(self, event):
        # ignore event for less flickering; 
        pass

    def on_paint(self, event):
        dc = wx.PaintDC(self.widget)
        dc.SetBrush(wx.WHITE_BRUSH)
        dc.SetPen(wx.BLACK_PEN)
        dc.SetBackground(wx.WHITE_BRUSH)
        dc.Clear()
        w, h = self.widget.GetClientSize()
        dc.DrawLine(0, 0, w, h)
        dc.DrawLine(w, 0, 0, h)
        text = _('Custom Widget: %s') % self.instance_class
        tw, th = dc.GetTextExtent(text)
        x = (w - tw)//2
        y = (h - th)//2
        if self._error_message:
            text = "%s\n%s"%(text, self._error_message)
            dc.SetTextForeground(wx.RED)
        dc.SetPen(wx.ThePenList.FindOrCreatePen(wx.BLACK, 0, wx.TRANSPARENT))  # transparent: border colour
        #dc.SetPen(pen)
        dc.DrawRectangle(x-1, y-1, tw+2, th+2)
        dc.DrawText(text, x, y)

    def get_property_handler(self, name):
        if name == 'arguments':
            return ArgumentsHandler(self)
        return ManagedBase.get_property_handler(self, name)

    def _properties_changed(self, modified, actions):
        if modified and "instance_class" in modified:
            actions.update(("refresh","label"))
        if modified and "show_design" in modified or self.show_design:
            actions.add("recreate2")
        ManagedBase._properties_changed(self, modified, actions)


class Dialog(wx.Dialog):
    import re
    validation_re  = re.compile(r'^[a-zA-Z_\.\:]+[\w-]*(\[\w*\])*$')  # does not avoid ".."
    def __init__(self):
        title = _('Enter widget class')
        wx.Dialog.__init__(self, None, -1, title, wx.GetMousePosition())
        klass = 'CustomWidget'

        self.classname = wx.TextCtrl(self, -1, klass, size=(200,-1))
        sizer = wx.BoxSizer(wx.VERTICAL)
        hsizer = wx.BoxSizer(wx.HORIZONTAL)
        hsizer.Add(wx.StaticText(self, -1, _('Instance class')), 0, wx.ALL | wx.ALIGN_CENTER_VERTICAL, 5)
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
        w = self.GetTextExtent(title)[0] *2
        if self.GetSize()[0] < w:
            self.SetSize((w, -1))
        self.classname.Bind(wx.EVT_TEXT, self.validate)

    def validate(self, event):
        class_name = self.classname.GetValue()
        OK = bool( self.validation_re.match(class_name) )
        if ".." in class_name or class_name.endswith("."): OK = False
        if class_name.startswith(":") or (":" in class_name and not "::" in class_name): OK = False
        self.OK_button.Enable( OK )


def builder(parent, index):
    "factory function for CustomWidget objects"

    dialog = Dialog()
    with misc.disable_stay_on_top(common.adding_window or parent):
        res = dialog.ShowModal()
    klass = dialog.classname.GetValue().strip()
    dialog.Destroy()
    if res != wx.ID_OK: return

    name = parent.toplevel_parent.get_next_contained_name('window_%d')
    with parent.frozen():
        editor = CustomWidget(name, parent, index, klass)
        editor.properties["arguments"].set( [['$parent'], ['$id']] )  # ,['$width'],['$height']]
        editor.properties["proportion"].set(1)
        editor.properties["flag"].set("wxEXPAND")
        if parent.widget: editor.create()
    return editor


def xml_builder(parser, base, name, parent, index):
    "factory to build CustomWidget objects from a XML file"
    return CustomWidget(name, parent, index)


def initialize():
    "initialization function for the module: returns a wx.BitmapButton to be added to the main palette"
    common.widget_classes['CustomWidget'] = CustomWidget
    common.widgets['CustomWidget'] = builder
    common.widgets_from_xml['CustomWidget'] = xml_builder

    return common.make_object_button('CustomWidget', 'custom.png', tip='Add a custom widget')

