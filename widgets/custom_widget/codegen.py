"""
Code generator functions for CustomWidget objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2016 Carsten Grohmann
@copyright: 2017-2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import logging, common, config
import wcodegen


def format_ctor_arguments(arguments, parent, id, size):
    """Format constructor arguments; returns a list

    arguments: Constructor arguments (list)
    parent: Parent widget (string or unicode)
    id: Widget ID e.g. wxID_ANY
    size: Widget size 'width, height'"""
    vSize = size.split(',')
    for i in range(len(arguments)):
        if arguments[i] == '$parent':
            arguments[i] = parent
        elif arguments[i] == '$id':
            arguments[i] = id
        elif arguments[i] == '$width':
            arguments[i] = vSize[0]
        elif arguments[i] == '$height':
            arguments[i] = vSize[1]
    return arguments


class PythonCustomWidgetGenerator(wcodegen.PythonWidgetCodeWriter):
    def get_code(self, widget):
        if self.codegen.preview and (not widget.show_preview or not config.preferences.allow_custom_widgets):
            # if this CustomWidget refers to another class in the same wxg
            # file, use that for the preview
            return self.get_code_preview(widget)
        self.codegen.have_extracode = True
        id_name, id = self.codegen.generate_code_id(widget)
        parent_access = self.format_widget_access(widget.parent_window)
        init = []
        if id_name: init.append(id_name)
        arguments = format_ctor_arguments( widget.arguments, parent_access, id, widget.size)
        ctor = widget.custom_ctor.strip() or widget.instance_class
        widget_access = self.format_widget_access(widget)
        init.append( '%s = %s(%s)\n' % (widget_access, ctor, ", ".join(arguments)) )
        init += self.codegen.generate_code_common_properties(widget)
        return init, []

    def get_code_preview(self, widget):
        parent = self.format_widget_access(widget.parent_window)
        init = []
        append = init.append
        append('self.%s = wx.Window(%s, -1, style=wx.FULL_REPAINT_ON_RESIZE)\n' % (widget.name, parent))
        if widget.check_prop('size'):
            append( self.codegen.generate_code_size(widget) )
        else:
            # set a minimum size
            append( self.codegen.generate_code_size(widget, "10, 10") )
        on_paint_code = """\
def self_%s_on_paint(event):
    widget = self.%s
    dc = wx.PaintDC(widget)
    dc.SetBrush(wx.WHITE_BRUSH)
    dc.SetPen(wx.BLACK_PEN)
    dc.SetBackground(wx.WHITE_BRUSH)
    dc.Clear()
    w, h = widget.GetClientSize()
    dc.DrawLine(0, 0, w, h)
    dc.DrawLine(w, 0, 0, h)
    text = 'Custom Widget: %s'
    tw, th = dc.GetTextExtent(text)
    x = (w - tw)//2
    y = (h - th)//2
    dc.SetPen(wx.ThePenList.FindOrCreatePen(wx.BLACK, 0, wx.TRANSPARENT))
    dc.DrawRectangle(x-1, y-1, tw+2, th+2)
    dc.DrawText(text, x, y)
""" % ((widget.name,) * 3)
        for line in on_paint_code.splitlines():
            append(line + '\n')
        append( 'self.%s.Bind(wx.EVT_PAINT, self_%s_on_paint)\n' % (widget.name, widget.name) )
        append( 'self.%s.Bind(wx.EVT_ERASE_BACKGROUND, lambda event: None)\n' % widget.name )
        return init, []


class CppCustomWidgetGenerator(wcodegen.CppWidgetCodeWriter):
    def get_code(self, widget):
        id_name, id = self.codegen.generate_code_id(widget)
        if id_name:
            ids = [id_name]
        else:
            ids = []

        parent = self.format_widget_access(widget.parent_window)

        arguments = format_ctor_arguments( widget.arguments, parent, id, widget.size )
        ctor = widget.custom_ctor.strip() or ('new ' + widget.instance_class)
        init = [ '%s = %s(%s);\n' % (widget.name, ctor, ", ".join(arguments)) ]
        init += self.codegen.generate_code_common_properties(widget)
        return init, ids, []



def xrc_code_generator(obj):
    xrcgen = common.code_writers['XRC']

    class CustomXrcObject(xrcgen.DefaultXrcObject):
        # return value for xrc_code_generator
        def write(self, outfile, ntabs, properties=None):
            if properties is None: properties = {}
            # first, fix the class:
            self.subclass = self.klass = obj.instance_class
            # delete the custom constructor property
            properties['custom_ctor'] = None
            # then, the attributes:
            args_p = self.widget.properties['arguments']
            if args_p.is_active() and args_p.value!=args_p.default_value:
                for arg in args_p.value:
                    arg = arg[0].strip()
                    if arg in ("$parent", "$id"): continue
                    try:
                        name, val = [s.strip() for s in arg.split(':', 1)]
                    except Exception:
                        # show malformed arguments
                        logging.warning('Ignore malformed argument "%s" for "%s". Argument format should be: name:value',
                                        arg, self.klass )
                        continue
                    properties[name] = val
            properties["arguments"] = None
            xrcgen.DefaultXrcObject.write(self, outfile, ntabs, properties)

    return CustomXrcObject(obj)


def initialize():
    klass = 'CustomWidget'
    common.class_names[klass] = klass
    common.register('python', klass, PythonCustomWidgetGenerator(klass) )
    common.register('C++',    klass, CppCustomWidgetGenerator(klass) )
    common.register('XRC',    klass, xrc_code_generator )
