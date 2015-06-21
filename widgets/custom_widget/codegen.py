"""
Code generator functions for CustomWidget objects

@copyright: 2002-2007 Alberto Griggio
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import logging

import common
import wcodegen


class ArgumentsCodeHandler(object):
    def __init__(self):
        self.arguments = []
        self.curr_arg = []

    def start_elem(self, name, attrs):
        pass

    def end_elem(self, name, code_obj):
        if name == 'arguments':
            code_obj.properties['arguments'] = self.arguments
            return True
        elif name == 'argument':
            tab_name = "".join(self.curr_arg)
            self.arguments.append(tab_name)
            self.curr_arg = []
        return False

    def char_data(self, data):
        self.curr_arg.append(data)

# end of class ArgumentsCodeHandler


def _fix_arguments(arguments, parent, id, size):
    # adding $width e $height:
    vSize = size.split(',')
    for i in range(len(arguments)):
        if arguments[i] == '$parent': arguments[i] = parent
        elif arguments[i] == '$id': arguments[i] = id
        elif arguments[i] == '$width': arguments[i] = vSize[0]
        elif arguments[i] == '$height': arguments[i] = vSize[1]
    return arguments


class PythonCustomWidgetGenerator(wcodegen.PythonWidgetCodeWriter):
    def get_code(self, widget):
        if widget.preview and widget.klass not in widget.parser.class_names:
            # if this CustomWidget refers to another class in the same wxg
            # file, use that for the preview
            return self.get_code_preview(widget)
        prop = widget.properties
        id_name, id = self.codegen.generate_code_id(widget)
        parent = self.format_widget_access(widget.parent)
        init = []
        if id_name: init.append(id_name)
        arguments = _fix_arguments(
            prop.get('arguments', []), parent, id,
            prop.get('size', '-1, -1').strip())
        ctor = widget.klass
        cust_ctor = prop.get('custom_ctor', '').strip()
        if cust_ctor:
            ctor = cust_ctor
        init.append('self.%s = %s(%s)\n' % (widget.name, ctor,
                                            ", ".join(arguments)))
        props_buf = self.codegen.generate_common_properties(widget)
        return init, props_buf, []

    def get_code_preview(self, widget):
        parent = self.format_widget_access(widget.parent)
        init = []
        append = init.append
        append('self.%s = wx.Window(%s, -1)\n' % (widget.name, parent))
        on_paint_code = """\
def self_%s_on_paint(event):
    widget = self.%s
    dc = wx.PaintDC(widget)
    dc.BeginDrawing()
    dc.SetBrush(wx.WHITE_BRUSH)
    dc.SetPen(wx.BLACK_PEN)
    dc.SetBackground(wx.WHITE_BRUSH)
    dc.Clear()
    w, h = widget.GetClientSize()
    dc.DrawLine(0, 0, w, h)
    dc.DrawLine(w, 0, 0, h)
    text = 'Custom Widget: %s'
    tw, th = dc.GetTextExtent(text)
    x = (w - tw)/2
    y = (h - th)/2
    dc.SetPen(wx.ThePenList.FindOrCreatePen(wx.BLACK, 0, wx.TRANSPARENT))
    dc.DrawRectangle(x-1, y-1, tw+2, th+2)
    dc.DrawText(text, x, y)
    dc.EndDrawing()    
""" % ((widget.name,) * 3)
        for line in on_paint_code.splitlines():
            append(line + '\n')        
        append('wx.EVT_PAINT(self.%s, self_%s_on_paint)\n' %
               (widget.name, widget.name))
        return init, [], []

# end of class PythonCustomWidgetGenerator


class CppCustomWidgetGenerator(wcodegen.CppWidgetCodeWriter):
    def get_code(self, widget):
        prop = widget.properties
        id_name, id = self.codegen.generate_code_id(widget)
        if id_name:
            ids = [id_name]
        else:
            ids = []
        if not widget.parent.is_toplevel:
            parent = '%s' % widget.parent.name
        else:
            parent = 'this'
        arguments = _fix_arguments(
            prop.get('arguments', []), parent, id,
            prop.get('size', '-1, -1').strip())
        ctor = 'new ' + widget.klass
        cust_ctor = prop.get('custom_ctor', '').strip()
        if cust_ctor:
            ctor = cust_ctor
        init = ['%s = %s(%s);\n' % (widget.name, ctor,
                                    ", ".join(arguments)) ]
        props_buf = self.codegen.generate_common_properties(widget)
        return init, ids, props_buf, []

# end of class CppCustomWidgetGenerator


def xrc_code_generator(obj):
    xrcgen = common.code_writers['XRC']

    class CustomXrcObject(xrcgen.DefaultXrcObject):
        from xml.sax.saxutils import escape

        def write(self, outfile, ntabs):
            # first, fix the class:
            self.klass = obj.klass
            # delete the custom constructor property
            if 'custom_ctor' in self.properties:
                del self.properties['custom_ctor']
            # then, the attributes:
            if 'arguments' in self.properties:
                args = self.properties['arguments']
                del self.properties['arguments']
                for arg in args:           
                    try:
                        name, val = [s.strip() for s in arg.split(':', 1)]
                    except Exception:
                        # show malformed arguments
                        logging.warning(
                            'Ignore malformed argument "%s" for "%s". '
                            'Argument format should be: name:value',
                            arg, self.klass
                        )
                        continue
                    self.properties[name] = val
            xrcgen.DefaultXrcObject.write(self, outfile, ntabs)

    # end of class CustomXrcObject

    return CustomXrcObject(obj)


def initialize():
    klass = 'CustomWidget'
    common.class_names[klass] = klass
    common.register('python', klass, PythonCustomWidgetGenerator(klass),
                    'arguments', ArgumentsCodeHandler, klass)
    common.register('C++', klass, CppCustomWidgetGenerator(klass),
                    'arguments', ArgumentsCodeHandler, klass)
    common.register('XRC', klass, xrc_code_generator,
                    'arguments', ArgumentsCodeHandler, klass)
