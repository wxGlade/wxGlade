"""\
Code generator functions for wxFrame objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen
from wcodegen.taghandler import BaseCodeWriterTagHandler


class PythonFrameCodeGenerator(wcodegen.PythonWidgetCodeWriter):
    def get_code(self, obj):
        return [], [], []

    def get_properties_code(self, obj):
        out = []
        title = obj.properties.get('title')
        if title:
            out.append('self.SetTitle(%s)\n' % self.codegen.quote_str(title))
        icon = obj.properties.get('icon')
        if icon:
            stmt_icon = self.generate_code_bitmap(icon, obj.preview)
            out.append('_icon = %s\n' % self.cn('wxNullIcon'))
            out.append('_icon.CopyFromBitmap(%s)\n' % stmt_icon)
            out.append('self.SetIcon(_icon)\n')
        out.extend(self.codegen.generate_common_properties(obj))
        return out

    def get_layout_code(self, obj):
        ret = ['self.Layout()\n']
        try:
            if int(obj.properties['centered']):
                ret.append('self.Centre()\n')
        except (KeyError, ValueError):
            pass
        return ret

# end of class PythonFrameCodeGenerator


# property handlers for code generation
class StatusFieldsHandler(BaseCodeWriterTagHandler):
    """\
    Handler for statusbar fields
    """

    def __init__(self):
        super(StatusFieldsHandler, self).__init__()
        self.labels = []
        self.widths = []
        self.curr_label = []

    def start_elem(self, name, attrs):
        if name == 'field':
            self.widths.append(int(attrs.get('width', -1)))

    def end_elem(self, name, code_obj):
        if name == 'fields':
            code_obj.properties['statusbar'] = (self.labels, self.widths)
            return True
        char_data = self.get_char_data()
        self.labels.append(char_data)

# end of class StatusFieldsHandler


def xrc_frame_code_generator(obj):
    xrcgen = common.code_writers['XRC']

    class FrameXrcObject(xrcgen.DefaultXrcObject):
        def write(self, outfile, tabs):
            if 'menubar' in self.properties:
                del self.properties['menubar']
            if 'statusbar' in self.properties:
                del self.properties['statusbar']
            if 'toolbar' in self.properties:
                del self.properties['toolbar']
            xrcgen.DefaultXrcObject.write(self, outfile, tabs)

        def write_property(self, name, val, outfile, ntabs):
            if name != 'sizehints':
                xrcgen.DefaultXrcObject.write_property(
                    self, name, val, outfile, ntabs)

    # end of class FrameXrcObject

    return FrameXrcObject(obj)


def xrc_statusbar_code_generator(obj):
    xrcgen = common.code_writers['XRC']

    class StatusbarXrcObject(xrcgen.DefaultXrcObject):
        def write(self, outfile, tabs):
            if 'statusbar' in self.properties:
                fields, widths = self.properties['statusbar']
                self.properties['fields'] = str(len(fields))
                self.properties['widths'] = ', '.join([str(w) for w in widths])
                del self.properties['statusbar']
            xrcgen.DefaultXrcObject.write(self, outfile, tabs)

    # end of class StatusbarXrcObject

    return StatusbarXrcObject(obj)


class CppFrameCodeGenerator(wcodegen.CppWidgetCodeWriter):
    constructor = [('wxWindow*', 'parent'), ('wxWindowID', 'id'),
                   ('const wxString&', 'title'),
                   ('const wxPoint&', 'pos', 'wxDefaultPosition'),
                   ('const wxSize&', 'size', 'wxDefaultSize'),
                   ('long', 'style', 'wxDEFAULT_FRAME_STYLE')]

    def get_code(self, obj):
        return [], [], [], []  # the frame can't be a children

    def get_properties_code(self, obj):
        out = []
        title = obj.properties.get('title')
        if title:
            out.append('SetTitle(%s);\n' % self.codegen.quote_str(title))
        icon = obj.properties.get('icon')
        if icon:
            stmt_icon = self.generate_code_bitmap(icon, obj.preview)
            out.append('wxIcon _icon;\n')
            out.append('_icon.CopyFromBitmap(%s);\n' % stmt_icon)
            out.append('SetIcon(_icon);\n')
        out.extend(self.codegen.generate_common_properties(obj))
        return out

    def get_layout_code(self, obj):
        ret = ['Layout();\n']
        try:
            if int(obj.properties['centered']):
                ret.append('Centre();\n')
        except (KeyError, ValueError):
            pass
        return ret

# end of class CppFrameCodeGenerator


class CppMDIChildFrameCodeGenerator(CppFrameCodeGenerator):
    import_modules = ['<wx/mdi.h>']

    constructor = [('wxMDIParentFrame*', 'parent'), ('wxWindowID', 'id'),
                   ('const wxString&', 'title'),
                   ('const wxPoint&', 'pos', 'wxDefaultPosition'),
                   ('const wxSize&', 'size', 'wxDefaultSize'),
                   ('long', 'style', 'wxDEFAULT_FRAME_STYLE')]

# end of class CppMDIChildFrameCodeGenerator


def initialize():
    klass = 'wxFrame'
    cn = common.class_names
    cn['EditFrame'] = klass
    cn['EditMDIChildFrame'] = 'wxMDIChildFrame'
    common.toplevels['EditFrame'] = 1
    common.toplevels['EditMDIChildFrame'] = 1

    pygen = common.code_writers.get('python')
    if pygen:
        awh = pygen.add_widget_handler
        awh('wxFrame', PythonFrameCodeGenerator(klass))
        awh('wxMDIChildFrame', PythonFrameCodeGenerator(klass))

        aph = pygen.add_property_handler
        aph('menubar', pygen.DummyPropertyHandler)

    xrcgen = common.code_writers.get('XRC')
    if xrcgen:
        awh = xrcgen.add_widget_handler
        awh('wxFrame', xrc_frame_code_generator)
        awh('wxMDIChildFrame', xrcgen.NotImplementedXrcObject)

    cppgen = common.code_writers.get('C++')
    if cppgen:
        awh = cppgen.add_widget_handler
        awh('wxFrame', CppFrameCodeGenerator(klass))
        awh('wxMDIChildFrame', CppMDIChildFrameCodeGenerator(klass))

        aph = cppgen.add_property_handler
        aph('menubar', cppgen.DummyPropertyHandler)
