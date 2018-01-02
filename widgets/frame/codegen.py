"""\
Code generator functions for wxFrame objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016-2017 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PythonFrameCodeGenerator(wcodegen.PythonWidgetCodeWriter):
    def get_code(self, obj):
        return [], [], []

    def get_properties_code(self, obj):
        out = []
        if obj.title:
            out.append('self.SetTitle(%s)\n' % self.codegen.quote_str(obj.title))
        if obj.icon:
            stmt_icon = self.generate_code_bitmap(obj.icon)
            out.append('_icon = %s\n' % self.cn('wxNullIcon'))
            out.append('_icon.CopyFromBitmap(%s)\n' % stmt_icon)
            out.append('self.SetIcon(_icon)\n')
        out.extend(self.codegen.generate_common_properties(obj))
        return out

    def get_layout_code(self, obj):
        ret = ['self.Layout()\n']
        if "centered" in obj.properties and obj.centered:
            ret.append('self.Centre()\n')
        return ret


def xrc_frame_code_generator(obj):
    xrcgen = common.code_writers['XRC']

    class FrameXrcObject(xrcgen.DefaultXrcObject):
        def write(self, output, tabs):
            properties = {"menubar":None, "statusbar":None, "toolbar":None}
            xrcgen.DefaultXrcObject.write(self, output, tabs, properties)

        def write_property(self, name, val, output, ntabs):
            if name != 'sizehints':
                xrcgen.DefaultXrcObject.write_property(self, name, val, output, ntabs)

    return FrameXrcObject(obj)


def xrc_statusbar_code_generator(obj):
    xrcgen = common.code_writers['XRC']

    class StatusbarXrcObject(xrcgen.DefaultXrcObject):
        def write(self, output, tabs):
            properties = {"statusbar":None}
            prop = self.widget.properties['statusbar']
            if prop.is_active():
                fields, widths = self.widget.statusbar
                properties['fields'] = str(len(fields))
                properties['widths'] = ', '.join([str(w) for w in widths])
            xrcgen.DefaultXrcObject.write(self, output, tabs, properties)

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
        if obj.title:
            out.append('SetTitle(%s);\n' % self.codegen.quote_str(obj.title))
        if obj.icon:
            stmt_icon = self.generate_code_bitmap(obj.icon)
            out.append('wxIcon _icon;\n')
            out.append('_icon.CopyFromBitmap(%s);\n' % stmt_icon)
            out.append('SetIcon(_icon);\n')
        out.extend(self.codegen.generate_common_properties(obj))
        return out

    def get_layout_code(self, obj):
        ret = ['Layout();\n']
        if obj.centered:
            ret.append('Centre();\n')
        return ret


class CppMDIChildFrameCodeGenerator(CppFrameCodeGenerator):
    import_modules = ['<wx/mdi.h>']

    constructor = [('wxMDIParentFrame*', 'parent'), ('wxWindowID', 'id'),
                   ('const wxString&', 'title'),
                   ('const wxPoint&', 'pos', 'wxDefaultPosition'),
                   ('const wxSize&', 'size', 'wxDefaultSize'),
                   ('long', 'style', 'wxDEFAULT_FRAME_STYLE')]


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
        #aph('menubar', pygen.DummyPropertyHandler)

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

        #aph = cppgen.add_property_handler
        #aph('menubar', cppgen.DummyPropertyHandler)
