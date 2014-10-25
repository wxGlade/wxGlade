"""\
Code generator functions for wxFrame objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PythonFrameCodeGenerator(wcodegen.PythonWidgetCodeWriter):
    def get_code(self, obj):
        return [], [], []

    def get_properties_code(self, obj):
        cn = self.cn
        out = []
        title = obj.properties.get('title')
        if title:
            out.append('self.SetTitle(%s)\n' % self.codegen.quote_str(title))
        icon = obj.properties.get('icon')
        if icon:
            if icon.startswith('var:'):
                if not obj.preview:
                    out.append('_icon = ' + cn('wxEmptyIcon') + '()\n')
                    out.append(('_icon.CopyFromBitmap(' + cn('wxBitmap') +
                                '(%s, ' + cn('wxBITMAP_TYPE_ANY') + '))\n') %
                               icon[4:].strip())
                    out.append('self.SetIcon(_icon)\n')
            elif icon.startswith('code:'):
                if not obj.preview:
                    out.append('_icon = ' + cn('wxEmptyIcon') + '()\n')
                    out.append('_icon.CopyFromBitmap(%s)\n' %
                               icon[5:].strip())
                    out.append('self.SetIcon(_icon)\n')
            else:
                if obj.preview:
                    import misc
                    icon = misc.get_relative_path(icon, True)
                out.append('_icon = ' + cn('wxEmptyIcon') + '()\n')
                out.append(('_icon.CopyFromBitmap(' + cn('wxBitmap') +
                            '(%s, ' + cn('wxBITMAP_TYPE_ANY') + '))\n') %
                           self.codegen.quote_path(icon))
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
class StatusFieldsHandler(object):
    """Handler for statusbar fields"""
    def __init__(self):
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
        self.labels.append("".join(self.curr_label))
        self.curr_label = []

    def char_data(self, data):
        self.curr_label.append(data)

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
    constructor = [('wxWindow*', 'parent'), ('int', 'id'),
                   ('const wxString&', 'title'),
                   ('const wxPoint&', 'pos', 'wxDefaultPosition'),
                   ('const wxSize&', 'size', 'wxDefaultSize'),
                   ('long', 'style', 'wxDEFAULT_FRAME_STYLE')]

    def get_code(self, obj):
        return [], [], [], []  # the frame can't be a children

    def get_properties_code(self, obj):
        out = []
        append = out.append
        title = obj.properties.get('title')
        if title:
            append('SetTitle(%s);\n' % self.codegen.quote_str(title))
        icon = obj.properties.get('icon')
        if icon:
            append('wxIcon _icon;\n')
            if icon.startswith('var:'):
                append('_icon.CopyFromBitmap(wxBitmap(%s, '
                       'wxBITMAP_TYPE_ANY));\n' % icon[4:].strip())
            elif icon.startswith('code:'):
                append('_icon.CopyFromBitmap(%s);\n' % icon[5:].strip())
            else:
                append('_icon.CopyFromBitmap(wxBitmap(%s, '
                       'wxBITMAP_TYPE_ANY));\n' %
                       self.codegen.quote_path(icon))
            append('SetIcon(_icon);\n')

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
    extra_headers = ['<wx/mdi.h>']

    constructor = [('wxMDIParentFrame*', 'parent'), ('int', 'id'),
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