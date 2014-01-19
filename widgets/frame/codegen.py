"""\
Code generator functions for wxFrame objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen
from MenuTree import *


class PythonStatusBarGenerator(wcodegen.PythonWidgetCodeWriter):
    tmpl = '%(name)s = self.CreateStatusBar(%(labels_len)s%(style)s)\n'

    def _prepare_tmpl_content(self, obj):
        wcodegen.PythonWidgetCodeWriter._prepare_tmpl_content(self, obj)

        labels, widths = obj.properties['statusbar']
        self.tmpl_dict['labels'] = ', '.join(
            [self.codegen.quote_str(lb) for lb in labels])
        self.tmpl_dict['labels_len'] = len(labels)
        self.tmpl_dict['widths'] = repr(widths)
        self.tmpl_dict['widths_len'] = len(widths)
        append = self.tmpl_props.append

        append('%(name)s.SetStatusWidths(%(widths)s)\n')
        append('\n')

        append('%(comment)s statusbar fields\n')
        append('%(obj_name)s_fields = [%(labels)s]\n')
        append('for i in range(len(%(obj_name)s_fields)):\n')
        append('%(tab)s%(name)s.SetStatusText(%(obj_name)s_fields[i], i)\n')

# end of class PythonStatusBarGenerator


class PythonFrameCodeGenerator(wcodegen.PythonWidgetCodeWriter):
    def get_code(self, obj):
        return [], [], []

    def get_properties_code(self, obj):
        pygen = common.code_writers['python']
        cn = self.cn
        out = []
        title = obj.properties.get('title')
        if title:
            out.append('self.SetTitle(%s)\n' % pygen.quote_str(title))
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
                    out.append(('_icon.CopyFromBitmap(%s)\n') %
                               icon[5:].strip())
                    out.append('self.SetIcon(_icon)\n')
            else:
                if obj.preview:
                    import misc
                    icon = misc.get_relative_path(icon, True)
                out.append('_icon = ' + cn('wxEmptyIcon') + '()\n')
                out.append(('_icon.CopyFromBitmap(' + cn('wxBitmap') +
                            '(%s, ' + cn('wxBITMAP_TYPE_ANY') + '))\n') %
                           pygen.quote_path(icon))
                out.append('self.SetIcon(_icon)\n')

        out.extend(pygen.generate_common_properties(obj))
        return out

    def get_layout_code(self, obj):
        ret = ['self.Layout()\n']
        try:
            if int(obj.properties['centered']):
                ret.append('self.Centre()\n')
        except (KeyError, ValueError):
            pass
        if obj.properties.get('size', '').strip() and \
           self.codegen.for_version < (2, 8):
            ret.append(self.codegen.generate_code_size(obj))
        return ret

# end of class PythonFrameCodeGenerator


# property handlers for code generation

class StatusFieldsHandler:
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


class CppStatusBarGenerator(wcodegen.CppWidgetCodeWriter):

    tmpl = '%(name)s = CreateStatusBar(%(labels_len)s%(style)s);\n'
    prefix_style = False

    def _prepare_tmpl_content(self, obj):
        wcodegen.CppWidgetCodeWriter._prepare_tmpl_content(self, obj)

        labels, widths = obj.properties['statusbar']
        self.tmpl_dict['labels_len'] = len(labels)
        self.tmpl_dict['widths'] = ', '.join(map(str, widths))
        self.tmpl_dict['widths_len'] = len(widths)
        append = self.tmpl_props.append

        append('int %(name)s_widths[] = { %(widths)s };\n')
        append('%(name)s->SetStatusWidths(%(widths_len)s, '
               '%(name)s_widths);\n')
        append('\n')

        append('%(comment)s statusbar fields\n')
        append('const wxString %(name)s_fields[] = {\n')
        for lb in labels:
            append('%%(tab)s%s,\n' % self.codegen.quote_str(lb))
        append('};\n')

        append('for(int i = 0; i < %(name)s->GetFieldsCount(); ++i) {\n')
        append('%(tab)s%(name)s->SetStatusText(%(name)s_fields[i], i);\n')
        append('}\n')

# end of class CppStatusBarGenerator


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
        if obj.properties.get('size', '').strip() and \
           self.codegen.for_version < (2, 8):
            ret.append(self.codegen.generate_code_size(obj))
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
    cn = common.class_names
    cn['EditFrame'] = 'wxFrame'
    cn['EditMDIChildFrame'] = 'wxMDIChildFrame'
    cn['EditStatusBar'] = 'wxStatusBar'
    common.toplevels['EditFrame'] = 1
    common.toplevels['EditMDIChildFrame'] = 1

    pygen = common.code_writers.get('python')
    if pygen:
        awh = pygen.add_widget_handler
        awh('wxFrame', PythonFrameCodeGenerator())
        awh('wxMDIChildFrame', PythonFrameCodeGenerator())
        awh('wxStatusBar', PythonStatusBarGenerator())

        aph = pygen.add_property_handler
        aph('statusbar', pygen.DummyPropertyHandler)
        aph('fields', StatusFieldsHandler)
        aph('menubar', pygen.DummyPropertyHandler)

    xrcgen = common.code_writers.get('XRC')
    if xrcgen:
        awh = xrcgen.add_widget_handler
        awh('wxFrame', xrc_frame_code_generator)
        awh('wxMDIChildFrame', xrcgen.NotImplementedXrcObject)
        awh('wxStatusBar', xrc_statusbar_code_generator)

        xrcgen.add_property_handler('fields', StatusFieldsHandler)

    cppgen = common.code_writers.get('C++')
    if cppgen:
        awh = cppgen.add_widget_handler
        awh('wxFrame', CppFrameCodeGenerator())
        awh('wxMDIChildFrame', CppMDIChildFrameCodeGenerator())
        awh('wxStatusBar', CppStatusBarGenerator())

        aph = cppgen.add_property_handler
        aph('fields', StatusFieldsHandler)
        aph('menubar', cppgen.DummyPropertyHandler)
        aph('statusbar', cppgen.DummyPropertyHandler)
