"""\
Perl generator functions for wxToolBar objects

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


import common
import wcodegen
from .tool import *



class PerlCodeGenerator(wcodegen.PerlWidgetCodeWriter):

    def get_properties_code(self, obj):
        prop = obj.properties
        out = []
        append = out.append

        obj_name = self.format_widget_access(obj)

        if obj.properties["bitmapsize"].is_active():
            w, h = obj.properties["bitmapsize"].get_tuple()
            append( '%s->SetToolBitmapSize(wxSIZE(%s, %s));\n' % (obj_name, w, h) )

        if obj.properties["margins"].is_active():
            w, h = obj.properties["margins"].get_tuple()
            append( '%s->SetMargins(%s, %s);\n' % (obj_name, w, h) )

        if obj.properties["packing"].is_active():
            append( '%s->SetToolPacking(%s);\n' % (obj_name, obj.packing) )

        if obj.properties["separation"].is_active():
            append( '%s->SetToolSeparation(%s);\n' % (obj_name, obj.separation) )

        return out

    def get_init_code(self, obj):
        out = []
        append = out.append
        ids = []

        obj_name = self.codegen.format_generic_access(obj)

        for tool in obj.tools:
            if tool.id == '---':  # item is a separator
                append( '%s->AddSeparator();\n' % obj_name )
            else:
                name, val = self.codegen.generate_code_id(None, tool.id)
                if not name and (not val or val == '-1'):
                    wid = 'Wx::NewId()'
                else:
                    if name:
                        ids.append( name )
                    wid = val
                kinds = ['wxITEM_NORMAL', 'wxITEM_CHECK', 'wxITEM_RADIO']
                try:
                    kind = kinds[int(tool.type)]
                except (IndexError, ValueError):
                    kind = 'wxITEM_NORMAL'
                bmp1 = self.generate_code_bitmap(tool.bitmap1)
                bmp2 = self.generate_code_bitmap(tool.bitmap2)
                append( '%s->AddTool(%s, %s, %s, %s, %s, %s, %s);\n' %
                        (obj_name, wid, self.codegen.quote_str(tool.label),
                         bmp1, bmp2, kind,
                         self.codegen.quote_str(tool.short_help),
                         self.codegen.quote_str(tool.long_help)) )

        return ids + out

    def get_code(self, obj):
        "function that generates Perl code for the toolbar of a wxFrame"
        init = []
        style = obj.properties['style'].get_string_value()
        if style:
            style = 'wxTB_HORIZONTAL|' + style
            extra = ', wxDefaultPosition, wxDefaultSize, %s' % style
        else:
            extra = ''

        klass = obj.get_instantiation_class(self.cn, self.cn_class)

        init = ['\n', '# Tool Bar\n',
                '$self->{%s} = %s->new($self, -1%s);\n' % (obj.name, klass, extra)
                ] + self.get_init_code(obj) + self.get_properties_code(obj) + [
                '$self->SetToolBar($self->{%s});\n' % obj.name,
                '%s->Realize();\n' % self.format_widget_access(obj),
                '# Tool Bar end\n' ]
        return init, []

    def get_layout_code(self, obj):
        return ['%s->Realize();\n' % self.format_widget_access(obj)]


def initialize():
    klass = 'wxToolBar'
    common.class_names['EditToolBar'] = klass
    common.register('perl', klass, PerlCodeGenerator(klass))
