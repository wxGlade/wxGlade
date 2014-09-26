"""\
Perl generator functions for wxToolBar objects

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


import common
import wcodegen
from tool import *

from codegen import ToolsHandler


class PerlCodeGenerator(wcodegen.PerlWidgetCodeWriter):

    def get_properties_code(self, obj):
        prop = obj.properties
        out = []
        append = out.append
        
        if obj.is_toplevel:
            obj_name = '$self'
        else:
            obj_name = '$self->{%s}' % obj.name
        
        bitmapsize = prop.get('bitmapsize')
        if bitmapsize:
            try:
                w, h = [int(i) for i in bitmapsize.split(',')]
                append('%s->SetToolBitmapSize(wxSIZE(%s, %s));\n' % \
                       (obj_name, w, h))
            except:
                pass

        margins = prop.get('margins')
        if margins:
            try:
                w, h = [int(i) for i in margins.split(',')]
                append('%s->SetMargins(%s, %s);\n' % (obj_name, w, h))
            except:
                pass

        packing = prop.get('packing')
        if packing:
            append('%s->SetToolPacking(%s);\n' % (obj_name, packing))

        separation = prop.get('separation')
        if separation:
            append('%s->SetToolSeparation(%s);\n' % (obj_name, separation))
        append('%s->Realize();\n' % obj_name)

        return out

    def get_init_code(self, obj):
        out = []
        append = out.append
        tools = obj.properties['toolbar']
        ids = []
       
        if obj.is_toplevel:
            obj_name = '$self'
        else:
            obj_name = '$self->{%s}' % obj.name

        def _get_bitmap(bitmap):
            if not bitmap:
                return 'wxNullBitmap'
            elif bitmap.startswith('var:'):
                # this is a variable holding bitmap path
                var = bitmap[4:].strip()
                if var[0] != "$":
                    var = "$" + var
                return 'Wx::Bitmap->new(%s, wxBITMAP_TYPE_ANY)' % var
            elif bitmap.startswith('code:'):
                return '(%s)' % bitmap[5:].strip()
            else:
                return 'Wx::Bitmap->new(%s, wxBITMAP_TYPE_ANY)' % \
                       self.codegen.quote_path(bitmap)

        for tool in tools:
            if tool.id == '---': # item is a separator
                append('%s->AddSeparator();\n' % obj_name)
            else:
                name, val = self.codegen.generate_code_id(None, tool.id)
                if not name and (not val or val == '-1'):
                    id = 'Wx::NewId()'
                else:
                    if name: ids.append(name)
                    id = val
                kinds = ['wxITEM_NORMAL', 'wxITEM_CHECK', 'wxITEM_RADIO']
                try:
                    kind = kinds[int(tool.type)]
                except (IndexError, ValueError):
                    kind = 'wxITEM_NORMAL'
                bmp1 = _get_bitmap(tool.bitmap1)
                bmp2 = _get_bitmap(tool.bitmap2)
                append('%s->AddTool(%s, %s, %s, %s, %s, %s, %s);\n' %
                       (obj_name, id, self.codegen.quote_str(tool.label),
                        bmp1, bmp2, kind,
                        self.codegen.quote_str(tool.short_help),
                        self.codegen.quote_str(tool.long_help)))
        
        return ids + out


    def get_code(self, obj):
        """\
        function that generates Perl code for the toolbar of a wxFrame.
        """
        style = obj.properties.get('style')
        if style:
            style = 'wxTB_HORIZONTAL|' + style
        else:
            style = ''

        klass = obj.base
        if klass != obj.klass:
            klass = obj.klass
        else:
            klass = klass.replace('wx', 'Wx::', 1)

        init = [
            '\n# Tool Bar\n',
            '$self->{%s} = %s->new($self, -1, wxDefaultPosition, \
wxDefaultSize, %s);\n' % (obj.name, klass, style),
                 '$self->SetToolBar($self->{%s});\n' % obj.name 
            ]
        init.extend(self.get_init_code(obj))
        init.append('# Tool Bar end\n')
        return init, self.get_properties_code(obj), []

# end of class PerlCodeGenerator


def initialize():
    klass = 'wxToolBar'
    common.class_names['EditToolBar'] = klass
    common.toplevels['EditToolBar'] = 1
    common.register('perl', klass, PerlCodeGenerator(klass),
                    'tools', ToolsHandler)
