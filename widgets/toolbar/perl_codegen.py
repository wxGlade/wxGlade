# perl_codegen.py : perl generator functions for wxMenuBar objects
# $Id: perl_codegen.py,v 1.4 2003/07/11 07:30:26 crazyinsomniac Exp $
#
# Copyright (c) 2002-2003 D.H. aka crazyinsomniac on sourceforge.net
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY


import common
from tool import *

# yay
from codegen import ToolsHandler



class PerlCodeGenerator:
    def get_properties_code(self, obj):
        prop = obj.properties
        plgen = common.code_writers['perl']
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
                append('%s->SetToolBitmapSize(%s, %s);\n' % (obj_name, w, h))
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
        prop = obj.properties
        plgen = common.code_writers['perl']
        out = []
        append = out.append
        tools = obj.properties['toolbar']
        ids = []
       
        if obj.is_toplevel:
            obj_name = '$self'
        else:
            obj_name = '$self->{%s}' % obj.name

        for tool in tools:
            if tool.id == '---': # item is a separator
                append('%s->AddSeparator();\n' % obj_name)
            else:
                if not obj.preview and tool.id:
                    tokens = tool.id.split('=')
                    if len(tokens) > 1:
                        id = tokens[0]
                        ids.append(' = '.join(tokens) + '\n')
                    else:
                        id = tool.id
                else: id = 'Wx::NewId()'
                kinds = ['wxITEM_NORMAL', 'wxITEM_CHECK', 'wxITEM_RADIO']
                try:
                    kind = kinds[int(tool.type)]
                except (IndexError, ValueError):
                    kind = 'wxITEM_NORMAL'
                if tool.bitmap1:
                    bmp1 = 'Wx::Bitmap->new(%s, wxBITMAP_TYPE_ANY)' % \
                        plgen.quote_path(tool.bitmap1)
                else:
                    bmp1 = 'wxNullBitmap'
                if tool.bitmap2:
                    bmp2 = 'Wx::Bitmap->new(%s, wxBITMAP_TYPE_ANY)' % \
                        plgen.quote_path(tool.bitmap2)
                else:
                    bmp2 = 'wxNullBitmap'
#                append('%s->AddLabelTool(%s, %s, %s, %s, %s, %s, %s);\n' %
                append('%s->AddTool(%s, %s, %s, %s, %s, %s, %s);\n' %
                       (obj_name, id, plgen.quote_str(tool.label),
                        bmp1, bmp2, kind, plgen.quote_str(tool.short_help),
                        plgen.quote_str(tool.long_help)))
        
        return ids + out


    def get_code(self, obj):
        """\
        function that generates Perl code for the menubar of a wxFrame.
        """
        plgen = common.code_writers['perl']
        style = obj.properties.get('style')
        if style:
            style = 'wxTB_HORIZONTAL|' + style
        else:
            style = ''

        init = [
            '\n# Tool Bar\n',
            '$self->{%s} = %s->new($self, -1, wxDefaultPosition, \
wxDefaultSize, %s);\n' % (obj.name, obj.klass.replace('wx','Wx::',1), style),
                 '$self->SetToolBar($self->{%s});\n' % obj.name 
            ]
        init.extend(self.get_init_code(obj))
        init.append('# Tool Bar end\n')
        return init, self.get_properties_code(obj), []

# end of class PerlCodeGenerator

def initialize():
    common.class_names['EditToolBar'] = 'wxToolBar'
    common.toplevels['EditToolBar'] = 1

    plgen = common.code_writers.get('perl')

    if plgen:
        plgen.add_widget_handler('wxToolBar', PerlCodeGenerator())
        plgen.add_property_handler('tools', ToolsHandler)
