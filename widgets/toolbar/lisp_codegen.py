"""\
Lisp generator functions for wxToolBar objects

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


import common
import wcodegen
from .tool import *

#from .codegen import ToolsHandler


class LispCodeGenerator(wcodegen.LispWidgetCodeWriter):
    def get_properties_code(self, obj):
        prop = obj.properties
        out = []
        append = out.append

        obj_name = '(slot-%s obj)' % self.codegen._format_name(obj.name)

        if obj.properties["bitmapsize"].is_active():
            w, h = obj.properties["bitmapsize"].get_tuple()
            append( '(wxToolBar_SetToolBitmapSize %s %s %s)\n' % (obj_name, w, h) )

        if obj.properties["margins"].is_active():
            w, h = obj.properties["margins"].get_tuple()
            append( '(wxToolBar_SetMargins %s %s %s)\n' % (obj_name, w, h) )

        if obj.properties["packing"].is_active():
            append( '(wxToolBar_SetToolPacking %s %s)\n' % (obj_name, obj.packing) )

        if obj.properties["separation"].is_active():
            append( '(wxToolBar_SetToolSeparation %s %s)\n' % (obj_name, obj.separation) )

        return out

    def get_init_code(self, obj):
        out = []
        append = out.append
        ids = []

        obj_name = self.format_widget_access(obj)

        for tool in obj.tools:
            if tool.id == '---':  # item is a separator
                append( '(wxToolBar_AddSeparator %s)\n' % obj_name )
            else:
                name, val = self.codegen.generate_code_id(None, tool.id)
                if not name and (not val or val == '-1'):
                    wid = 'Wx::NewId()'
                else:
                    if name:
                        ids.append(name)
                    wid = val
                kinds = ['wxITEM_NORMAL', 'wxITEM_CHECK', 'wxITEM_RADIO']
                try:
                    kind = kinds[int(tool.type)]
                except (IndexError, ValueError):
                    kind = 'wxITEM_NORMAL'
                bmp1 = self.generate_code_bitmap(tool.bitmap1)
                bmp2 = self.generate_code_bitmap(tool.bitmap2)
#                append('%s->AddLabelTool(%s, %s, %s, %s, %s, %s, %s);\n' %
                append( '(wxToolBar_AddTool %s %s %s %s %s %s %s %s)\n' %
                        (obj_name, wid, self.codegen.quote_str(tool.label),
                         bmp1, bmp2, kind,
                         self.codegen.quote_str(tool.short_help),
                         self.codegen.quote_str(tool.long_help)) )

        return ids + out

    def get_code(self, obj):
        "function that generates Lisp code for the toolbar of a wxFrame"
        style = obj.properties['style'].get_string_value()
        if not style:
            style = 'wxTB_HORIZONTAL'
        else:
            style += "|wxTB_HORIZONTAL"
            style = self.cn_f(style)

        parent = self.format_widget_access(obj.parent_window)
        obj_name = self.codegen._format_name(obj.name)
        init = [ ';;; Tool Bar\n',
                 '(setf (slot-%s obj) (wxToolBar_Create %s -1 -1 -1 -1 -1 %s))\n' % (obj_name, parent, style),
                ] + self.get_init_code(obj) + self.get_properties_code(obj) + [
                 '(wxFrame_SetToolBar (slot-top-window obj) (slot-%s obj))\n' % obj_name,
                 '(wxToolBar_Realize %s)\n' % self.format_widget_access(obj),
                 ';;; Tool Bar end\n']
        return init, []

    def get_layout_code(self, obj):
        obj_name = '(slot-%s obj)' % self.codegen._format_name(obj.name)
        return ['(wxToolBar_Realize %s)\n' % obj_name]



def initialize():
    klass = 'wxToolBar'
    common.class_names['EditToolBar'] = klass
    common.register('lisp', klass, LispCodeGenerator(klass) )#, 'tools', ToolsHandler)
