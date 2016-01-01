"""\
Lisp generator functions for wxToolBar objects

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


import common
import wcodegen
from tool import *

from codegen import ToolsHandler


class LispCodeGenerator(wcodegen.LispWidgetCodeWriter):
    def get_properties_code(self, obj):
        prop = obj.properties
        out = []
        append = out.append

        obj_name = '(slot-%s obj)' % obj.name

        bitmapsize = prop.get('bitmapsize')
        if bitmapsize:
            try:
                w, h = [int(i) for i in bitmapsize.split(',')]
                append('(wxToolBar_SetToolBitmapSize %s %s %s)\n' % \
                       (obj_name, w, h))
            except:
                pass

        margins = prop.get('margins')
        if margins:
            try:
                w, h = [int(i) for i in margins.split(',')]
                append('(wxToolBar_SetMargins %s %s %s)\n'
                       % (obj_name, w, h))
            except:
                pass

        packing = prop.get('packing')
        if packing:
            append('(wxToolBar_SetToolPacking %s %s)\n' % (obj_name, packing))

        separation = prop.get('separation')
        if separation:
            append('(wxToolBar_SetToolSeparation %s %s)\n' % (obj_name, separation))
        append('(wxToolBar_Realize %s)\n' % obj_name)

        return out

    def get_init_code(self, obj):
        out = []
        append = out.append
        tools = obj.properties['toolbar']
        ids = []

        obj_name = self.format_widget_access(obj)

        for tool in tools:
            if tool.id == '---':  # item is a separator
                append('(wxToolBar_AddSeparator %s)\n' % obj_name)
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
                bmp1 = self.generate_code_bitmap(tool.bitmap1, obj.preview)
                bmp2 = self.generate_code_bitmap(tool.bitmap2, obj.preview)
#                append('%s->AddLabelTool(%s, %s, %s, %s, %s, %s, %s);\n' %
                append('(wxToolBar_AddTool %s %s %s %s %s %s %s %s)\n' %
                       (obj_name, wid, self.codegen.quote_str(tool.label),
                        bmp1, bmp2, kind,
                        self.codegen.quote_str(tool.short_help),
                        self.codegen.quote_str(tool.long_help)))

        return ids + out

    def get_code(self, obj):
        """\
        function that generates Lisp code for the toolbar of a wxFrame.
        """
        prop = obj.properties

        style = prop.get("style")
        if not style:
            style = 'wxTB_HORIZONTAL'
        else:
            style += "|wxTB_HORIZONTAL"
            style = self.cn_f(style)

        parent = self.format_widget_access(obj.parent)

        init = [
            '\n\t;;; Tool Bar\n',
            '(setf (slot-%s obj) (wxToolBar_Create %s -1 -1 -1 -1 -1 %s))\n' % (obj.name, parent, style),
                 '(wxFrame_SetToolBar (slot-top-window obj) (slot-%s obj))\n' % obj.name
            ]
        init.extend(self.get_init_code(obj))
        init.append(';;; Tool Bar end\n')
        return init, self.get_properties_code(obj), []

# end of class LispCodeGenerator


def initialize():
    klass = 'wxToolBar'
    common.class_names['EditToolBar'] = klass
    common.toplevels['EditToolBar'] = 1
    common.register('lisp', klass, LispCodeGenerator(klass),
                    'tools', ToolsHandler)
