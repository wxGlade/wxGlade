"""\
Code generator functions for wxToolBar objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016-2017 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common, compat
import wcodegen
from .tool import *


class PythonCodeGenerator(wcodegen.PythonWidgetCodeWriter):
    def get_properties_code(self, obj):
        out = []
        append = out.append

        obj_name = self.format_widget_access(obj)
        if obj.properties["bitmapsize"].is_active():
            w, h = obj.properties["bitmapsize"].get_tuple()
            append( '%s.SetToolBitmapSize((%s, %s))\n' % (obj_name, w, h) )
        if obj.properties["margins"].is_active():
            w, h = obj.properties["margins"].get_tuple()
            append( '%s.SetMargins((%s, %s))\n' % (obj_name, w, h) )
        if obj.properties["packing"].is_active():
            append( '%s.SetToolPacking(%s)\n' % (obj_name, obj.packing) )
        if obj.properties["separation"].is_active():
            append( '%s.SetToolSeparation(%s)\n' % (obj_name, obj.separation) )
        append( '%s.Realize()\n' % obj_name )

        return out

    def get_init_code(self, obj):
        out = []
        append = out.append
        ids = []

        obj_name = self.format_widget_access(obj)

        for tool in obj.tools:
            if tool.id == '---':  # item is a separator
                append( '%s.AddSeparator()\n' % obj_name )
            else:
                name, val = self.codegen.generate_code_id(None, tool.id)
                if self.codegen.preview or (not name and (not val or val == '-1')):
                    wid = self.cn('wxNewId()')
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
                method = "AddLabelTool" if compat.IS_CLASSIC else "AddTool"
                append( '%s.%s(%s, %s, %s, %s, %s, %s, %s)\n' %
                        (obj_name, method, wid, self.codegen.quote_str(tool.label),
                         bmp1, bmp2, self.cn(kind),
                         self.codegen.quote_str(tool.short_help), self.codegen.quote_str(tool.long_help)) )

        return ids + out

    def get_code(self, obj):
        "function that generates Python code for the menubar of a wxFrame"
        #style = obj.properties.get('style')
        style = obj.properties['style'].get_string_value()
        if style:
            style = ', style=' + self.cn_f( 'wxTB_HORIZONTAL|' + style )
        #else:
            #style = ''
        klass = obj.klass
        if klass == obj.base:
            klass = self.cn(klass)
        init = ['\n', '# Tool Bar\n', 'self.%s = %s(self, -1%s)\n' %
                                                      (obj.name, klass, style), 'self.SetToolBar(self.%s)\n' % obj.name]
        init.extend( self.get_init_code(obj) )
        init.append( '# Tool Bar end\n' )
        return init, self.get_properties_code(obj), []

    def get_event_handlers(self, obj):
        out = []

        def do_get(tool):
            ret = []
            name, val = self.codegen.generate_code_id(None, tool.id)
            if not val:
                val = '-1'  # but this is wrong anyway...
            if tool.handler:
                ret.append((val, 'EVT_TOOL', tool.handler, 'wxCommandEvent'))
            return ret

        for tool in obj.tools:
            out.extend(do_get(tool))
        return out


def xrc_code_generator(obj):
    "function that generates XRC code for a toolbar"
    from xml.sax.saxutils import escape, quoteattr
    xrcgen = common.code_writers['XRC']

    class ToolBarXrcObject(xrcgen.DefaultXrcObject):
        def append_item(self, item, output, tabs):
            if item.id == '---':  # item is a separator
                output.append('    '*tabs + '<object class="separator"/>\n')
            else:
                if item.id:
                    name = item.id.split('=', 1)[0]
                    if name:
                        output.append('    '*tabs + '<object class="tool" name=%s>\n' % quoteattr(name))
                    else:
                        output.append('    '*tabs + '<object class="tool">\n')
                else:
                    output.append('    '*tabs + '<object class="tool">\n')
                # why XRC seems to ignore label??
                # this has been fixed on CVS, so add it (it shouldn't hurt...)
                if item.label:
                    output.append('    '*(tabs+1) + '<label>%s</label>\n' % escape(item.label))
                if item.short_help:
                    output.append('    '*(tabs+1) + '<tooltip>%s</tooltip>\n' % escape(item.short_help))
                if item.long_help:
                    output.append('    '*(tabs+1) + '<longhelp>%s</longhelp>\n' % escape(item.long_help))
                if item.bitmap1:
                    prop = self._format_bitmap_property( 'bitmap', item.bitmap1, tabs+1 )
                    if prop: output.append(prop)
                if item.bitmap2:
                    prop = self._format_bitmap_property( 'bitmap2', item.bitmap2, tabs+1 )
                    if prop: output.append(prop)
                try:
                    # again, it seems that XRC doesn't support "radio" tools
                    if int(item.type) == 1:
                        output.append('    '*(tabs+1) + '<toggle>1</toggle>\n')
                    # the above has been fixed on CVS, so add a radio if it's there
                    elif int(item.type) == 2:
                        output.append('    '*(tabs+1) + '<radio>1</radio>\n')
                except ValueError:
                    pass
                output.append('    '*tabs + '</object>\n')
        def write(self, output, tabs):
            tools = self.widget.tools
            output.append('    '*tabs + '<object class="wxToolBar" name=%s>\n' % quoteattr(self.name))
            
            for prop_name in 'bitmapsize', 'margins':
                prop = self.widget.properties.get(prop_name)
                if prop.is_active():
                    try:
                        w, h = prop.get_tuple()
                        output.append('    ' * (tabs+1) + '<%s>%s, %s</%s>\n' % (prop_name, w, h, prop_name))
                    except:
                        pass
            for prop_name in 'packing', 'separation':
                prop = self.widget.properties.get(prop_name)
                if prop.is_active():
                    output.append('    ' * (tabs+1) + '<%s>%s</%s>\n' % (prop_name, escape(prop), prop_name))
            style = self.widget.properties['style'].get_string_value()
            if style:
                style = self.cn_f(style)
                style = style.split('|')
                style.append('wxTB_HORIZONTAL')
                style.sort()
                output.append('    '*(tabs+1) + '<style>%s</style>\n' % escape('|'.join(style)))
            for t in tools:
                self.append_item(t, output, tabs+1)
            output.append('    '*tabs + '</object>\n')
    # end of class ToolBarXrcObject

    return ToolBarXrcObject(obj)


class CppCodeGenerator(wcodegen.CppWidgetCodeWriter):
    constructor = [('wxWindow*', 'parent'), ('wxWindowID', 'id'),
                   ('const wxPoint&', 'pos', 'wxDefaultPosition'),
                   ('const wxSize&', 'size', 'wxDefaultSize'),
                   ('long', 'style', 'wxTB_HORIZONTAL|wxNO_BORDER')]

    def get_code(self, obj):
        "generates C++ code for the toolbar of a wxFrame."
        style = obj.properties['style'].get_string_value()
        if style:
            style = ', wxDefaultPosition, wxDefaultSize, wxTB_HORIZONTAL|' + style
        else:
            style = ''
        init = ['%s = new %s(this, -1%s);\n' % (obj.name, obj.klass, style), 'SetToolBar(%s);\n' % obj.name]
        init.extend(self.get_properties_code(obj))
        ids = self.get_ids_code(obj)
        return init, ids, [], []

    def get_properties_code(self, obj):
        out = []
        append = out.append
        prop = obj.properties

        obj_name = self.codegen.format_generic_access(obj)

        if obj.properties["bitmapsize"].is_active():
            w, h = obj.properties["bitmapsize"].get_tuple()
            append('%sSetToolBitmapSize(wxSize(%s, %s));\n' % (obj_name, w, h))

        if obj.properties["margins"].is_active():
            w, h = obj.properties["margins"].get_tuple()
            append('%sSetMargins(wxSize(%s, %s));\n' % (obj_name, w, h))

        if obj.properties["packing"].is_active():
            append('%sSetToolPacking(%s);\n' % (obj_name, obj.packing))
        if obj.properties["separation"].is_active():
            append('%sSetToolSeparation(%s);\n' % (obj_name, obj.separation))

        for tool in obj.tools:
            if tool.id == '---':  # item is a separator
                append('%sAddSeparator();\n' % obj_name)
            else:
                name, val = self.codegen.generate_code_id(None, tool.id)
                if not name and (not val or val == '-1'):
                    wid = 'wxNewId()'
                else:
                    wid = val
                kinds = ['wxITEM_NORMAL', 'wxITEM_CHECK', 'wxITEM_RADIO']
                try:
                    kind = kinds[int(tool.type)]
                except (IndexError, ValueError):
                    kind = 'wxITEM_NORMAL'
                bmp1 = self.generate_code_bitmap(tool.bitmap1)
                bmp2 = self.generate_code_bitmap(tool.bitmap2)
                append('%sAddTool(%s, %s, %s, %s, %s, %s, %s);\n' %
                       (obj_name, wid, self.codegen.quote_str(tool.label),
                        bmp1, bmp2, kind,
                        self.codegen.quote_str(tool.short_help),
                        self.codegen.quote_str(tool.long_help)))

        append('%sRealize();\n' % obj_name)

        return out

    def get_ids_code(self, obj):
        ids = []

        for item in obj.tools:
            if item.id == '---':  # item is a separator
                pass  # do nothing
            else:
                name, val = self.codegen.generate_code_id(None, item.id)
                if name.find('=') != -1:
                    ids.append(name)
        return ids

    def get_event_handlers(self, obj):
        out = []

        def do_get(tool):
            ret = []
            name, val = self.codegen.generate_code_id(None, tool.id)
            if not val: val = '-1'  # but this is wrong anyway...
            if tool.handler:
                ret.append((val, 'EVT_TOOL', tool.handler, 'wxCommandEvent'))
            return ret

        for tool in obj.tools:
            out.extend(do_get(tool))
        return out



def initialize():
    klass = 'wxToolBar'
    common.class_names['EditToolBar'] = klass
    common.toplevels['EditToolBar'] = 1
    common.register('python', klass, PythonCodeGenerator(klass), 'tools')
    common.register('C++',    klass, CppCodeGenerator(klass),    'tools')
    common.register('XRC',    klass, xrc_code_generator,         'tools')
