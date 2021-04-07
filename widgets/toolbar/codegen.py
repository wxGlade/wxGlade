"""\
Code generator functions for wxToolBar objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016-2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common, compat
import wcodegen
from .tool import *


class PythonCodeGenerator(wcodegen.PythonWidgetCodeWriter):
    def get_properties_code(self, obj):
        out = []

        obj_name = self.format_widget_access(obj)
        if obj.properties["bitmapsize"].is_active():
            w, h = obj.properties["bitmapsize"].get_tuple()
            out.append( '%s.SetToolBitmapSize((%s, %s))\n' % (obj_name, w, h) )
        if obj.properties["margins"].is_active():
            w, h = obj.properties["margins"].get_tuple()
            out.append( '%s.SetMargins((%s, %s))\n' % (obj_name, w, h) )
        if obj.properties["packing"].is_active():
            out.append( '%s.SetToolPacking(%s)\n' % (obj_name, obj.packing) )
        if obj.properties["separation"].is_active():
            out.append( '%s.SetToolSeparation(%s)\n' % (obj_name, obj.separation) )

        return out

    def get_init_code(self, obj):
        out = []
        ids = []

        obj_name = self.format_widget_access(obj)

        bmp1 = self.generate_code_bitmap("empty:16,16")

        for tool in obj.tools:
            if tool.id == '---':  # item is a separator
                out.append( '%s.AddSeparator()\n' % obj_name )
            else:
                id_access = None  # default: access via item.GetId()
                id_declaration, val = self.codegen.generate_code_id(None, tool.id)
                if self.codegen.preview or (not id_declaration and (not val or val == '-1')):
                    wid = self.cn('wxNewId()')
                else:
                    if id_declaration: ids.append( id_declaration )
                    wid = val
                    if val!='wx.ID_ANY': id_access = val

                kinds = ['wxITEM_NORMAL', 'wxITEM_CHECK', 'wxITEM_RADIO']
                try:
                    kind = kinds[int(tool.type)]
                except (IndexError, ValueError):
                    kind = 'wxITEM_NORMAL'
                if tool.bitmap1:
                    bmp1 = self.generate_code_bitmap(tool.bitmap1, required=self.codegen.preview)
                else:
                    bmp1 = self.generate_code_bitmap("empty:16,16")
                bmp2 = self.generate_code_bitmap(tool.bitmap2)
                
                # append and optionally assign the returned item to a temporary variable
                if False: # tool.name:
                    # assign to attribute
                    name = '%s.%s' % (obj_name, item.name)
                    assignment = '%s = '%name
                    if not id_access:
                        id_access = "%s.GetId()"%name
                elif tool.handler and not id_access:
                    # assignment to local variable to bind handler
                    assignment = 'tool = '
                    id_access = 'tool.GetId()'
                else:
                    # no assignment necessary, as no handler defined
                    assignment = ''

                method = "AddLabelTool" if compat.IS_CLASSIC else "AddTool"
                out.append( '%s%s.%s(%s, %s, %s, %s, %s, %s, %s)\n' %
                            (assignment, obj_name, method, wid, self.codegen.quote_str(tool.label),
                             bmp1, bmp2, self.cn(kind),
                             self.codegen.quote_str(tool.short_help), self.codegen.quote_str(tool.long_help)) )
                if tool.handler:
                    handler = tool.handler if "." in tool.handler else "self.%s"%tool.handler
                    out.append( "self.Bind(wx.EVT_TOOL, %s, id=%s)\n"%(handler, id_access) )

        return ids + out

    def get_code(self, obj):
        "function that generates Python code for the menubar of a wxFrame"
        style = obj.properties['style'].get_string_value()
        if style:
            style = ', style=' + self.cn_f(style)

        klass = obj.get_instantiation_class(self.cn, self.cn_class, self.codegen.preview)

        code = ['# Tool Bar\n',
                'self.%s = %s(self, -1%s)\n' % (obj.name, klass, style)
                ] + self.get_init_code(obj) + self.get_properties_code(obj) + [
                'self.SetToolBar(self.%s)\n' % obj.name,
                'self.%s.Realize()\n' % obj.name,
                '# Tool Bar end\n']
        return code, []

    def get_layout_code(self, obj):
        return ['%s.Realize()\n' % self.format_widget_access(obj)]

    def get_event_handlers(self, obj):
        ret = []

        for tool in obj.tools:
            if not tool.handler or tool.handler=="---": continue
            # first item is None -> just generate stub for item.handler, do not bind again
            ret.append( (None, 'EVT_TOOL', tool.handler, 'wxCommandEvent') )

        return ret


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
        style = style and (', wxDefaultPosition, wxDefaultSize, wxTB_HORIZONTAL|' + style) or ''

        klass = obj.get_instantiation_class(self.cn, self.cn_class)

        init = ['%s = new %s(this, -1%s);\n' % (obj.name, klass, style), 'SetToolBar(%s);\n' % obj.name
                ] + self.get_properties_code(obj) + self.get_layout_code(obj)
        ids = self.get_ids_code(obj)
        return init, ids, []

    def get_layout_code(self, obj):
        return ['%sRealize();\n' % self.codegen.format_generic_access(obj)]

    def get_properties_code(self, obj):
        out = []

        obj_name = self.codegen.format_generic_access(obj)

        if obj.properties["bitmapsize"].is_active():
            w, h = obj.properties["bitmapsize"].get_tuple()
            out.append('%sSetToolBitmapSize(wxSize(%s, %s));\n' % (obj_name, w, h))

        if obj.properties["margins"].is_active():
            w, h = obj.properties["margins"].get_tuple()
            out.append('%sSetMargins(wxSize(%s, %s));\n' % (obj_name, w, h))

        if obj.properties["packing"].is_active():
            out.append('%sSetToolPacking(%s);\n' % (obj_name, obj.packing))
        if obj.properties["separation"].is_active():
            out.append('%sSetToolSeparation(%s);\n' % (obj_name, obj.separation))

        need_tmp = False

        for tool in obj.tools:
            if tool.id == '---':  # item is a separator
                out.append('%sAddSeparator();\n' % obj_name)
            else:
                id_access = None
                name, val = self.codegen.generate_code_id(None, tool.id)
                if not name and (not val or val == '-1'):
                    wid = 'wxNewId()'
                else:
                    wid = val
                    if val!='wxID_ANY': id_access = val
                kinds = ['wxITEM_NORMAL', 'wxITEM_CHECK', 'wxITEM_RADIO']
                try:
                    kind = kinds[int(tool.type)]
                except (IndexError, ValueError):
                    kind = 'wxITEM_NORMAL'
                bmp1 = self.generate_code_bitmap(tool.bitmap1)
                bmp2 = self.generate_code_bitmap(tool.bitmap2)

                if False: # tool.name:
                    # assign to attribute
                    self.codegen.classes[obj.parent].sub_objs.append( ('wxMenuItem',item.name) )
                    assignment = '%s = '%tool.name
                    if not id_access:
                        id_access = "%s->GetId()"%tool.name
                elif tool.handler and not id_access:
                    # assignment to local variable to bind handler
                    assignment = 'wxglade_tmp_tool = '
                    id_access = 'wxglade_tmp_tool->GetId()'
                    need_tmp = True
                else:
                    # no assignment necessary, as no handler defined
                    assignment = ''

                out.append('%s%sAddTool(%s, %s, %s, %s, %s, %s, %s);\n' %
                            (assignment, obj_name, wid, self.codegen.quote_str(tool.label),
                             bmp1, bmp2, kind,
                             self.codegen.quote_str(tool.short_help),
                             self.codegen.quote_str(tool.long_help)))

                if tool.handler:
                    handler = tool.handler if "::" in tool.handler else '%s::%s'%(obj.parent.klass, tool.handler)

                    if self.codegen.for_version==(2,8):
                        tmpl = 'Connect(%(id)s, wxEVT_TOOL, %(handler)s);\n'
                    else:
                        tmpl = 'Bind(wxEVT_MENU, &%(handler)s, this, %(id)s);\n'
                    out.append( tmpl % {"id":id_access, "handler":handler} )
        
        if need_tmp:
            out.insert(0, "wxToolBarToolBase *wxglade_tmp_tool;\n" )

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
        ret = []

        for tool in obj.tools:
            if not tool.handler: continue
            # first item is None -> just generate stub for item.handler, do not bind again
            ret.append( (None, 'EVT_TOOL', tool.handler, 'wxCommandEvent') )

        return ret


def initialize():
    klass = 'wxToolBar'
    common.class_names['EditToolBar'] = klass
    common.register('python', klass, PythonCodeGenerator(klass))
    common.register('C++',    klass, CppCodeGenerator(klass))
    common.register('XRC',    klass, xrc_code_generator)
