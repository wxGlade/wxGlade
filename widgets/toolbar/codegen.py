# codegen.py: code generator functions for wxMenuBar objects
#
# Copyright (c) 2002-2003 Alberto Griggio <albgrig@tiscalinet.it>
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

import common
from tool import *

class PythonCodeGenerator:
    def get_properties_code(self, obj):
        return []
        
    def get_init_code(self, obj):
        prop = obj.properties
        pygen = common.code_writers['python']
        out = []
        append = out.append
        tools = obj.properties['toolbar']
        ids = []
       
        if obj.is_toplevel: obj_name = 'self'
        else: obj_name = 'self.' + obj.name

        bitmapsize = obj.properties.get('bitmapsize')
        if bitmapsize:
            try:
                w, h = [int(i) for i in bitmapsize.split(',')]
                append('%s.SetToolBitmapSize((%s, %s))\n' % (obj_name, w, h))
            except:
                pass

        margins = obj.properties.get('margins')
        if margins:
            try:
                w, h = [int(i) for i in margins.split(',')]
                append('%s.SetMargins((%s, %s))\n' % (obj_name, w, h))
            except:
                pass

        for tool in tools:
            if tool.id == '---': # item is a separator
                append('%s.AddSeparator()\n' % obj_name)
            else:
                if tool.id:
                    tokens = tool.id.split('=')
                    if len(tokens) > 1:
                        id = tokens[0]
                        ids.append(' = '.join(tokens) + '\n')
                    else:
                        id = tool.id
                else: id = 'wxNewId()'
                kinds = ['wxITEM_NORMAL', 'wxITEM_CHECK', 'wxITEM_RADIO']
                try:
                    kind = kinds[int(tool.type)]
                except (IndexError, ValueError):
                    kind = 'wxITEM_NORMAL'
                if tool.bitmap1:
                    bmp1 = 'wxBitmap("%s", wxBITMAP_TYPE_ANY)' % \
                           tool.bitmap1.replace('"', r'\"')
                else:
                    bmp1 = 'wxNullBitmap'
                if tool.bitmap2:
                    bmp2 = 'wxBitmap("%s", wxBITMAP_TYPE_ANY)' % \
                           tool.bitmap2.replace('"', r'\"')
                else:
                    bmp2 = 'wxNullBitmap'
                append('%s.AddLabelTool(%s, %s, %s, %s, %s, %s, %s)\n' %
                       (obj_name, id, pygen.quote_str(tool.label),
                        bmp1, bmp2, kind, pygen.quote_str(tool.short_help),
                        pygen.quote_str(tool.long_help)))
        #print 'menus = %s' % menus
        append('%s.Realize()\n' % obj_name)
        
        return ids + out

    def get_code(self, obj):
        """\
        function that generates Python code for the menubar of a wxFrame.
        """
        pygen = common.code_writers['python']
        style = obj.properties.get('style')
        if style:
            style = ', style=wxTB_HORIZONTAL|' + style
        else:
            style = ''
        init = [ '\n', '# Tool Bar\n', 'self.%s = %s(self, -1%s)\n' %
                 (obj.name, obj.klass, style),
                 'self.SetToolBar(self.%s)\n' % obj.name ]
        init.extend(self.get_init_code(obj))
        init.append('# Tool Bar end\n')
        return init, [], []

# end of class PythonCodeGenerator


class ToolsHandler:
    """Handler for tools of a toolbar"""
    item_attrs = ('label', 'id', 'short_help', 'type', 'long_help',
                  'bitmap1', 'bitmap2')
    def __init__(self):
        self.tools = []
        self.curr_tool = None
        self.attr_val = []

    def start_elem(self, name, attrs):
        if name == 'tool':
            self.curr_tool = Tool()

    def end_elem(self, name, code_obj):
        if name == 'tools':
            code_obj.properties['toolbar'] = self.tools
            return True
        if name == 'tool' and self.curr_tool:
            self.tools.append(self.curr_tool)
        elif name in self.item_attrs:
            setattr(self.curr_tool, name, "".join(self.attr_val))
            self.attr_val = []

    def char_data(self, data):
        self.attr_val.append(data)

# end of class MenuHandler


def xrc_code_generator(obj):
    """\
    function that generates XRC code for a toolbar
    """
    from xml.sax.saxutils import escape, quoteattr
    xrcgen = common.code_writers['XRC']
    
    class ToolBarXrcObject(xrcgen.DefaultXrcObject):
        def append_item(self, item, outfile, tabs):
            write = outfile.write
            if item.id == '---': # item is a separator
                write('    '*tabs + '<object class="separator"/>\n')
            else:
                if item.id:
                    write('    '*tabs + '<object class="tool" ' \
                          'name=%s>\n' % quoteattr(item.id))
                else:
                    write('    '*tabs + '<object class="tool">\n')
                # why XRC seems to ignore label??
                if item.short_help:
                    write('    '*(tabs+1) + '<tooltip>%s</tooltip>\n' % \
                          escape(item.short_help))
                if item.long_help:
                    write('    '*(tabs+1) + '<longhelp>%s</longhelp>\n' % \
                          escape(item.long_help))
                if item.bitmap1:
                    write('    '*(tabs+1) + '<bitmap>%s</bitmap>\n' % \
                          escape(item.bitmap1))
                if item.bitmap2:
                    write('    '*(tabs+1) + '<bitmap2>%s</bitmap2>\n' % \
                          escape(item.bitmap2))
                try:
                    # again, it seems that XRC doesn't support "radio" tools
                    if int(item.type) == 1:
                        write('    '*(tabs+1) + '<toggle>1</toggle>\n')
                except ValueError:
                    pass
                write('    '*tabs + '</object>\n')
        
        def write(self, outfile, tabs):
            tools = self.code_obj.properties['toolbar']
            write = outfile.write
            write('    '*tabs + '<object class="wxToolBar" name=%s>\n' % \
                  quoteattr(self.name))
            bitmapsize = self.code_obj.properties.get('bitmapsize')
            if bitmapsize:
                try:
                    w, h = [int(i) for i in bitmapsize.split(',')]
                    write('    ' * (tabs+1) + '<bitmapsize>%s, %s'
                          '</bitmapsize>\n' % (w, h))
                except:
                    pass
            margins = self.code_obj.properties.get('margins')
            if margins:
                try:
                    w, h = [int(i) for i in margins.split(',')]
                    append('    '*(tabs+1) + '<margins>%s, %s</margins>\n' % \
                           (w, h))
                except:
                    pass
            for t in tools:
                self.append_item(t, outfile, tabs+1)
            write('    '*tabs + '</object>\n')

    # end of class ToolBarXrcObject
    
    return ToolBarXrcObject(obj)


class CppCodeGenerator:
    constructor = [('wxWindow*', 'parent'), ('int', 'id'),
                   ('const wxPoint&', 'pos', 'wxDefaultPosition'),
                   ('const wxSize&', 'size', 'wxDefaultSize'),
                   ('long', 'style', 'wxTB_HORIZONTAL|wxTB_NOBORDER')]

    def get_code(self, obj):
        """\
        generates C++ code for the toolbar of a wxFrame.
        """
        cppgen = common.code_writers['C++']
        style = obj.properties.get('style')
        if style:
            style = ', wxDefaultPosition, wxDefaultSize, wxTB_HORIZONTAL|' + \
                    style
        else:
            style = ''
        init = [ '%s = new %s(this, -1%s);\n' % (obj.name, obj.klass, style),
                 'SetToolBar(%s);\n' % obj.name ]
        init.extend(self.get_properties_code(obj))
        ids = self.get_ids_code(obj)
        return init, ids, [], []

    def get_properties_code(self, obj):
        cppgen = common.code_writers['C++']
        tools = obj.properties['toolbar']
        out = []
        append = out.append

        if obj.is_toplevel: obj_name = ''
        else: obj_name = obj.name + '->'

        bitmapsize = obj.properties.get('bitmapsize')
        if bitmapsize:
            try:
                w, h = [int(i) for i in bitmapsize.split(',')]
                append('%sSetToolBitmapSize(wxSize(%s, %s));\n' % \
                       (obj_name, w, h))
            except:
                pass
        margins = obj.properties.get('margins')
        if margins:
            try:
                w, h = [int(i) for i in margins.split(',')]
                append('%sSetMargins(wxSize(%s, %s));\n' % \
                       (obj_name, w, h))
            except:
                pass

        for tool in tools:
            if tool.id == '---': # item is a separator
                append('%sAddSeparator();\n' % obj_name)
            else:
                if tool.id:
                    tokens = tool.id.split('=')
                    if len(tokens) > 1:
                        id = tokens[0]
                    else:
                        id = tool.id
                else:
                    id = 'wxNewId()'
                kinds = ['wxITEM_NORMAL', 'wxITEM_CHECK', 'wxITEM_RADIO']
                try:
                    kind = kinds[int(tool.type)]
                except (IndexError, ValueError):
                    kind = 'wxITEM_NORMAL'
                if tool.bitmap1:
                    bmp1 = 'wxBitmap("%s", wxBITMAP_TYPE_ANY)' % \
                           tool.bitmap1.replace('"', r'\"')
                else:
                    bmp1 = 'wxNullBitmap'
                if tool.bitmap2:
                    bmp2 = 'wxBitmap("%s", wxBITMAP_TYPE_ANY)' % \
                           tool.bitmap2.replace('"', r'\"')
                else:
                    bmp2 = 'wxNullBitmap'
                append('%sAddTool(%s, %s, %s, %s, %s, %s, %s);\n' %
                       (obj_name, id, cppgen.quote_str(tool.label),
                        bmp1, bmp2, kind, cppgen.quote_str(tool.short_help),
                        cppgen.quote_str(tool.long_help)))

        append('%sRealize();\n' % obj_name)

        return out

    def get_ids_code(self, obj):
        ids = []
        tools = obj.properties['toolbar']
        
        for item in tools:
            if item.id == '---': # item is a separator
                pass # do nothing
            else:
                if item.id:
                    tokens = item.id.split('=')
                    if len(tokens) > 1:
                        id = tokens[0]
                        ids.append(' = '.join(tokens))

        return ids

# end of class CppCodeGenerator


def initialize():
    common.class_names['EditToolBar'] = 'wxToolBar'
    common.toplevels['EditToolBar'] = 1

    pygen = common.code_writers.get('python')
    if pygen:
        pygen.add_widget_handler('wxToolBar', PythonCodeGenerator())
        pygen.add_property_handler('tools', ToolsHandler)
    xrcgen = common.code_writers.get('XRC')
    if xrcgen:
        xrcgen.add_widget_handler('wxToolBar', xrc_code_generator)
        xrcgen.add_property_handler('tools', ToolsHandler)
    cppgen = common.code_writers.get('C++')
    if cppgen:
        cppgen.add_widget_handler('wxToolBar', CppCodeGenerator())
        cppgen.add_property_handler('tools', ToolsHandler)
