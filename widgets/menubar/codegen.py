"""
Code generator functions for wxMenuBar objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen
from MenuTree import *


class PythonMenubarGenerator(wcodegen.PythonWidgetCodeWriter):
    def get_properties_code(self, obj):
        return []

    def get_init_code(self, obj):
        cn = self.cn
        out = []
        append = out.append
        quote_str = self.codegen.quote_str
        menus = obj.menus
        ids = []

        obj_name = self.format_widget_access(obj)

        def append_items(menu, items):
            for item in items:
                if item.name == '---':  # item is a separator
                    append('%s.AppendSeparator()\n' % menu)
                    continue
                name, val = self.codegen.generate_code_id(None, item.id)
                if self.codegen.preview or (not name and ( not val or val == '-1')):
                    id = cn('wxNewId()')
                else:
                    if name: ids.append(name)
                    id = val
                if item.children:
                    # a submenu
                    name = item.name or '%s_sub' % menu
                    append(('%s = ' + cn('wxMenu') + '()\n') % name)
                    append_items(name, item.children)
                    args = (menu, id, quote_str(item.label), name, quote_str(item.help_str))
                    append('%s.AppendMenu(%s, %s, %s, %s)\n' % args)
                else:
                    item_type = cn('wxITEM_NORMAL')
                    if item.checkable == '1':
                        item_type = cn('wxITEM_CHECK')
                    elif item.radio == '1':
                        item_type = cn('wxITEM_RADIO')
                    if item.name:
                        # create MenuItem and assign to property, then append to menu
                        name = '%s.%s' % (obj_name, item.name)
                        args = ( name, cn('wxMenuItem'), menu, id, quote_str(item.label), quote_str(item.help_str) )
                        if item_type:
                            append( 'item = %s = %s(%s, %s, %s, %s, %s)\n' % (args + (item_type,)) )
                        else:
                            append( 'item = %s = %s(%s, %s, %s, %s)\n' % args)
                        append('%s.AppendItem(%s)\n' % (menu, name))
                    else:
                        # just append and assign the returned item to a temporary variable
                        args = ( menu, id, quote_str(item.label), quote_str(item.help_str) )
                        if item_type:
                            append( 'item = %s.Append(%s, %s, %s, %s)\n' % (args + (item_type,)) )
                        else:
                            append( 'item = %s.Append(%s, %s, %s)\n' % args)

                    if item.handler:
                        handler = item.handler if "." in item.handler else "self.%s"%item.handler
                        append( "self.Bind(wx.EVT_MENU, %s, id=item.GetId())\n"%handler )

        for m in menus:
            menu = m.root
            if menu.name: name = 'self.' + menu.name
            else: name = 'wxglade_tmp_menu'
            append(('%s = ' + cn('wxMenu') + '()\n') % name)
            if menu.children:
                append_items(name, menu.children)
            append('%s.Append(%s, %s)\n' % (obj_name, name, quote_str(menu.label)))

        return ids + out

    def get_code(self, obj):
        if obj.klass == obj.base:
            klass = self.cn(obj.klass)
        else:
            klass = obj.klass
        init = [ '\n', '# Menu Bar\n', 'self.%s = %s()\n' % (obj.name, klass) ]
        init.extend(self.get_init_code(obj))
        init.append('self.SetMenuBar(self.%s)\n' % obj.name)
        init.append('# Menu Bar end\n')
        return init, [], []

    def get_event_handlers(self, obj):
        out = []

        def do_get(item):
            ret = []
            if item.handler and item.handler!="---":
                # first item is None -> just generate stub for item.handler, do not bind again
                ret.append((None, 'EVT_MENU', item.handler, 'wxCommandEvent'))
            if item.children:
                for c in item.children:
                    ret.extend(do_get(c))
            return ret

        for menu in obj.menus:
            out.extend(do_get(menu.root))
        return out



def xrc_code_generator(obj):
    "function that generates XRC code for the menubar of a wxFrame."
    from xml.sax.saxutils import escape, quoteattr
    xrcgen = common.code_writers['XRC']

    class MenuBarXrcObject(xrcgen.DefaultXrcObject):
        def append_item(self, item, output, tabs):
            if item.name == '---':  # item is a separator
                output.append('    '*tabs + '<object class="separator"/>\n')
            else:
                if item.children:
                    name = self.get_name(item)
                    if name:
                        output.append('    '*tabs + '<object class="wxMenu" name=%s>\n' % quoteattr(name))
                    else:
                        output.append('    '*tabs + '<object class="wxMenu">\n')
                else:
                    name = self.get_name(item)
                    if name:
                        output.append('    '*tabs + '<object class="wxMenuItem" name=%s>\n' % quoteattr(name))
                    else:
                        output.append('    '*tabs + '<object class="wxMenuItem">\n')
                if item.label:
                    # translate & into _ as accelerator marker
                    val = item.label.replace('&', '_')
                    output.append('    '*(tabs+1) + '<label>%s</label>\n' % escape(val))
                if item.help_str:
                    output.append('    '*(tabs+1) + '<help>%s</help>\n' % escape(item.help_str))
                if item.children:
                    for c in item.children:
                        self.append_item(c, output, tabs+1)
                elif item.checkable == '1':
                    output.append('    '*(tabs+1) + '<checkable>1</checkable>\n')
                elif item.radio == '1':
                    output.append('    '*(tabs+1) + '<radio>1</radio>\n')
                output.append('    '*tabs + '</object>\n')

        def get_name(self, item):
            if item.name: return item.name.strip()
            tokens = item.id.split('=')
            if tokens: return tokens[0].strip()

        def write(self, output, tabs):
            output.append('    '*tabs + '<object class="wxMenuBar" name=%s>\n' % quoteattr(self.name))
            for m in self.widget.menus:
                self.append_item(m.root, output, tabs+1)
            output.append('    '*tabs + '</object>\n')

    # end of class MenuBarXrcObject

    return MenuBarXrcObject(obj)


class CppMenubarGenerator(wcodegen.CppWidgetCodeWriter):
    constructor = []

    def get_code(self, obj):
        init = [ '%s = new %s();\n' % (obj.name, obj.klass) ]
        init.extend(self.get_properties_code(obj))
        init.append('SetMenuBar(%s);\n' % obj.name)
        ids = self.get_ids_code(obj)
        return init, ids, [], []

    def get_properties_code(self, obj):
        out = []
        append = out.append
        quote_str = self.codegen.quote_str

        def append_items(menu, items):
            for item in items:
                if item.name == '---':  # item is a separator
                    append('%s->AppendSeparator();\n' % menu)
                    continue
                name, val = self.codegen.generate_code_id(None, item.id)
                if not name and val == '-1':
                    id = 'wxNewId()'
                else:
                    #if name: ids.append(name)
                    id = val
                if item.children:
                    if item.name: name = item.name
                    else: name = '%s_sub' % menu
                    append('wxMenu* %s = new wxMenu();\n' % name)
                    append_items(name, item.children)
                    args = (menu, id, quote_str(item.label), name, quote_str(item.help_str))
                    append('%s->Append(%s, %s, %s, %s);\n' % args)
                else:
                    item_type = 'wxITEM_NORMAL'
                    if item.checkable == '1':
                        item_type = 'wxITEM_CHECK'
                    elif item.radio == '1':
                        item_type = 'wxITEM_RADIO'
                    args = (menu, id, quote_str(item.label), quote_str(item.help_str))
                    if item_type:
                        append( '%s->Append(%s, %s, %s, %s);\n' % (args+(item_type,)) )
                    else:
                        append( '%s->Append(%s, %s, %s);\n' % args )

        obj_name = self.codegen.format_generic_access(obj)

        i = 1
        for m in obj.menus:
            menu = m.root
            if menu.name:
                name = menu.name
            else:
                name = 'wxglade_tmp_menu_%s' % i
                i += 1
            append('wxMenu* %s = new wxMenu();\n' % name)
            if menu.children:
                append_items(name, menu.children)
            append('%sAppend(%s, %s);\n' % (obj_name, name, quote_str(menu.label)))

        return out

    def get_ids_code(self, obj):
        ids = []

        def collect_ids(items):
            for item in items:
                if item.name == '---':  # item is a separator
                    continue  # do nothing
                name, val = self.codegen.generate_code_id(None, item.id)
                if name.find('=') != -1:
                    ids.append(name)
                if item.children:
                    collect_ids(item.children)

        for m in obj.menus:
            if m.root.children:
                collect_ids(m.root.children)

        return ids

    def get_event_handlers(self, obj):
        out = []

        def do_get(item):
            ret = []
            name, val = self.codegen.generate_code_id(None, item.id)
            if not val:
                val = '-1'  # but this is wrong anyway...
            if item.handler:
                ret.append((val, 'EVT_MENU', item.handler, 'wxCommandEvent'))
            if item.children:
                for c in item.children:
                    ret.extend(do_get(c))
            return ret

        for menu in obj.menus:
            out.extend(do_get(menu.root))
        return out



def initialize():
    klass = 'wxMenuBar'
    common.class_names['EditMenuBar'] = klass
    common.toplevels['EditMenuBar'] = 1
    common.register('python', klass, PythonMenubarGenerator(klass) )#, 'menus', MenuHandler)
    common.register('C++',    klass, CppMenubarGenerator(klass),   )# 'menus', MenuHandler)
    common.register('XRC',    klass, xrc_code_generator,           )# 'menus', MenuHandler)
