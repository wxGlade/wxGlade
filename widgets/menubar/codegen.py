"""
Code generator functions for wxMenuBar objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common, compat
import wcodegen
from MenuTree import *


class PythonMenubarGenerator(wcodegen.PythonWidgetCodeWriter):
    def get_properties_code(self, obj):
        return []

    def get_init_code(self, obj):
        cn = self.cn
        out = []
        quote_str = self.codegen.quote_str
        id_declarations = []

        obj_name = self.format_widget_access(obj)

        def append_items(menu, items):
            for item in items:
                if item.name == '---':  # item is a separator
                    out.append('%s.AppendSeparator()\n' % menu)
                    continue

                id_access = None  # default: access via item.GetId()
                id_declaration, val = self.codegen.generate_code_id(None, item.id)
                if self.codegen.preview or (not id_declaration and ( not val or val == '-1')):
                    id = cn('wxNewId()')
                else:
                    if id_declaration: id_declarations.append(id_declaration)
                    id = val
                    if val!='wx.ID_ANY': id_access = "id=%s"%val

                label = quote_str(item.label)
                help_str = quote_str(item.help_str)

                if item.children:
                    # a submenu
                    name = item.name or '%s_sub' % menu
                    out.append( '%s = %s()\n' % (name, cn('wxMenu')) )
                    append_items(name, item.children)
                    args = (menu, id, label, name, help_str)
                    if compat.IS_PHOENIX:
                        out.append( '%s.Append(%s, %s, %s, %s)\n' % args )
                    else:
                        out.append( '%s.AppendMenu(%s, %s, %s, %s)\n' % args )
                else:
                    # checkable, radio or normal?
                    if item.checkable: item_type = cn('wxITEM_CHECK')
                    elif item.radio:   item_type = cn('wxITEM_RADIO')
                    else:              item_type = None

                    # append and optionally assign the returned item to a temporary variable
                    if item.name:
                        # assign to attribute
                        name = '%s.%s' % (obj_name, item.name)
                        assignment = '%s = '%name
                        if not id_access:
                            id_access = name
                    elif item.handler and not id_access:
                        # assignment to local variable to bind handler
                        assignment = 'item = '
                        id_access = 'item'
                    else:
                        # no assignment necessary, as no handler defined
                        assignment = ''
                    if item_type:
                        out.append( '%s%s.Append(%s, %s, %s, %s)\n'%(assignment, menu, id, label, help_str, item_type) )
                    else:
                        out.append( '%s%s.Append(%s, %s, %s)\n' % ( assignment, menu, id, label, help_str) )

                    if item.handler and (not self.codegen.preview or not item.handler.startswith("lambda ")):
                        handler = item.handler if "." in item.handler else "self.%s"%item.handler
                        out.append( "self.Bind(wx.EVT_MENU, %s, %s)\n"%(handler, id_access) )

        for m in obj.menus:
            menu = m.root
            if menu.name: name = 'self.' + menu.name
            else:         name = 'wxglade_tmp_menu'
            out.append( '%s = %s()\n' % (name, cn('wxMenu')) )
            if menu.children:
                append_items(name, menu.children)
            out.append('%s.Append(%s, %s)\n' % (obj_name, name, quote_str(menu.label)))

        return id_declarations + out

    def get_code(self, obj):
        klass = obj.get_instantiation_class(self.cn, self.cn_class, self.codegen.preview)

        code = [ '# Menu Bar\n', 'self.%s = %s()\n' % (obj.name, klass) ]
        if not obj.IS_CLASS:  # if it's a class, then the menus will be generated in the class code
            code.extend(self.get_init_code(obj))
        code.append('self.SetMenuBar(self.%s)\n' % obj.name)
        code.append('# Menu Bar end\n')
        return code, []

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
        klass = obj.get_instantiation_class(self.cn, self.cn_class)
        init = [ '%s = new %s();\n' % (obj.name, klass) ]
        init.extend(self.get_properties_code(obj))
        init.append('SetMenuBar(%s);\n' % obj.name)
        ids = self.get_ids_code(obj)
        return init, ids, []

    def get_properties_code(self, obj):
        out = []
        quote_str = self.codegen.quote_str

        def append_items(menu, items):
            for item in items:
                if item.name == '---':  # item is a separator
                    out.append('%s->AppendSeparator();\n' % menu)
                    continue
                id_access = None
                name, val = self.codegen.generate_code_id(None, item.id)
                if not name and val == '-1':
                    id = 'wxNewId()'
                else:
                    #if name: ids.append(name)
                    id = val
                    if val!='wxID_ANY': id_access = val

                label = quote_str(item.label)
                help_str = quote_str(item.help_str)
                if item.children:
                    name = item.name or '%s_sub'%menu
                    out.append('wxMenu* %s = new wxMenu();\n' % name)
                    append_items(name, item.children)
                    args = (menu, id, label, name, help_str)
                    out.append('%s->Append(%s, %s, %s, %s);\n' % args)
                else:
                    if item.checkable: item_type = 'wxITEM_CHECK'
                    elif item.radio:   item_type = 'wxITEM_RADIO'
                    else:              item_type = None

                    if item.name:
                        # assign to attribute
                        self.codegen.classes[obj.parent].sub_objs.append( ('wxMenuItem',item.name) )
                        assignment = '%s = '%item.name
                        if not id_access:
                            id_access = "%s->GetId()"%item.name
                    elif item.handler and not id_access:
                        # assignment to local variable to bind handler
                        assignment = 'wxglade_tmp_item = '
                        id_access = 'wxglade_tmp_item->GetId()'
                    else:
                        # no assignment necessary, as no handler defined
                        assignment = ''

                    if item_type:
                        out.append('%s%s->Append(%s, %s, %s, %s);\n'%(assignment, menu, id, label, help_str, item_type))
                    else:
                        out.append( '%s%s->Append(%s, %s, %s);\n' % (assignment, menu, id, label, help_str) )

                    if item.handler:
                        handler = item.handler if "::" in item.handler else '%s::%s'%(obj.parent.klass, item.handler)

                        if self.codegen.for_version==(2,8):
                            tmpl = 'Connect(%(id)s, wxEVT_COMMAND_MENU_SELECTED, wxCommandEventHandler(%(handler)s));\n'
                        else:
                            tmpl = 'Bind(wxEVT_MENU, &%(handler)s, this, %(id)s);\n'
                        out.append( tmpl % {"id":id_access, "handler":handler} )

        obj_name = self.codegen.format_generic_access(obj)

        if any( m for m in obj.menus if not m.root.name ):
            out.append('wxMenu *wxglade_tmp_menu;\n')
        for m in obj.menus:
            menu = m.root
            if menu.name:
                # assign to attribute
                name = menu.name
                self.codegen.classes[obj.parent].sub_objs.append( ('wxMenu',menu.name) )
            else:
                name = 'wxglade_tmp_menu'
            out.append('%s = new wxMenu();\n' % name)
            if menu.children:
                append_items(name, menu.children)
            out.append('%sAppend(%s, %s);\n' % (obj_name, name, quote_str(menu.label)))
        if any( [l for l in out if "wxglade_tmp_item =" in l]):
            out.insert(1, "wxMenuItem *wxglade_tmp_item;\n" )
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
            if item.handler:
                # first item is None -> just generate stub for item.handler, do not bind again
                ret.append((None, 'EVT_MENU', item.handler, 'wxCommandEvent'))
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
    common.register('python', klass, PythonMenubarGenerator(klass) )
    common.register('C++',    klass, CppMenubarGenerator(klass),   )
    common.register('XRC',    klass, xrc_code_generator,           )
