"""
Code generator functions for wxMenuBar objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen
from wcodegen.taghandler import BaseCodeWriterTagHandler
from MenuTree import *


class PythonMenubarGenerator(wcodegen.PythonWidgetCodeWriter):
    def get_properties_code(self, obj):
        return []

    def get_init_code(self, obj):
        cn = self.cn
        out = []
        append = out.append
        menus = obj.properties['menubar']
        ids = []

        def append_items(menu, items):
            for item in items:
                if item.name == '---':  # item is a separator
                    append('%s.AppendSeparator()\n' % menu)
                    continue
                name, val = self.codegen.generate_code_id(None, item.id)
                if obj.preview or (not name and ( not val or val == '-1')):
                    id = cn('wxNewId()')
                else:
                    if name: ids.append(name)
                    id = val
                if item.children:
                    if item.name: name = item.name
                    else: name = '%s_sub' % menu
                    append(('%s = ' + cn('wxMenu') + '()\n') % name)
##                     if not obj.preview and item.id: # generating id
##                         tokens = item.id.split('=')
##                         if len(tokens) > 1:
##                             id = tokens[0]
##                             ids.append(' = '.join(tokens) + '\n')
##                         else:
##                             id = item.id
##                     else: id = 'wxNewId()'
                    append_items(name, item.children)
                    append('%s.AppendMenu(%s, %s, %s, %s)\n' %
                           (menu, id, self.codegen.quote_str(item.label),
                            name, self.codegen.quote_str(item.help_str)))
                else:
##                     if not obj.preview and item.id: # no ids for preview
##                         tokens = item.id.split('=')
##                         if len(tokens) > 1:
##                             id = tokens[0]
##                             ids.append(' = '.join(tokens) + '\n')
##                         else:
##                             id = item.id
##                     else: id = 'wxNewId()'

                    item_type = cn('wxITEM_NORMAL')
                    if item.checkable == '1':
                        item_type = cn('wxITEM_CHECK')
                    elif item.radio == '1':
                        item_type = cn('wxITEM_RADIO')
                    if item.name:
                        # ALB 2004-18-07
                        name = 'self.%s' % item.name
                        if item_type:
                            append('%s = %s(%s, %s, %s, %s, %s)\n' %
                                   (name, cn('wxMenuItem'), menu, id,
                                    self.codegen.quote_str(item.label),
                                    self.codegen.quote_str(item.help_str), item_type))
                        else:
                            append('%s = %s(%s, %s, %s, %s)\n' %
                                   (name, cn('wxMenuItem'), menu,
                                    id, self.codegen.quote_str(item.label),
                                    self.codegen.quote_str(item.help_str)))
                        append('%s.AppendItem(%s)\n' % (menu, name))
                    else:
                        if item_type:
                            append('%s.Append(%s, %s, %s, %s)\n' %
                                   (menu, id, self.codegen.quote_str(item.label),
                                    self.codegen.quote_str(item.help_str), item_type))
                        else:
                            append('%s.Append(%s, %s, %s)\n' %
                                   (menu, id, self.codegen.quote_str(item.label),
                                    self.codegen.quote_str(item.help_str)))
        #self._logger.debug('menus = %s', menus)

        obj_name = self.format_widget_access(obj)

        for m in menus:
            menu = m.root
            if menu.name: name = 'self.' + menu.name
            else: name = 'wxglade_tmp_menu'
            append(('%s = ' + cn('wxMenu') + '()\n') % name)
            if menu.children:
                append_items(name, menu.children)
            append('%s.Append(%s, %s)\n' %
                   (obj_name, name, self.codegen.quote_str(menu.label)))

        return ids + out

    def get_code(self, obj):
        if obj.klass == obj.base:
            klass = self.cn(obj.klass)
        else:
            klass = obj.klass
        init = [ '\n', '# Menu Bar\n', 'self.%s = %s()\n' %
                 (obj.name, klass) ]
        init.extend(self.get_init_code(obj))
        init.append('self.SetMenuBar(self.%s)\n' % obj.name)
        init.append('# Menu Bar end\n')
        return init, [], []

    def get_event_handlers(self, obj):
        out = []

        def do_get(item):
            ret = []
            if item.name:
                val = '#self.%s' % item.name  # see py_codegen.py, ~480
            else:
                val = self.codegen.generate_code_id(None, item.id)[1]
                if not val:
                    val = '-1'  # but this is wrong anyway...
            if item.handler:
                ret.append((val, 'EVT_MENU', item.handler, 'wxCommandEvent'))
            if item.children:
                for c in item.children:
                    ret.extend(do_get(c))
            return ret

        for menu in obj.properties['menubar']:
            out.extend(do_get(menu.root))
        return out

# end of class PythonMenubarGenerator


class MenuHandler(BaseCodeWriterTagHandler):
    """\
    Handler for menus and menu items of a menubar
    """
    item_attrs = ('label', 'id', 'name', 'help_str', 'checkable', 'radio',
                  'handler')

    def __init__(self):
        super(MenuHandler, self).__init__()
        self.menu_depth = 0
        self.menus = []
        self.curr_menu = None
        self.curr_item = None

    def start_elem(self, name, attrs):
        if name == 'menu':
            self.menu_depth += 1
            label = attrs['label']
            if self.menu_depth == 1:
                t = MenuTree(attrs['name'], label)
                self.curr_menu = t.root
                self.menus.append(t)
                return
            id = attrs.get('itemid', '')
            handler = attrs.get('handler', '')
            node = MenuTree.Node(label=label, name=attrs['name'], id=id,
                                 handler=handler)
            node.parent = self.curr_menu
            self.curr_menu.children.append(node)
            self.curr_menu = node
        elif name == 'item':
            self.curr_item = MenuTree.Node()

    def end_elem(self, name, code_obj):
        if name == 'menus':
            code_obj.properties['menubar'] = self.menus
            return True
        if name == 'item' and self.curr_menu:
            self.curr_menu.children.append(self.curr_item)
            self.curr_item.parent = self.curr_menu
        elif name == 'menu':
            self.menu_depth -= 1
            self.curr_menu = self.curr_menu.parent
        elif name in self.item_attrs:
            char_data = self.get_char_data()
            setattr(self.curr_item, name, char_data)

# end of class MenuHandler


def xrc_code_generator(obj):
    """\
    function that generates XRC code for the menubar of a wxFrame.
    """
    from xml.sax.saxutils import escape, quoteattr
    xrcgen = common.code_writers['XRC']

    class MenuBarXrcObject(xrcgen.DefaultXrcObject):
        def append_item(self, item, outfile, tabs):
            write = outfile.write
            if item.name == '---':  # item is a separator
                write('    '*tabs + '<object class="separator"/>\n')
            else:
                if item.children:
                    name = self.get_name(item)
                    if name:
                        write('    '*tabs + '<object class="wxMenu" ' \
                              'name=%s>\n' % quoteattr(name))
                    else:
                        write('    '*tabs + '<object class="wxMenu">\n')
                else:
                    name = self.get_name(item)
                    if name:
                        write('    '*tabs + '<object class="wxMenuItem" ' \
                              'name=%s>\n' % quoteattr(name))
                    else:
                        write('    '*tabs + '<object class="wxMenuItem">\n')
                if item.label:
                    # translate & into _ as accelerator marker
                    val = item.label.replace('&', '_')
                    write('    '*(tabs+1) + '<label>%s</label>\n' % \
                          escape(val))
                if item.help_str:
                    write('    '*(tabs+1) + '<help>%s</help>\n' % \
                          escape(item.help_str))
                if item.children:
                    for c in item.children:
                        self.append_item(c, outfile, tabs+1)
                elif item.checkable == '1':
                    write('    '*(tabs+1) + '<checkable>1</checkable>\n')
                elif item.radio == '1':
                    write('    '*(tabs+1) + '<radio>1</radio>\n')
                write('    '*tabs + '</object>\n')

        def get_name(self, item):
            if item.name: return item.name.strip()
            tokens = item.id.split('=')
            if tokens: return tokens[0].strip()

        def write(self, outfile, tabs):
            menus = self.code_obj.properties['menubar']
            write = outfile.write
            write('    '*tabs + '<object class="wxMenuBar" name=%s>\n' % \
                  quoteattr(self.name))
            for m in menus:
                self.append_item(m.root, outfile, tabs+1)
            write('    '*tabs + '</object>\n')

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
        menus = obj.properties['menubar']
        out = []
        append = out.append

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
                    append('%s->Append(%s, %s, %s, %s);\n' %
                           (menu, id, self.codegen.quote_str(item.label),
                            name, self.codegen.quote_str(item.help_str)))
                else:
                    item_type = 'wxITEM_NORMAL'
                    if item.checkable == '1':
                        item_type = 'wxITEM_CHECK'
                    elif item.radio == '1':
                        item_type = 'wxITEM_RADIO'
                    if item_type:
                        append('%s->Append(%s, %s, %s, %s);\n' %
                               (menu, id, self.codegen.quote_str(item.label),
                                self.codegen.quote_str(item.help_str), item_type))
                    else:
                        append('%s->Append(%s, %s, %s);\n' %
                               (menu, id, self.codegen.quote_str(item.label),
                                self.codegen.quote_str(item.help_str)))

        obj_name = self.codegen.format_generic_access(obj)

        i = 1
        for m in menus:
            menu = m.root
            if menu.name:
                name = menu.name
            else:
                name = 'wxglade_tmp_menu_%s' % i
                i += 1
            append('wxMenu* %s = new wxMenu();\n' % name)
            if menu.children:
                append_items(name, menu.children)
            append('%sAppend(%s, %s);\n' %
                   (obj_name, name, self.codegen.quote_str(menu.label)))

        return out

    def get_ids_code(self, obj):
        ids = []
        menus = obj.properties['menubar']

        def collect_ids(items):
            for item in items:
                if item.name == '---':  # item is a separator
                    continue  # do nothing
                name, val = self.codegen.generate_code_id(None, item.id)
                if name.find('=') != -1:
                    ids.append(name)
                if item.children:
                    collect_ids(item.children)

        for m in menus:
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

        for menu in obj.properties['menubar']:
            out.extend(do_get(menu.root))
        return out

# end of class CppMenubarGenerator


def initialize():
    klass = 'wxMenuBar'
    common.class_names['EditMenuBar'] = klass
    common.toplevels['EditMenuBar'] = 1
    common.register('python', klass, PythonMenubarGenerator(klass),
                    'menus', MenuHandler)
    common.register('C++', klass, CppMenubarGenerator(klass),
                    'menus', MenuHandler)
    common.register('XRC', klass, xrc_code_generator,
                    'menus', MenuHandler)
