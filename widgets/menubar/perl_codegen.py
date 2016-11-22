"""
Perl generator functions for wxMenuBar objects

@copyright: 2002-2004 D.H. aka crazyinsomniac on sourceforge.net
@copyright: 2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen
from .codegen import MenuHandler


class PerlMenubarGenerator(wcodegen.PerlWidgetCodeWriter):
    def get_properties_code(self, obj):
        return []

    def get_init_code(self, obj):
        out = []
        append = out.append
        quote_str = self.codegen.quote_str
        menus = obj.properties['menubar']
        ids = []
        tmpsused = {}           # keep track used temporary names
        obj_name = self.format_widget_access(obj)

        def append_items(menu, items):
            for item in items:
                if item.name == '---':  # item is a separator
                    append('%s->AppendSeparator();\n' % menu)
                    continue
                name, val = self.codegen.generate_code_id(None, item.id)
                if not name and ( not val or val == '-1'):
                    id = 'Wx::NewId()'
                else:
                    if name: ids.append(name)
                    id = val

                if item.children:
                    # a submenu
                    name = item.name or '%s_sub' % menu

                    if not item.name:
                        if name not in tmpsused:
                            tmpsused[name] = 1
                            append('my %s;\n' % name)

                    append('%s = Wx::Menu->new();\n' % name)
                    append_items(name, item.children)
                    args = (menu, id, quote_str(item.label), name, quote_str(item.help_str))
                    append('%s->Append(%s, %s, %s, %s);\n' % args)
                    return

                item_type = None
                if item.checkable == '1':
                    item_type = self.cn('wxITEM_CHECK')
                elif item.radio == '1':
                    item_type = self.cn('wxITEM_RADIO')

                if item.name:
                    # create MenuItem and assign to property, then append to menu
                    menu_item = '$self->{%s}' % item.name
                    args = ( menu, id, quote_str(item.label), quote_str(item.help_str) )
                    if item_type:
                        args += (item_type,)
                    append('%s = Wx::MenuItem->new(%s);\n' % (menu_item, ', '.join(args)))
                    append('%s->Append(%s);\n' % (menu, menu_item))

                else:
                    # append single menu entry to the menu
                    args = ( id, quote_str(item.label), quote_str(item.help_str) )
                    tmpl = '%s->Append(%%s);\n' % menu
                    if item_type:
                        args += (item_type,)

                    # assign the returned item to a temporary variable to use to Bind() the handler
                    if item.handler:
                        tmpl = '$item = %s' % tmpl
                        menu_item = '$item'
                        if menu_item not in tmpsused:
                            tmpsused[menu_item] = 1
                            append('my %s;\n' % menu_item)

                    append(tmpl % ', '.join(args))

                if item.handler:
                    handler = item.handler if "->" in item.handler else "$self->{%s}" % item.handler
                    append("Wx::Event::EVT_MENU($self, %s->GetId(), $self->can('%s'));\n" % (menu_item, handler))

        for m in menus:
            menu = m.root
            if menu.name:
                menu_item = '$self->{%s}' % menu.name
            else:
                menu_item = '$wxglade_tmp_menu'
                if menu_item not in tmpsused:
                    tmpsused[menu_item] = 1
                    append('my %s;\n' % menu_item)    # define temporary wxMenuItem variable
            append('%s = Wx::Menu->new();\n' % menu_item)
            if menu.children:
                append_items(menu_item, menu.children)
            append('%s->Append(%s, %s);\n' % (obj_name, menu_item, quote_str(menu.label)))

        return ids + out

    def get_code(self, obj):
        if obj.klass == obj.base:
            klass = self.cn(obj.klass)
        else:
            klass = obj.klass
        init = ['\n', '# Menu Bar\n', '$self->{%s} = %s->new();\n' % (obj.name, klass)]
        init.extend(self.get_init_code(obj))
        init.append('$self->SetMenuBar($self->{%s});\n' % obj.name)
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

        for menu in obj.properties['menubar']:
            out.extend(do_get(menu.root))
        return out

# end of class PerlMenubarGenerator


def initialize():
    klass = 'wxMenuBar'
    common.class_names['EditMenuBar'] = klass
    common.toplevels['EditMenuBar'] = 1
    common.register('perl', klass, PerlMenubarGenerator(klass), 'menus', MenuHandler)
