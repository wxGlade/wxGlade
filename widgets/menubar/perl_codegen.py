"""
Perl generator functions for wxMenuBar objects

@copyright: 2002-2004 D.H. aka crazyinsomniac on sourceforge.net
@copyright: 2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen
from MenuTree import *
#from .codegen import MenuHandler


class PerlMenubarGenerator(wcodegen.PerlWidgetCodeWriter):
    def get_properties_code(self, obj):
        return []

    def get_init_code(self, obj):
        out = []
        append = out.append
        menus = obj.menus
        ids = []
        # We need to keep track of tmpnames used.
        tmpsused = {}

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

                label = self.codegen.quote_str(item.label)
                help_str = self.codegen.quote_str(item.help_str)

                if item.children:
                    if item.name:
                        name = item.name
                    else:
                        name = '%s_sub' % menu
                        if not name in tmpsused:
                            tmpsused[name] = 1
                            append('my %s;\n' % name)

                    append('%s = Wx::Menu->new();\n' % name)
                    append_items(name, item.children)
                    append( '%s->Append(%s, %s, %s, %s);\n' % (menu, id, label, name, help_str) )
                else:
                    item_type = 0
                    if item.checkable == '1':
                        item_type = 1
                    elif item.radio == '1':
                        item_type = 2

                    if item.name:
                        itemname = '$self->{%s} = ' % item.name
                    else:
                        itemname = ''

                    if item_type:
                        append( '%s%s->Append(%s, %s, %s, %s);\n' % (itemname, menu, id, label, help_str, item_type) )
                    else:
                        append( '%s%s->Append(%s, %s, %s);\n' % (itemname, menu, id, label, help_str) )
        #self._logger.debug('menus = %s', menus)

        obj_name = self.format_widget_access(obj)

        append('my $wxglade_tmp_menu;\n')  # NOTE below name =
        for m in menus:
            menu = m.root
            if menu.name:
                name = '$self->{%s}' % menu.name
            else:
                name = '$wxglade_tmp_menu'
            append('%s = Wx::Menu->new();\n' % name)
            if menu.children:
                append_items(name, menu.children)
            append('%s->Append(%s, %s);\n' %
                   (obj_name, name, self.codegen.quote_str(menu.label)))

        return ids + out

    def get_code(self, obj):
        klass = obj.get_instantiation_class(self.cn, self.cn_class)

        init = [ '\n\n', '# Menu Bar\n\n', '$self->{%s} = %s->new();\n' % (obj.name, klass) ]
        init.extend(self.get_init_code(obj))
        init.append('$self->SetMenuBar($self->{%s});\n' % obj.name)
        init.append('\n')
        init.append('# Menu Bar end\n\n')
        return init, []

    def get_event_handlers(self, obj):
        ret = []

        def do_get(item):
            if item.handler:
                ret.append((item, 'EVT_MENU', item.handler, 'wxCommandEvent'))
            for c in item.children:
                do_get(c)

        for menu in obj.menus:
            do_get(menu.root)

        return ret



def initialize():
    klass = 'wxMenuBar'
    common.class_names['EditMenuBar'] = klass
    common.register('perl', klass, PerlMenubarGenerator(klass), ) #'menus', MenuHandler)
