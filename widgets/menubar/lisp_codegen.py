"""
Lisp generator functions for wxMenuBar objects

@copyright: 2002-2004 D.H. aka crazyinsomniac on sourceforge.net
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen
from MenuTree import *
from codegen import MenuHandler


class LispMenubarGenerator(wcodegen.LispWidgetCodeWriter):
    def get_properties_code(self, obj):
        return []

    def get_init_code(self, obj):
        out = []
        append = out.append
        menus = obj.properties['menubar']
        ids = []

        def append_items(menu, items):
            for item in items:
                if item.name == '---':  # item is a separator
                    append('(wxMenu_AppendSeparator %s)\n' % menu)
                    continue
                id_name, id_number = self.codegen.generate_code_id(None, item.id)
                if not id_name and ( not id_number or id_number == '-1'):
                    widget_id = '-1'
                else:
                    if id_name: ids.append(id_name)
                    widget_id = id_number

                if item.children:
                    if item.name:
                        id_name = item.name
                    else:
                        id_name = '%s_sub' % menu

                    append('(let ((%s (wxMenu_Create "" 0)))\n' % id_name)
                    append_items(id_name, item.children)
                    append('(wxMenuBar_AppendSub %s %s %s %s %s))\n' %
                           (menu, widget_id, self.codegen.quote_str(item.label),
                            id_name, self.codegen.quote_str(item.help_str)))
                else:
                    item_type = 0
                    if item.checkable == '1':
                        item_type = 1
                    elif item.radio == '1':
                        item_type = 2
                    append('(wxMenu_Append %s %s %s %s %s)\n' %
                           (menu, widget_id, self.codegen.quote_str(item.label),
                            self.codegen.quote_str(item.help_str), item_type))

        for m in menus:
            menu = m.root
            if menu.name:
                name = menu.name
            else:
                name = 'wxglade_tmp_menu'
            append('(let ((%s (wxMenu_Create "" 0)))\n' % name)
            if menu.children:
                append_items(name, menu.children)
            append('\t\t(wxMenuBar_Append (slot-%s obj) %s %s))\n' %
                   (obj.name, name, self.codegen.quote_str(menu.label)))

        return ids + out

    def get_code(self, obj):
        init = [ '\n', ';;; Menu Bar\n', '(setf (slot-%s obj) (wxMenuBar_Create 0))\n' %
                 obj.name]
        init.extend(self.get_init_code(obj))
        init.append('(wxFrame_SetMenuBar (slot-top-window obj) ' \
                    '(slot-%s obj))\n' % obj.name)
        init.append(';;; Menu Bar end\n\n')
        return init, [], []

# end of class LispMenubarGenerator


def initialize():
    klass = 'wxMenuBar'
    common.class_names['EditMenuBar'] = klass
    common.toplevels['EditMenuBar'] = 1
    common.register('lisp', klass, LispMenubarGenerator(klass),
                    'menus', MenuHandler)
