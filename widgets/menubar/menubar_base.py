"""\
Code shared between different language code generators

@copyright: 2017 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

from wcodegen import BaseWidgetWriter


class MenubarMixin(BaseWidgetWriter):
    """Generic code to handle wxMenu / wxMenuItem code in all language code generators"""

    add_default_item = False
    """Add default item type wxITEM_NORMAL to method call for menu item creation"""

    add_menuids_to_initcode = True
    """Declare menu item ids in the init code"""

    tmpl_menubar_new = ''
    """Statement to create a new wxMenuBar"""

    tmpl_toplevel_set_menubar = ''
    """Statement to add the menubar to the toplevel widget"""

    tmpl_menu_new = ''
    """Statement to create a new wxMenu"""

    tmpl_menu_append_separator = ''
    """Statement to add a separator to a menu"""

    tmpl_menu_append_to_menubar = ''
    """Statement to append a menu to a menubar"""

    tmpl_menu_append_to_menu = ''
    """Statement to append a menu to a menu to create a submenu"""

    tmpl_menu_add_menuitem = ''
    """Statement to add (Append()) a menu item to the menu"""

    tmpl_menu_add_and_assign_menuitem = ''
    """Statement to add a menu item to the menu and assign the created item to a variable"""

    tmpl_menu_tmpname = ''
    """Name to assign a menu item temporary variable to """

    tmpl_inline_access_class_scope = ''
    """Statement to access a class member"""

    tmpl_inline_access_local_scope = ''
    """Statement to access a local variable"""

    tmpl_menuitem_new = ''
    """Statement to create a new wxMenuItem"""

    tmpl_menuitem_append_to_menu = ''
    """Statement to add a wxMenuItem object to a menu"""

    tmpl_menuitem_tmp_variable = ''
    """Name of a local variable to store a reference to a wxMenuItem instance"""

    tmpl_bind = ''
    """Statement to Bind() a menu event to a event handler"""

    tmpl_menuitem_declare_tmp_variable = ''
    """Statement to declare a temporary variable for a wxMenuItem instance"""

    tmpl_menu_declare_tmp_variable = ''
    """Statement to declare a temporary variable for a wxMenu instance"""

    _init_lines = []
    """Buffer to add all lines to initialise the menu"""

    _declared_local_variables = {}
    """Dict to declare local variables once"""

    _menuitem_ids = []
    """Declared IDs for menu items"""

    _menubar_obj = None
    """Reference to the menubar L{xml_parse.CodeObject}"""

    def get_properties_code(self, obj):
        return []

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

    def get_code(self, obj):
        # called for non-toplevel widgets
        out = self._create_all_menu_code(obj)
        return out, [], []

    def get_init_code(self, obj):
        # called for toplevel widgets
        out = self._create_all_menu_code(obj, menubar_is_toplevel=True)
        return out

    def _prepare_tmpl_content(self, obj):
        super(MenubarMixin, self)._prepare_tmpl_content(obj)
        self.tmpl_dict['menubar'] = self.tmpl_dict['obj_name']

    def _reset_vars(self):
        super(MenubarMixin, self)._reset_vars()
        self._menubar_obj = None
        self._init_lines = []
        self._declared_local_variables = {}

    def _create_all_menu_code(self, obj, menubar_is_toplevel=False):
        """Create all menu and menuitem related code"""
        self._reset_vars()
        self._prepare_tmpl_content(obj)
        self._menubar_obj = obj
        self._root_obj = self.get_root_obj(obj)

        if menubar_is_toplevel:
            self._create_menus_and_menuitems()
        else:
            self._create_menubar()
            self._create_menus_and_menuitems()
            self._add_menubar_to_toplevel_widget()
            self._create_enclosing_coments()

        # remove reference to instances of xml_parse.CodeObject to prevent copy.Error exceptions
        # "un(deep)copyable object of type <type 'weakproxy'>" caused by using weak references in the SAX XML parser
        self._menubar_obj = None
        self._root_obj = None

        return self._init_lines

    def _create_enclosing_coments(self):
        self._init_lines.insert(0, '\n')
        self._init_lines.insert(1, '%(comment)s Menu Bar\n' % self.tmpl_dict)
        self._init_lines.append('%(comment)s Menu Bar end\n' % self.tmpl_dict)

    def _create_menubar(self):
        self._init_lines.append(self.tmpl_menubar_new % self.tmpl_dict)

    def _add_menubar_to_toplevel_widget(self):
        self._init_lines.append(self.tmpl_toplevel_set_menubar % self.tmpl_dict)

    def _create_menus_and_menuitems(self):
        menu_lines = []
        for menu_tree in self._menubar_obj.properties['menubar']:
            menu = menu_tree.root

            if menu.name:
                formatted_menu_name = self._format_access_in_local_scope(menu.name)
            else:
                formatted_menu_name = self.tmpl_menu_tmpname

            # declare menu only if it's a non-class member
            if not (menu.name and self.is_class_member(formatted_menu_name)):
                declare_menu_with_local_scope = self._format_declaration_tmp_menu_local_scope(formatted_menu_name)
                if declare_menu_with_local_scope:
                    menu_lines.append(declare_menu_with_local_scope)

            # create the menu
            menu_lines.append(self.tmpl_menu_new % formatted_menu_name)

            # add menu items
            if menu.children:
                menu_lines.extend(self._create_all_menuitems(formatted_menu_name, menu.children))

            # Append the menu to the menubar
            menu_lines.append(self.tmpl_menu_append_to_menubar % {
                'menubar': self.format_widget_access(self._menubar_obj),
                'menu': formatted_menu_name,
                'label': self.codegen.quote_str(menu.label),
                 })

        if self.add_menuids_to_initcode:
            self._init_lines.extend(self._menuitem_ids)
        self._init_lines.extend(menu_lines)

    def _create_all_menuitems(self, menu, menu_items):
        menu_lines = []
        self._menuitem_ids = []

        for item in menu_items:
            self._menu_item = None

            if item.name == '---':  # item is a separator
                menu_lines.append(self.tmpl_menu_append_separator % {'menu': menu})
                continue

            self._generate_and_store_menu_id(item)

            if item.children:
                menu_lines.extend(self._create_submenu(menu, item))
                continue

            if item.name:
                menu_lines.extend(self._create_menuitem_property(menu, item))
            else:
                menu_lines.extend(self._create_menuitem_simple(menu, item))

            if item.handler:
                if self.is_class_member(item.handler):
                    handler = item.handler
                else:
                    handler = self.tmpl_inline_access_class_scope % {
                        'toplevel': self._root_obj.klass,
                        'member': item.handler,
                    }

                tmpl = self.get_tmpl('tmpl_bind')
                menu_lines.append(tmpl % {
                    'item': self._menu_item,
                    'handler': handler,
                    'tab': self.tmpl_dict['tab'],
                })

        return menu_lines

    def _create_submenu(self, menu, item):
        """Add a submenu to the given menu"""

        init_lines = []
        name = item.name or '%s_sub' % menu

        if not item.name:
            declare_menu_with_local_scope = self._format_declaration_tmp_menu_local_scope(name)
            if declare_menu_with_local_scope:
                init_lines.append(declare_menu_with_local_scope)

        init_lines.append(self.tmpl_menu_new % name)
        self._create_all_menuitems(name, item.children)
        args = (menu, self._menu_id, self.codegen.quote_str(item.label), name, self.codegen.quote_str(item.help_str))

        init_lines.append(self.tmpl_menu_append_to_menu % (args,))

        return init_lines

    def _create_menuitem_property(self, menu, item):
        """Create MenuItem and assign to property, then append to menu"""

        init_lines = []
        self._menu_item = self._format_access_in_local_scope(item.name)

        args = (menu, self._menu_id, self.codegen.quote_str(item.label), self.codegen.quote_str(item.help_str))
        args = self._append_itemtype_to_arglist(item, args)

        self.codegen.register_class_member(self._root_obj, 'wxMenuItem', self._menu_item)

        init_lines.append(self.tmpl_menuitem_new % (self._menu_item, self.arg_sep.join(args)))
        init_lines.append(self.tmpl_menuitem_append_to_menu % (menu, self._menu_item))

        return init_lines

    def _create_menuitem_simple(self, menu, item):
        """Append single menu entry to the menu"""

        init_lines = []
        args = (self._menu_id, self.codegen.quote_str(item.label), self.codegen.quote_str(item.help_str))
        args = self._append_itemtype_to_arglist(item, args)

        args_dict = {'menu': menu, 'args': self.arg_sep.join(args)}

        # assign the returned item to a temporary variable to use to Bind() the handler
        if item.handler:
            args_dict['assigment'] = self.tmpl_menuitem_tmp_variable
            tmpl = self.tmpl_menu_add_and_assign_menuitem
            self._menu_item = self.tmpl_menuitem_tmp_variable
            declare_menu_with_local_scope = self._format_declaration_tmp_menuitem_local_scope(self._menu_item)
            if declare_menu_with_local_scope:
                init_lines.append(declare_menu_with_local_scope)
        else:
            tmpl = self.tmpl_menu_add_menuitem

        init_lines.append(tmpl % args_dict)
        return init_lines

    def _append_itemtype_to_arglist(self, item, args):
        """Return argument list extended by the item type, if it's a non-default wxItemKind"""
        if item.checkable == '1':
            args += (self.cn('wxITEM_CHECK'), )
        elif item.radio == '1':
            args += (self.cn('wxITEM_RADIO'), )
        elif self.add_default_item:
            args += (self.cn('wxITEM_NORMAL'), )
        return args

    def _format_access_in_class_scope(self, name):
        if self.is_class_member(name):
            handler = name.handler
        else:
            handler = self.tmpl_inline_access_class_scope % {'toplevel': self._root_obj.klass, 'member': name}
        return handler

    def _format_access_in_local_scope(self, name):
        if self.is_class_member(name):
            handler = name.handler
        else:
            handler = self.tmpl_inline_access_local_scope % {'member': name}
        return handler

    def _format_declaration_tmp_menuitem_local_scope(self, name):
        return self._format_generic_declaration(name, self.tmpl_menuitem_declare_tmp_variable)

    def _format_declaration_tmp_menu_local_scope(self, name):
        return self._format_generic_declaration(name, self.tmpl_menu_declare_tmp_variable)

    def _format_generic_declaration(self, name, tmpl):
        """Return the declaration statement based on the given template or None if a declaration isn't necessary"""
        if not name or self.is_class_member(name) or \
           name in self._declared_local_variables or \
           not tmpl:
            return None
        self._declared_local_variables[name] = 1
        return tmpl % name

# XXX
    def _generate_and_store_menu_id(self, menu_item):
        name, val = self.codegen.generate_code_id(None, menu_item.id)
        if self._root_obj.preview or (not name and (not val or val == '-1')):
            self._menu_id = self.cn('wxNewId()')
        else:
            self._menu_id = val
            if name:
                self._menuitem_ids.append(name)
