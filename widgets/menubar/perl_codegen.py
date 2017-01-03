"""
Perl generator functions for wxMenuBar objects

@copyright: 2002-2004 D.H. aka crazyinsomniac on sourceforge.net
@copyright: 2016-2017 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
from wcodegen import PerlWidgetCodeWriter
from .menubar_base import MenubarMixin
from .codegen import MenuHandler


class PerlMenubarGenerator(MenubarMixin, PerlWidgetCodeWriter):

    tmpl_menubar_new = '$self->{%(menubar)s} = Wx::MenuBar->new();\n'
    tmpl_toplevel_set_menubar = '$self->SetMenuBar($self->{%(menubar)s});\n'

    tmpl_menu_new = '%s = Wx::Menu->new();\n'
    tmpl_menu_append_separator = '%(menu)s->AppendSeparator();\n'
    tmpl_menu_append_to_menubar = '%(menubar)s->Append(%(menu)s, %(label)s);\n'
    tmpl_menu_append_to_menu = '%(parent_menu)s->AppendSubMenu(%(sub_menu)s, %(label)s, %(help)s);\n'
    tmpl_menu_add_menuitem = '%(menu)s->Append(%(args)s);\n'
    tmpl_menu_add_and_assign_menuitem = '%(assigment)s = %(menu)s->Append(%(args)s);\n'
    tmpl_menu_tmpname = '$wxglade_tmp_menu'

    tmpl_menuitem_new = '%s = Wx::MenuItem->new(%s);\n'
    tmpl_menuitem_append_to_menu = '%s->Append(%s);\n'
    tmpl_menuitem_tmp_variable = '$wxglade_tmp_item'

    tmpl_inline_access_class_scope = '$self->{%(member)s}'
    tmpl_inline_access_local_scope = '$self->{%(member)s}'

    tmpl_bind = "Wx::Event::EVT_MENU($self, %(item)s->GetId(), $self->can('%(handler)s'));\n"

    tmpl_menuitem_declare_tmp_variable = 'my %s;\n'
    tmpl_menu_declare_tmp_variable = 'my %s;\n'

# end of class PerlMenubarGenerator


def initialize():
    klass = 'wxMenuBar'
    common.class_names['EditMenuBar'] = klass
    common.toplevels['EditMenuBar'] = 1
    common.register('perl', klass, PerlMenubarGenerator(klass), 'menus', MenuHandler)
