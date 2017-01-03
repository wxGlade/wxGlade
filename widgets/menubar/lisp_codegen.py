"""
Lisp generator functions for wxMenuBar objects

@copyright: 2002-2004 D.H. aka crazyinsomniac on sourceforge.net
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
from wcodegen import LispWidgetCodeWriter
from .menubar_base import MenubarMixin
from codegen import MenuHandler


class LispMenubarGenerator(MenubarMixin, LispWidgetCodeWriter):

    add_default_item = True

    tmpl_menubar_new = '(setf (slot-%(menubar)s obj) (wxMenuBar_Create 0))\n'
    tmpl_toplevel_set_menubar = '(wxFrame_SetMenuBar (slot-top-window obj) (slot-%(menubar)s obj))\n'

    tmpl_menu_new = '(let ((%s (wxMenu_Create "" 0))))\n'
    tmpl_menu_append_separator = '(wxMenu_AppendSeparator %(menu)s)\n'
    tmpl_menu_append_to_menubar = '(wxMenuBar_Append %(menubar)s %(menu)s %(label)s))\n'
    tmpl_menu_append_to_menu = '(wxMenu_Append (%(menu)s) %s %s %s %s)\n'
    tmpl_menu_add_menuitem = '(wxMenu_Append %(menu)s %(args)s)\n'
    tmpl_menu_add_and_assign_menuitem = '(let ((%(assigment)s (wxMenu_Append %(menu)s %(args)s))))\n'
    tmpl_menu_tmpname = 'wxglade_tmp_menu'

    tmpl_menuitem_new = '(let (%s (wxMenuItem_Create %s)))\n'
    tmpl_menuitem_append_to_menu = '(wxMenu_Append %s %s)\n'
    tmpl_menuitem_tmp_variable = 'wxglade_tmp_item'

    tmpl_inline_access_class_scope = '%(member)s'
    tmpl_inline_access_local_scope = '%(member)s'

    tmpl_bind = "(wxEvtHandler_Connect (slot-top-window obj) %(item)s (expwxEVT_MENU)\n" \
                "%(tab)s%(tab)s(wxClosure_Create #'%(handler)s obj))\n"

# end of class LispMenubarGenerator


def initialize():
    klass = 'wxMenuBar'
    common.class_names['EditMenuBar'] = klass
    common.toplevels['EditMenuBar'] = 1
    common.register('lisp', klass, LispMenubarGenerator(klass), 'menus', MenuHandler)
