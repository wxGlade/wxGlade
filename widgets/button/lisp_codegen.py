# lisp_codegen.py : lisp generator functions for wxButton objects
# $Id: lisp_codegen.py,v 1.3 2007/04/01 12:29:50 agriggio Exp $
#
# Copyright (c) 2002-2004 D.H. aka crazyinsomniac on sourceforge.net
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY


import common

class LispCodeGenerator:
    def get_code(self, obj):
        """\
        fuction that generates lisp code for wxButton objects.
        """
        init = []

        codegen = common.code_writers['lisp']
        prop = obj.properties
        id_name, id = codegen.generate_code_id(obj)
        stockitem = prop.get('stockitem', 'None')
        if stockitem != 'None':
            label = codegen.quote_str('')
            id = "wxID_" + stockitem
        else:
            label = codegen.quote_str(prop.get('label', ''))
        
        if not obj.parent.is_toplevel:
            parent = '(slot-%s obj)' % obj.parent.name
        else:
            parent = '(slot-top-window obj)'

        style = prop.get("style")
        if not style:
            style = '0'
        else:
            style = codegen.cn_f(style)

        if id_name: init.append(id_name)

        init.append('(setf (slot-%s obj) (wxButton_Create %s %s %s -1 -1 -1 -1 %s))\n' %
                    (obj.name, parent, id, label, style))
        props_buf = codegen.generate_common_properties(obj)

        if prop.get('default', False):
            props_buf.append('(wxButton_SetDefault (slot-%s obj))\n' % obj.name)

        return init, props_buf, []

# end of class LispCodeGenerator

def initialize():
    common.class_names['EditButton'] = 'wxButton'

    codegen = common.code_writers.get('lisp')
    if codegen:
        codegen.add_widget_handler('wxButton', LispCodeGenerator())
