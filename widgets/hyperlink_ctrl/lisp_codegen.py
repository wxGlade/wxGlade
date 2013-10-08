"""
Lisp code generator functions for wxHyperlinkCtrl objects

@copyright: 2012 Carsten Grohmann <mail@carstengrohmann.de>
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common


class LispCodeGenerator:

    supported_by = ((2, 8), (3, 0),)
    """\
    wxHyperlinkCtrl is only available at wx 2.8 and newer
    """

    def get_code(self, obj):
        init = []
        codegen = common.code_writers['lisp']
        prop = obj.properties

        attribute = codegen.test_attribute(obj)

        id_name, id = codegen.generate_code_id(obj)
        label = codegen.quote_str(prop.get('label', ''))
        url = codegen.quote_str(prop.get('url', ''))
        if not obj.parent.is_toplevel:
            parent = '(slot-%s obj)' % obj.parent.name
        else:
            parent = '(slot-top-window obj)'

        style = prop.get("style")
        if not style:
            style = '0'
        else:
            style = codegen.cn_f(style)

        if id_name:
            init.append(id_name)

        init.append('(setf (slot-%s obj) (wxHyperlinkCtrl %s %s %s '
                    '%s -1 -1 -1 -1 %s))\n' % (
                        obj.name, parent, id, label, url, style
                        )
                   )

        props_buf = codegen.generate_common_properties(obj)
        if not attribute:
            # the object doesn't have to be stored as an attribute of the
            # custom class, but it is just considered part of the layout
            return [], [], init + props_buf
        return init, props_buf, []

# end of class LispCodeGenerator


def initialize():

    common.class_names['EditHyperlinkCtrl'] = 'wxHyperlinkCtrl'
    codegen = common.code_writers.get('lisp')

    if codegen:
        codegen.add_widget_handler('wxHyperlinkCtrl', LispCodeGenerator())
