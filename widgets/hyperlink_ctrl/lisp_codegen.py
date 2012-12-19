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
        lispgen = common.code_writers['lisp']
        prop = obj.properties

        attribute = lispgen.test_attribute(obj)

        id_name, id = lispgen.generate_code_id(obj)
        label = lispgen.quote_str(prop.get('label', ''))
        url = lispgen.quote_str(prop.get('url', ''))
        if not obj.parent.is_toplevel:
            parent = '(slot-%s obj)' % obj.parent.name
        else:
            parent = '(slot-top-window obj)'

        style = prop.get("style")
        if not style:
            style = '0'
        else:
            style = style.strip().replace('|', ' ')
            if style.find(' ') != -1:
                style = '(logior %s)' % style

        if id_name:
            init.append(id_name)

        init.append('(setf (slot-%s obj) (wxHyperlinkCtrl %s %s %s '
                    '%s -1 -1 -1 -1 %s))\n' % (
                        obj.name, parent, id, label, url, style
                        )
                   )

        props_buf = lispgen.generate_common_properties(obj)
        if not attribute:
            # the object doesn't have to be stored as an attribute of the
            # custom class, but it is just considered part of the layout
            return [], [], init + props_buf
        return init, props_buf, []

# end of class LispCodeGenerator


def initialize():

    common.class_names['EditHyperlinkCtrl'] = 'wxHyperlinkCtrl'
    lispgen = common.code_writers.get('lisp')

    if lispgen:
        lispgen.add_widget_handler('wxHyperlinkCtrl', LispCodeGenerator())
