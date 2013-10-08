"""
Code generator functions for wxHyperlinkCtrl objects

@copyright: 2012 Carsten Grohmann <mail@carstengrohmann.de>
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common


class PythonCodeGenerator:

    supported_by = ((2, 8), (3, 0),)
    """\
    wxHyperlinkCtrl is only available at wx 2.8 and newer
    """

    def get_code(self, obj):
        pygen = common.code_writers['python']
        prop = obj.properties

        attribute = pygen.test_attribute(obj)

        id_name, id = pygen.generate_code_id(obj)
        label = pygen.quote_str(prop.get('label', ''))
        url = pygen.quote_str(prop.get('url', ''))
        if not obj.parent.is_toplevel:
            parent = 'self.%s' % obj.parent.name
        else:
            parent = 'self'
        style = prop.get("style")
        if style:
            style = ", style=%s" % pygen.cn_f(style)
        else:
            style = ''
        init = []
        if id_name:
            init.append(id_name)
        if attribute:
            prefix = 'self.'
        else:
            prefix = ''
        klass = obj.klass
        if klass == obj.base:
            klass = pygen.cn(klass)
        init.append('%s%s = %s(%s, %s, %s, %s%s)\n' %
                    (prefix, obj.name, klass, parent, id, label, url, style))
        props_buf = pygen.generate_common_properties(obj)
        if not attribute:
            # the object doesn't have to be stored as an attribute of the
            # custom class, but it is just considered part of the layout
            return [], [], init + props_buf
        return init, props_buf, []

# end of class PythonCodeGenerator


class CppCodeGenerator:

    extra_headers = ['<wx/hyperlink.h>']

    supported_by = ((2, 8), (3, 0),)
    """\
    wxHyperlinkCtrl is only available at wx 2.8 and newer
    """

    def get_code(self, obj):
        """\
        generates the C++ code for wxHyperlinkCtrl objects
        """
        cppgen = common.code_writers['C++']
        prop = obj.properties
        id_name, id = cppgen.generate_code_id(obj)
        if id_name:
            ids = [id_name]
        else:
            ids = []

        attribute = cppgen.test_attribute(obj)

        label = cppgen.quote_str(prop.get('label', ''))
        url = cppgen.quote_str(prop.get('url', ''))
        if not obj.parent.is_toplevel:
            parent = '%s' % obj.parent.name
        else:
            parent = 'this'
        extra = ''
        style = prop.get("style")
        if style:
            extra = ', wxDefaultPosition, wxDefaultSize, %s' % style
        if attribute:
            prefix = ''
        else:
            prefix = '%s* ' % obj.klass
        init = ['%s%s = new %s(%s, %s, %s, %s%s);\n' %
                (prefix, obj.name, obj.klass, parent, id, label, url, extra)]
        props_buf = cppgen.generate_common_properties(obj)
        if not attribute:
            return [], ids, [], init + props_buf
        return init, ids, props_buf, []

# end of class CppCodeGenerator


def xrc_code_generator(obj):
    xrcgen = common.code_writers['XRC']

    class XrcCodeGenerator(xrcgen.DefaultXrcObject):
        def write(self, *args, **kwds):
            try:
                del self.properties['attribute']
            except KeyError:
                pass
            xrcgen.DefaultXrcObject.write(self, *args, **kwds)

    return XrcCodeGenerator(obj)


def initialize():
    common.class_names['EditHyperlinkCtrl'] = 'wxHyperlinkCtrl'

    pygen = common.code_writers.get("python")
    if pygen:
        pygen.add_widget_handler('wxHyperlinkCtrl', PythonCodeGenerator())
    cppgen = common.code_writers.get('C++')
    if cppgen:
        cppgen.add_widget_handler('wxHyperlinkCtrl', CppCodeGenerator())
    xrcgen = common.code_writers.get('XRC')
    if xrcgen:
        xrcgen.add_widget_handler('wxHyperlinkCtrl', xrc_code_generator)
