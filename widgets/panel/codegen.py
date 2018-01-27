"""\
Code generator functions for wxPanel objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2017 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PythonPanelGenerator(wcodegen.PythonWidgetCodeWriter):
    def get_code(self, panel):
        scrollable = panel.scrollable
        id_name, id = self.codegen.generate_code_id(panel)
        parent = self.format_widget_access(panel.parent)
        if panel.is_toplevel:
            l = []
            if id_name: l.append(id_name)
            l.append('self.%s = %s(%s, %s)\n' % (panel.name, self.codegen.get_class(panel.klass), parent, id))
            return l, [], []
        init = []
        if id_name:
            init.append(id_name)
        style = panel.properties["style"].get_string_value() or 'wxTAB_TRAVERSAL'
        if scrollable or style != 'wxTAB_TRAVERSAL':
            style = ", style=%s" % self.cn_f(style)
        else:
            style = ''
        #if not int(panel.properties.get('no_custom_class', False)) or panel.preview:
        if not panel.no_custom_class or self.codegen.preview:
            if scrollable:
                klass = self.cn( 'wxScrolledWindow')
            else:
                klass = self.cn('wxPanel')
        else:
            klass = panel.klass
            if klass in ('wxPanel', 'wxScrolledWindow'):
                klass = self.cn(klass)
        init.append( 'self.%s = %s(%s, %s%s)\n' % (panel.name, klass, parent, id, style) )
        props_buf = self.codegen.generate_common_properties(panel)
        if scrollable and panel.check_prop("scroll_rate"):
            props_buf.append('self.%s.SetScrollRate(%s)\n' % (panel.name, panel.scroll_rate))
        return init, props_buf, []

    def get_properties_code(self, obj):
        props_buf = self.codegen.generate_common_properties(obj)
        if obj.scrollable and obj.check_prop("scroll_rate"):
            props_buf.append('self.SetScrollRate(%s)\n' % obj.scroll_rate)
        return props_buf

    def get_layout_code(self, obj):
        ret = ['self.Layout()\n']
        if "centered" in obj.properties and obj.centered:
            ret.append('self.Centre()\n')
        return ret


class CppPanelGenerator(wcodegen.CppWidgetCodeWriter):
    constructor = [('wxWindow*', 'parent'), ('wxWindowID', 'id'),
                   ('const wxPoint&', 'pos', 'wxDefaultPosition'),
                   ('const wxSize&', 'size', 'wxDefaultSize'),
                   ('long', 'style', '0')]

    def get_code(self, panel):
        "generates the C++ code for wxPanel objects"
        prop = panel.properties
        scrollable = panel.scrollable
        id_name, id = self.codegen.generate_code_id(panel)
        if id_name:
            ids = [id_name]
        else:
            ids = []
        if not panel.parent.is_toplevel:
            parent = '%s' % panel.parent.name
        else:
            parent = 'this'
        if panel.is_toplevel:
            l = [ '%s = new %s(%s, %s);\n' % (panel.name, panel.klass, parent, id) ]
            return l, ids, [], []
        extra = ''
        style = panel.properties["style"].get_string_value() or 'wxTAB_TRAVERSAL'
        if scrollable or style != 'wxTAB_TRAVERSAL':
            extra = ', wxDefaultPosition, wxDefaultSize, %s' % style
        if not panel.no_custom_class:
            if scrollable:
                klass = 'wxScrolledWindow'
            else:
                klass = 'wxPanel'
        else:
            klass = panel.klass
        init = [ '%s = new %s(%s, %s%s);\n' % (panel.name, klass, parent, id, extra) ]
        props_buf = self.codegen.generate_common_properties(panel)
        if scrollable and panel.check_prop("scroll_rate"):
            props_buf.append('%s->SetScrollRate(%s);\n' % (panel.name, panel.scroll_rate))
        return init, ids, props_buf, []

    def get_properties_code(self, obj):
        props_buf = self.codegen.generate_common_properties(obj)
        if obj.scrollable and obj.check_prop("scroll_rate"):
            props_buf.append('SetScrollRate(%s);\n' % obj.scroll_rate)
        return props_buf



def xrc_code_generator(obj):
    xrcgen = common.code_writers['XRC']

    class XrcCodeGenerator(xrcgen.DefaultXrcObject):
        def write(self, out, tabs, properties=None):
            if properties is None: properties = {}
            if self.widget.check_prop('scrollable') and self.widget.scrollable:
                style = self.widget.properties['style'].get_string_value().split("|")
                if 'wxTAB_TRAVERSAL' in style: style.remove('wxTAB_TRAVERSAL')
                properties['style'] = '|'.join(style)
            properties["scrollable"] = properties["scroll_rate"] = None # not to be written
            properties['no_custom_class'] = None
            xrcgen.DefaultXrcObject.write(self, out, tabs, properties)

    return XrcCodeGenerator(obj)


def initialize():
    klass = 'wxPanel'
    common.class_names['EditPanel'] = klass
    common.class_names['EditTopLevelPanel'] = klass
    common.toplevels['EditPanel'] = 1
    common.toplevels['EditTopLevelPanel'] = 1

    common.class_names['EditScrolledWindow'] = 'wxScrolledWindow'
    common.class_names['EditTopLevelScrolledWindow'] = 'wxScrolledWindow'
    common.toplevels['EditScrolledWindow'] = 1
    common.toplevels['EditTopLevelScrolledWindow'] = 1

    common.register('python', klass,              PythonPanelGenerator(klass))
    common.register('python', 'wxScrolledWindow', PythonPanelGenerator(klass))
    common.register('C++',    klass,              CppPanelGenerator(klass))
    common.register('C++',    'wxScrolledWindow', CppPanelGenerator(klass))
    common.register('XRC',    klass,              xrc_code_generator)
    common.register('XRC',    'wxScrolledWindow', xrc_code_generator)
