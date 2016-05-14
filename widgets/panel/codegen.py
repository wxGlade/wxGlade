"""\
Code generator functions for wxPanel objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PythonPanelGenerator(wcodegen.PythonWidgetCodeWriter):
    def get_code(self, panel):
        prop = panel.properties
        try:
            scrollable = int(prop['scrollable'])
        except:
            scrollable = False
        id_name, id = self.codegen.generate_code_id(panel)
        parent = self.format_widget_access(panel.parent)
        if panel.is_toplevel:
            l = []
            if id_name: l.append(id_name)
            l.append('self.%s = %s(%s, %s)\n' %
                     (panel.name, self.codegen.get_class(panel.klass),
                      parent, id))
            return l, [], []
        init = []
        if id_name:
            init.append(id_name)
        style = prop.get("style", 'wxTAB_TRAVERSAL')
        if scrollable or style != 'wxTAB_TRAVERSAL':
            style = ", style=%s" % self.cn_f(style)
        else:
            style = ''
        if not int(panel.properties.get('no_custom_class', False)) \
               or panel.preview:
            if scrollable:
                klass = self.cn('wxScrolledWindow')
            else:
                klass = self.cn('wxPanel')
        else:
            klass = panel.klass
            if klass in ('wxPanel', 'wxScrolledWindow'):
                klass = self.cn(klass)
        init.append('self.%s = %s(%s, %s%s)\n' % \
                    (panel.name, klass, parent, id, style))
        props_buf = self.codegen.generate_common_properties(panel)
        if scrollable:
            sr = prop.get('scroll_rate', '0, 0')
            props_buf.append('self.%s.SetScrollRate(%s)\n' % (panel.name, sr))
        return init, props_buf, []

    def get_properties_code(self, obj):
        try:
            scrollable = int(obj.properties['scrollable'])
        except:
            scrollable = False
        props_buf = self.codegen.generate_common_properties(obj)
        if scrollable:
            sr = obj.properties.get('scroll_rate', '0, 0')
            props_buf.append('self.SetScrollRate(%s)\n' % sr)
        return props_buf

    def get_layout_code(self, obj):
        ret = ['self.Layout()\n']
        try:
            if int(obj.properties['centered']):
                ret.append('self.Centre()\n')
        except (KeyError, ValueError):
            pass
        return ret

# end of class PythonPanelGenerator


class CppPanelGenerator(wcodegen.CppWidgetCodeWriter):
    constructor = [('wxWindow*', 'parent'), ('wxWindowID', 'id'),
                   ('const wxPoint&', 'pos', 'wxDefaultPosition'),
                   ('const wxSize&', 'size', 'wxDefaultSize'),
                   ('long', 'style', '0')]

    def get_code(self, panel):
        """\
        generates the C++ code for wxPanel objects
        """
        prop = panel.properties
        try:
            scrollable = int(prop['scrollable'])
        except:
            scrollable = False
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
            l = ['%s = new %s(%s, %s);\n' %
                 (panel.name, panel.klass, parent, id)]
            return l, ids, [], []
        extra = ''
        style = prop.get("style", 'wxTAB_TRAVERSAL')
        if scrollable or style != 'wxTAB_TRAVERSAL':
            extra = ', wxDefaultPosition, wxDefaultSize, %s' % style
        if not int(panel.properties.get('no_custom_class', False)):
            if scrollable:
                klass = 'wxScrolledWindow'
            else:
                klass = 'wxPanel'
        else:
            klass = panel.klass
        init = ['%s = new %s(%s, %s%s);\n' %
                (panel.name, klass, parent, id, extra) ]
        props_buf = self.codegen.generate_common_properties(panel)
        if scrollable:
            sr = prop.get('scroll_rate', '0, 0')
            props_buf.append('%s->SetScrollRate(%s);\n' % (panel.name, sr))
        return init, ids, props_buf, []

    def get_properties_code(self, obj):
        try:
            scrollable = int(obj.properties['scrollable'])
        except:
            scrollable = False
        props_buf = self.codegen.generate_common_properties(obj)
        if scrollable:
            sr = obj.properties.get('scroll_rate', '0, 0')
            props_buf.append('SetScrollRate(%s);\n' % sr)
        return props_buf

# end of class CppPanelGenerator


def xrc_code_generator(obj):
    xrcgen = common.code_writers['XRC']

    class XrcCodeGenerator(xrcgen.DefaultXrcObject):
        def write(self, *args, **kwds):
            if 'scrollable' in self.properties:
                style = self.properties.get('style', '').split('|')
                try: style.remove('wxTAB_TRAVERSAL')
                except ValueError: pass
                self.properties['style'] = '|'.join(style)
            for prop in ('scrollable', 'scroll_rate'):
                try: del self.properties[prop]
                except KeyError: pass
            if 'no_custom_class' in self.properties:
                del self.properties['no_custom_class']
            xrcgen.DefaultXrcObject.write(self, *args, **kwds)

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

    common.register('python', klass, PythonPanelGenerator(klass))
    common.register('python', 'wxScrolledWindow', PythonPanelGenerator(klass))
    common.register('C++', klass, CppPanelGenerator(klass))
    common.register('C++', 'wxScrolledWindow', CppPanelGenerator(klass))
    common.register('XRC', klass, xrc_code_generator)
    common.register('XRC', 'wxScrolledWindow', xrc_code_generator)
