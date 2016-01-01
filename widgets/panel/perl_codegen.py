"""\
Perl generator functions for wxPanel objects

@copyright: 2002-2004 D.H. aka crazyinsomniac on sourceforge.net
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PerlPanelGenerator(wcodegen.PerlWidgetCodeWriter):
    new_signature = [
        '$parent', '$id', '$pos', '$size', '$style', '$name'
    ]

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
            if id_name:
                l.append(id_name)

            klass = panel.base
            if klass != panel.klass:
                klass = panel.klass
            else:
                klass = klass.replace('wx', 'Wx::', 1)

            l.append('$self->{%s} = %s->new(%s, %s);\n' %
                     (panel.name, klass, parent, id))
            return l, [], []

        init = []
        if id_name:
            init.append(id_name)
        style = prop.get("style", 'wxTAB_TRAVERSAL')
        if not (scrollable or style != 'wxTAB_TRAVERSAL'):
            style = ''
        if not int(panel.properties.get('no_custom_class', False)):
            if scrollable:
                klass = 'Wx::ScrolledWindow'
            else:
                klass = 'Wx::Panel'
        else:
            klass = self.codegen.cn(panel.klass)

        if style:
            extra = ', wxDefaultPosition, wxDefaultSize, %s' % style
        else:
            extra = ''

        init.append('$self->{%s} = %s->new(%s, %s%s);\n' %
                    (panel.name, klass, parent, id, extra))

        props_buf = self.codegen.generate_common_properties(panel)
        if scrollable:
            sr = prop.get('scroll_rate', '0, 0')
            props_buf.append('$self->{%s}->SetScrollRate(%s);\n' %
                             (panel.name, sr))
        return init, props_buf, []

    def get_properties_code(self, obj):
        try:
            scrollable = int(obj.properties['scrollable'])
        except:
            scrollable = False

        props_buf = self.codegen.generate_common_properties(obj)
        if scrollable:
            sr = obj.properties.get('scroll_rate', '0, 0')
            props_buf.append('$self->SetScrollRate(%s);\n' % sr)
        return props_buf

    def get_layout_code(self, obj):
        ret = ['$self->Layout();\n']
        try:
            if int(obj.properties['centered']):
                ret.append('$self->Centre();\n')
        except (KeyError, ValueError):
            pass
        return ret

# end of class PerlPanelGenerator


def initialize():
    klass = 'wxPanel'
    common.class_names['EditPanel'] = klass
    common.class_names['EditTopLevelPanel'] = klass
    common.toplevels['EditPanel'] = 1
    common.toplevels['EditTopLevelPanel'] = 1
    common.register('perl', klass, PerlPanelGenerator(klass))
    common.register('perl', 'wxScrolledWindow', PerlPanelGenerator(klass))
