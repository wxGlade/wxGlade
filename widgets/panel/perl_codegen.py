"""\
Perl generator functions for wxPanel objects

@copyright: 2002-2004 D.H. aka crazyinsomniac on sourceforge.net
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2018-2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PerlPanelGenerator(wcodegen.PerlWidgetCodeWriter):
    new_signature = ['$parent', '$id', '$pos', '$size', '$style', '$name']

    def get_code(self, panel):
        prop = panel.properties
        scrollable = panel.scrollable

        id_name, id = self.codegen.generate_code_id(panel)
        parent = self.format_widget_access(panel.parent_window)
        klass = panel.get_instantiation_class(self.cn, self.cn_class)

        if panel.IS_CLASS:
            l = []
            if id_name: l.append(id_name)
            l.append( '$self->{%s} = %s->new(%s, %s);\n' % (panel.name, klass, parent, id) )
            return l, []

        init = []
        if id_name:
            init.append(id_name)
        style = panel.properties["style"].get_string_value() or 'wxTAB_TRAVERSAL'
        if not (scrollable or style != 'wxTAB_TRAVERSAL'):
            style = ''

        extra = style and ', wxDefaultPosition, wxDefaultSize, %s'%style or ''

        init.append( '$self->{%s} = %s->new(%s, %s%s);\n' % (panel.name, klass, parent, id, extra) )

        init += self.codegen.generate_code_common_properties(panel)
        if scrollable and panel.check_prop("scroll_rate"):
            init.append( '$self->{%s}->SetScrollRate(%s);\n' % (panel.name, panel.scroll_rate) )
        return init, []

    def get_properties_code(self, obj):
        # XXX check whether this can be used to unify with above code:
        # self.format_widget_access(obj)
        props_buf = self.codegen.generate_code_common_properties(obj)
        if obj.scrollable and obj.check_prop("scroll_rate"):
            props_buf.append('$self->SetScrollRate(%s);\n' % obj.scroll_rate)
        return props_buf

    def get_layout_code(self, obj):
        ret = ['$self->Layout();\n']
        if "centered" in obj.properties and obj.centered:
            ret.append('$self->Centre();\n')
        return ret


def initialize():
    klass = 'wxPanel'
    common.class_names['EditPanel'] = klass
    common.class_names['EditTopLevelPanel'] = klass
    common.register('perl', klass, PerlPanelGenerator(klass))
    common.register('perl', 'wxScrolledWindow', PerlPanelGenerator(klass))
