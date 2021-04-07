"""\
Lisp generator functions for wxPanel objects

@copyright: 2002-2004 D.H. aka crazyinsomniac on sourceforge.net
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2018-2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class LispPanelGenerator(wcodegen.LispWidgetCodeWriter):

    def get_code(self, panel):
        prop = panel.properties
        scrollable = panel.scrollable

        id_name, id = self.codegen.generate_code_id(panel)
        parent = self.format_widget_access(panel.parent_window)
        panel_name = self.codegen._format_name(panel.name)

        if panel.IS_CLASS:
            l = []
            if id_name: l.append(id_name)

            panel_name = panel.name
            l.append( '(setf (slot-%s obj) (wxPanel_Create %s %s -1 -1 -1 -1))\n' % (panel_name, parent, id) )
            return l, []

        init = []
        if id_name: init.append(id_name)

        style = panel.properties["style"].get_string_value() or 'wxTAB_TRAVERSAL'
        if not (scrollable or style != 'wxTAB_TRAVERSAL'):
            style = 'wxTAB_TRAVERSAL'
        else:
            style = self.codegen.cn_f(style)

        init.append( '(setf (slot-%s obj) (wxPanel_Create %s %s -1 -1 -1 -1 %s))\n' % (panel_name, parent, id, style) )

        init += self.codegen.generate_code_common_properties(panel)
        if scrollable and panel.check_prop("scroll_rate"):
            sr = panel.scroll_rate.replace(',', ' ')
            init.append( '(wxScrolledWindow:wxScrolledWindow_SetScrollRate (slot-%s obj) %s)\n'% (panel_name, sr) )
        return init, []

    def get_properties_code(self, obj):
        props_buf = self.codegen.generate_code_common_properties(obj)
        if obj.scrollable and obj.check_prop("scroll_rate"):
            sr = obj.scroll_rate.replace(',', ' ')
            props_buf.append('(wxScrolledWindow:wxScrolledWindow_SetScrollRate (slot-%s obj))\n' % sr)
        return props_buf

    def get_layout_code(self, obj):
        ret = ['(wxPanel_layout (slot-%s self))\n' % self.codegen._format_name(obj.name)]
        if "centered" in obj.properties and obj.centered:
            ret.append('(wxPanel_Centre (slot-top-window obj) wxBOTH)\n')
        return ret


def initialize():
    klass = 'wxPanel'
    common.class_names['EditPanel'] = klass
    common.class_names['EditTopLevelPanel'] = klass
    common.register('lisp', klass, LispPanelGenerator(klass))
    common.register('lisp', 'wxScrolledWindow', LispPanelGenerator(klass))
