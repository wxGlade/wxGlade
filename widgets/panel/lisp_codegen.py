"""\
Lisp generator functions for wxPanel objects

@copyright: 2002-2004 D.H. aka crazyinsomniac on sourceforge.net
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common


class LispPanelGenerator:
    new_signature = [
        '$parent', '$id', '$pos', '$size', '$style', '$name'
    ]

    def get_code(self, panel):
        codegen = common.code_writers['lisp']
        prop = panel.properties
        try:
            scrollable = int(prop['scrollable'])
        except:
            scrollable = False

        id_name, id = codegen.generate_code_id(panel)
        if not panel.parent.is_toplevel:
            parent = '(slot-%s obj)' % panel.parent.name
        else:
            parent = '(slot-top-window obj)'

        if panel.is_toplevel:
            l = []
            if id_name: l.append(id_name)

            l.append('(setf (slot-%s obj) (wxPanel_Create %s %s -1 -1 -1 -1))\n'
                     % (panel.name, parent, id))
            return l, [], []

        init = []
        if id_name: init.append(id_name)

        style = prop.get("style", 'wxTAB_TRAVERSAL')
        if not( scrollable or style != 'wxTAB_TRAVERSAL' ):
            style = 'wxTAB_TRAVERSAL'
        else:
            style = codegen.cn_f(style)

        init.append('(setf (slot-%s obj) '
                    '(wxPanel_Create %s %s -1 -1 -1 -1 %s))\n'
                    % (panel.name, parent, id, style))

        props_buf = codegen.generate_common_properties(panel)
        if scrollable:
            sr = prop.get('scroll_rate', '0 0')
            sr = sr.replace(',',' ')
            props_buf.append('(wxScrolledWindow:wxScrolledWindow_SetScrollRate'
                             ' (slot-%s obj) %s)\n' % (panel.name, sr))
        return init, props_buf, []

    def get_properties_code(self, obj):
        codegen = common.code_writers['lisp']
        prop = obj.properties
        try:
            scrollable = int(prop['scrollable'])
        except:
            scrollable = False

        props_buf = codegen.generate_common_properties(obj)
        if scrollable:
            sr = prop.get('scroll_rate', '0 0')
            props_buf.append('(wxScrolledWindow:wxScrolledWindow_SetScrollRate '
                             '(slot-%s obj))\n' % sr)
        return props_buf

# end of class LispPanelGenerator


def initialize():
    klass = 'wxPanel'
    common.class_names['EditPanel'] = klass
    common.class_names['EditTopLevelPanel'] = klass
    common.toplevels['EditPanel'] = 1
    common.toplevels['EditTopLevelPanel'] = 1
    common.register('lisp', klass, LispPanelGenerator())
    common.register('lisp', 'wxScrolledWindow', LispPanelGenerator())
