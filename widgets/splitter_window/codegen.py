# codegen.py: code generator functions for wxSplitterWindow objects
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: GPL (see license.txt)

import common

def python_code_generator(window):
    """\
    generates the python code for wxSplitterWindow
    """
    pygen = common.code_writers['python']
    prop = window.properties
    id_name, id = pygen.generate_code_id(window)
    if not window.parent.is_toplevel: parent = 'self.%s' % window.parent.name
    else: parent = 'self'
    if window.is_toplevel:
        l = ['self.%s = %s(%s, %s)\n' % (window.name, window.klass, parent,id)]
        if id_name: l.append(id_name)
        return l, [], []
    style = prop.get("style")
    if style and style != 'wxSP_3D': style = ", style=%s" % style
    else: style = ''
    init = ['self.%s = wxSplitterWindow(%s, %s%s)\n' %
            (window.name, parent, id, style) ]
    if id_name: init.append(id_name)

    props_buf = pygen.generate_common_properties(window)
    win_1 = prop.get('window_1')
    win_2 = prop.get('window_2')
    orientation = prop.get('orientation', 'wxSPLIT_VERTICAL')
    if win_1 and win_2:
        if orientation == 'wxSPLIT_VERTICAL': f_name = 'SplitVertically'
        else: f_name = 'SplitHorizontally'
        props_buf.append('self.%s.%s(self.%s, self.%s)\n' % \
                         (window.name, f_name, win_1, win_2))
    else:
        def add_sub(win):
            props_buf.append('self.%s.SetSplitMode(%s)\n' % (window.name,
                                                             orientation))
            props_buf.append('self.%s.Initialize(self.%s)\n' % \
                             (window.name, win))
        if win_1: add_sub(win_1)
        elif win_2: add_sub(win_2)

    sash_pos = prop.get('sash_pos')
    if sash_pos:
        props_buf.append('self.%s.SetSashPosition(%s)\n' % (window.name,
                                                            sash_pos))
    return init, props_buf, []


def python_generate_properties(obj):
    prop = obj.properties
    pygen = common.code_writers['python']
    win_1 = prop.get('window_1')
    win_2 = prop.get('window_2')
    orientation = prop.get('orientation', 'wxSPLIT_VERTICAL')
    props_buf = []
    if win_1 and win_2:
        if orientation == 'wxSPLIT_VERTICAL': f_name = 'SplitVertically'
        else: f_name = 'SplitHorizontally'
        props_buf.append('self.%s(self.%s, self.%s)\n' %
                         (f_name, win_1, win_2))
    else:
        def add_sub(win):
            props_buf.append('self.SetSplitMode(%s)\n' % orientation)
            props_buf.append('self.Initialize(self.%s)\n' % win)
        if win_1: add_sub(win_1)
        elif win_2: add_sub(win_2)
    sash_pos = prop.get('sash_pos')
    if sash_pos:
        props_buf.append('self.SetSashPosition(%s)\n' % sash_pos)
    props_buf.extend(pygen.generate_common_properties(obj))
    return props_buf    


def initialize():
    common.class_names['EditSplitterWindow'] = 'wxSplitterWindow'
    common.class_names['SplitterPane'] = 'wxPanel'

    pygen = common.code_writers.get('python')
    if pygen:
        pygen.add_widget_handler('wxSplitterWindow', python_code_generator,
                                 python_generate_properties)
    xrcgen = common.code_writers.get('XRC')
    if xrcgen:
        xrcgen.add_widget_handler('wxSplitterWindow',
                                  xrcgen.NotImplementedXrcObject)
        
    
