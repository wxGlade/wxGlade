# sizers_codegen.py: code generation functions for the various wxSizerS
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: Python 2.2 license (see license.txt)

import common

def wxBoxSizer_builder(obj):
    """\
    function used to generate the python code for wxBoxSizer objects.
    """
    orient = obj.properties.get('orient', 'wxHORIZONTAL')
    init = ['%s = wxBoxSizer(%s)\n' % (obj.name, orient)]
    layout = []
    if obj.is_toplevel:
        if not obj.parent.is_toplevel: parent = 'self.%s' % obj.parent.name
        else: parent = 'self'
        layout.append('%s.SetAutoLayout(1)\n' % parent)
        layout.append('%s.SetSizer(%s)\n' % (parent, obj.name))
        if not obj.parent.properties.has_key('size'):
            layout.append('%s.Fit(%s)\n' % (obj.name, parent))
    return init, [], layout

def wxStaticBoxSizer_builder(obj):
    """\
    function used to generate the python code for wxStaticBoxSizer objects.
    """
    orient = obj.properties.get('orient', 'wxHORIZONTAL')
    label = obj.properties.get('label', '')
    if not obj.parent.is_toplevel: parent = 'self.%s' % obj.parent.name
    else: parent = 'self'
    init = ['%s = wxStaticBoxSizer(wxStaticBox(%s, -1, "%s"), %s)\n' %
            (obj.name, parent, label.replace('"', r'\"'), orient)]
    layout = []
    if obj.is_toplevel:
        layout.append('%s.SetAutoLayout(1)\n' % parent)
        layout.append('%s.SetSizer(%s)\n' % (parent, obj.name))
        if not obj.parent.properties.has_key('size'):
            layout.append('%s.Fit(%s)\n' % (obj.name, parent))
    return init, [], layout

def _GridSizers_builder(obj, klass):
    props = obj.properties
    if not obj.parent.is_toplevel: parent = 'self.%s' % obj.parent.name
    else: parent = 'self'
    rows = props.get('rows', '0')
    cols = props.get('cols', '0')
    vgap = props.get('vgap', '0')
    hgap = props.get('hgap', '0')
    init = [ '%s = %s(%s, %s, %s, %s)\n' % (obj.name, klass, rows, cols,
                                                 vgap, hgap) ]
    layout = []
    if obj.is_toplevel:
        layout.append('%s.SetAutoLayout(1)\n' % parent)
        layout.append('%s.SetSizer(%s)\n' % (parent, obj.name))
        if not obj.parent.properties.has_key('size'):
            layout.append('%s.Fit(%s)\n' % (obj.name, parent))
    return init, [], layout   

def wxGridSizer_builder(obj):
    """\
    function used to generate the python code for wxGridSizer objects.
    """
    return _GridSizers_builder(obj, 'wxGridSizer')

def wxFlexGridSizer_builder(obj):
    """\
    function used to generate the python code for wxFlexGridSizer objects.
    """
    init, p, layout = _GridSizers_builder(obj, 'wxFlexGridSizer')
    props = obj.properties
    if props.has_key('growable_rows'):
        for r in props['growable_rows'].split(','):
            layout.append('%s.AddGrowableRow(%s)\n' % (obj.name, r.strip()))
    if props.has_key('growable_cols'):
        for r in props['growable_cols'].split(','):
            layout.append('%s.AddGrowableCol(%s)\n' % (obj.name, r.strip()))
    return init, p, layout


def cpp_wxBoxSizer_builder(obj):
    """\
    function used to generate the C++ code for wxBoxSizer objects.
    """
    orient = obj.properties.get('orient', 'wxHORIZONTAL')
    init = ['wxBoxSizer* %s = new wxBoxSizer(%s);\n' % (obj.name, orient)]
    layout = []
    if obj.is_toplevel:
        if not obj.parent.is_toplevel: parent = '%s->' % obj.parent.name
        else: parent = ''
        layout.append('%sSetAutoLayout(true);\n' % parent)
        layout.append('%sSetSizer(%s);\n' % (parent, obj.name))
        if not obj.parent.properties.has_key('size'):
            if not obj.parent.is_toplevel: parent = '%s' % obj.parent.name
            else: parent = 'this'
            layout.append('%s->Fit(%s);\n' % (obj.name, parent))
    return init, [], [], layout

def cpp_wxStaticBoxSizer_builder(obj):
    """\
    function used to generate the C++ code for wxStaticBoxSizer objects.
    """
    orient = obj.properties.get('orient', 'wxHORIZONTAL')
    label = obj.properties.get('label', '')
    if not obj.parent.is_toplevel: parent = '%s' % obj.parent.name
    else: parent = 'this'
    init = ['wxStaticBoxSizer* %s = new wxStaticBoxSizer('
            'new wxStaticBox(%s, -1, "%s"), %s);\n' %
            (obj.name, parent, label.replace('"', r'\"'), orient)]
    layout = []
    if obj.is_toplevel:
        if not obj.parent.is_toplevel: parent = '%s->' % obj.parent.name
        else: parent = ''
        layout.append('%sSetAutoLayout(true);\n' % parent)
        layout.append('%sSetSizer(%s);\n' % (parent, obj.name))
        if not obj.parent.properties.has_key('size'):
            if not obj.parent.is_toplevel: parent = '%s' % obj.parent.name
            else: parent = 'this'
            layout.append('%s->Fit(%s);\n' % (obj.name, parent))
    return init, [], [], layout

def _cpp_GridSizers_builder(obj, klass):
    props = obj.properties
    rows = props.get('rows', '0')
    cols = props.get('cols', '0')
    vgap = props.get('vgap', '0')
    hgap = props.get('hgap', '0')
    init = [ '%s* %s = new %s(%s, %s, %s, %s);\n' % \
             (klass, obj.name, klass, rows, cols, vgap, hgap) ]
    layout = []
    if obj.is_toplevel:
        if not obj.parent.is_toplevel: parent = '%s->' % obj.parent.name
        else: parent = ''
        layout.append('%sSetAutoLayout(true);\n' % parent)
        layout.append('%sSetSizer(%s);\n' % (parent, obj.name))
        if not obj.parent.properties.has_key('size'):
            if not obj.parent.is_toplevel: parent = '%s' % obj.parent.name
            else: parent = 'this'
            layout.append('%s->Fit(%s);\n' % (obj.name, parent))
    return init, [], [], layout   

def cpp_wxGridSizer_builder(obj):
    """\
    function used to generate the C++ code for wxGridSizer objects.
    """
    return _cpp_GridSizers_builder(obj, 'wxGridSizer')

def cpp_wxFlexGridSizer_builder(obj):
    """\
    function used to generate the C++ code for wxFlexGridSizer objects.
    """
    init, ids, p, layout = _cpp_GridSizers_builder(obj, 'wxFlexGridSizer')
    props = obj.properties
    if props.has_key('growable_rows'):
        for r in props['growable_rows'].split(','):
            layout.append('%s->AddGrowableRow(%s);\n' % (obj.name, r.strip()))
    if props.has_key('growable_cols'):
        for r in props['growable_cols'].split(','):
            layout.append('%s->AddGrowableCol(%s);\n' % (obj.name, r.strip()))
    return init, ids, p, layout


def xrc_wxFlexGridSizer_builder(obj):
    xrcgen = common.code_writers['XRC']
    class FlexGridSizerXrcObject(xrcgen.DefaultXrcObject):
        def write_property(self, name, val, outfile, tabs):
            if val and name in ('growable_rows', 'growable_cols'):
                if name == 'growable_rows': name2 = 'growablerows'
                else: name2 = 'growablecols'
                for v in val.split(','):
                    outfile.write('    '*tabs + '<%s>%s</%s>\n' %
                                  (name2, v.strip(), name2))
            else:
                xrcgen.DefaultXrcObject.write_property(self, name, val,
                                                       outfile, tabs)

    # end of class FlexGridSizerXrcObject

    return FlexGridSizerXrcObject(obj)
    


def initialize():
    cn = common.class_names
    cn['EditBoxSizer'] = 'wxBoxSizer'
    cn['EditStaticBoxSizer'] = 'wxStaticBoxSizer'
    cn['EditGridSizer'] = 'wxGridSizer'
    cn['EditFlexGridSizer'] = 'wxFlexGridSizer'

    pygen = common.code_writers.get("python")
    if pygen:
        awh = pygen.add_widget_handler
        awh('wxBoxSizer', wxBoxSizer_builder)
        awh('wxStaticBoxSizer', wxStaticBoxSizer_builder)
        awh('wxGridSizer', wxGridSizer_builder)
        awh('wxFlexGridSizer', wxFlexGridSizer_builder)
    cppgen = common.code_writers.get("C++")
    if cppgen:
        awh = cppgen.add_widget_handler
        awh('wxBoxSizer', cpp_wxBoxSizer_builder)
        awh('wxStaticBoxSizer', cpp_wxStaticBoxSizer_builder)
        awh('wxGridSizer', cpp_wxGridSizer_builder)
        awh('wxFlexGridSizer', cpp_wxFlexGridSizer_builder)
    xrcgen = common.code_writers.get("XRC")
    if xrcgen:
        xrcgen.add_widget_handler('wxFlexGridSizer',
                                  xrc_wxFlexGridSizer_builder)
