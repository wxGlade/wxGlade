# sizers_codegen.py: code generation functions for the various wxSizerS
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: GPL (see license.txt)

import common

def wxBoxSizer_builder(obj):
    """\
    function used to generate the python code for wxBoxSizer objects.
    """
    orient = obj.properties.get('orient', 'wxHORIZONTAL')
    init = ['self.%s = wxBoxSizer(%s)\n' % (obj.name, orient)]
    layout = []
    if obj.is_toplevel:
        if not obj.parent.is_toplevel: parent = 'self.%s' % obj.parent.name
        else: parent = 'self'
        layout.append('%s.SetAutoLayout(1)\n' % parent)
        layout.append('%s.SetSizer(self.%s)\n' % (parent, obj.name))
        if not obj.parent.properties.has_key('size'):
            layout.append('self.%s.Fit(%s)\n' % (obj.name, parent))
    return init, [], layout

def wxStaticBoxSizer_builder(obj):
    """\
    function used to generate the python code for wxStaticBoxSizer objects.
    """
    orient = obj.properties.get('orient', 'wxHORIZONTAL')
    label = obj.properties.get('label', '')
    if not obj.parent.is_toplevel: parent = 'self.%s' % obj.parent.name
    else: parent = 'self'
    init = ['self.%s = wxStaticBoxSizer(wxStaticBox(%s, -1, "%s"), %s)\n' %
            (obj.name, parent, label.replace('"', '\"'), orient)]
    layout = []
    if obj.is_toplevel:
        layout.append('%s.SetAutoLayout(1)\n' % parent)
        layout.append('%s.SetSizer(self.%s)\n' % (parent, obj.name))
        if not obj.parent.properties.has_key('size'):
            layout.append('self.%s.Fit(%s)\n' % (obj.name, parent))
    return init, [], layout

def _GridSizers_builder(obj, klass):
    props = obj.properties
    if not obj.parent.is_toplevel: parent = 'self.%s' % obj.parent.name
    else: parent = 'self'
    rows = props.get('rows', '0')
    cols = props.get('cols', '0')
    vgap = props.get('vgap', '0')
    hgap = props.get('hgap', '0')
    init = [ 'self.%s = %s(%s, %s, %s, %s)\n' % (obj.name, klass, rows, cols,
                                                 vgap, hgap) ]
    layout = []
    if obj.is_toplevel:
        layout.append('%s.SetAutoLayout(1)\n' % parent)
        layout.append('%s.SetSizer(self.%s)\n' % (parent, obj.name))
        if not obj.parent.properties.has_key('size'):
            layout.append('self.%s.Fit(%s)\n' % (obj.name, parent))
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
    return _GridSizers_builder(obj, 'wxFlexGridSizer')


def initialize():
    cn = common.class_names
    cn['EditBoxSizer'] = 'wxBoxSizer'
    cn['EditStaticBoxSizer'] = 'wxStaticBoxSizer'
    cn['EditGridSizer'] = 'wxGridSizer'
    cn['EditFlexGridSizer'] = 'wxFlexGridSizer'

    # python code generation functions
    pygen = common.code_writers.get("python")
    if pygen:
        awh = pygen.add_widget_handler
        awh('wxBoxSizer', wxBoxSizer_builder)
        awh('wxStaticBoxSizer', wxStaticBoxSizer_builder)
        awh('wxGridSizer', wxGridSizer_builder)
        awh('wxFlexGridSizer', wxFlexGridSizer_builder)
