# codegen.py: code generator functions for wxPanel objects
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: GPL (see license.txt)

import common

def python_code_generator(panel):
    """\
    generates the python code for wxPanel objects
    """
    pygen = common.code_writers['python']
    prop = panel.properties
    id_name, id = pygen.generate_code_id(panel)
    if not panel.parent.is_toplevel: parent = 'self.%s' % panel.parent.name
    else: parent = 'self'
    if panel.is_toplevel:
        l = ['self.%s = %s(%s, %s)\n' % (panel.name, panel.klass, parent, id)]
        if id_name: l.append(id_name)
        return l, [], []
    init = ['self.%s = wxPanel(%s, %s)\n' % (panel.name, parent, id) ]
    if id_name: init.append(id_name)
    props_buf = pygen.generate_common_properties(panel)
    return init, props_buf, []


def initialize():
    common.class_names['EditPanel'] = 'wxPanel'

    # python code generation functions
    pygen = common.code_writers.get('python')
    if pygen:
        pygen.add_widget_handler('wxPanel', python_code_generator)
