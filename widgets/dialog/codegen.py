# codegen.py: code generator functions for wxDialog objects
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: GPL (see license.txt)

import common

def python_generate_properties(dialog):
    """\
    generates the code for the various wxDialog specific properties.
    Returns a list of strings containing the generated code
    """
    prop = dialog.properties
    pygen = common.code_writers['python']
    out = []
    title = prop.get('title')
    if title: out.append('self.SetTitle("%s")\n' % title)
    out.extend(pygen.generate_common_properties(dialog))
    return out

def initialize():
    cn = common.class_names
    cn['EditDialog'] = 'wxDialog'
    
    pygen = common.code_writers.get('python')
    if pygen:
        awh = pygen.add_widget_handler
        awh('wxDialog', lambda o: None, python_generate_properties)
