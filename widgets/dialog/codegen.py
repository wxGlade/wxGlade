# codegen.py: code generator functions for wxDialog objects
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

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
    if title: out.append('self.SetTitle("%s")\n' % title.replace('"', r'\"'))
    out.extend(pygen.generate_common_properties(dialog))
    return out

def cpp_generate_properties(dialog):
    """\
    generates the code for the various wxDialog specific properties.
    Returns a list of strings containing the generated code
    """
    prop = dialog.properties
    cppgen = common.code_writers['C++']
    out = []
    title = prop.get('title')
    if title: out.append('SetTitle("%s");\n' % title.replace('"', r'\"'))
    out.extend(cppgen.generate_common_properties(dialog))
    return out

def initialize():
    cn = common.class_names
    cn['EditDialog'] = 'wxDialog'
    common.toplevels['EditDialog'] = 1
    
    pygen = common.code_writers.get('python')
    if pygen:
        awh = pygen.add_widget_handler
        awh('wxDialog', lambda o: None, python_generate_properties)

    cppgen = common.code_writers.get('C++')
    if pygen:
        constructor = [('wxWindow*', 'parent'), ('int', 'id'),
                       ('const wxString&', 'title'),
                       ('const wxPoint&', 'pos', 'wxDefaultPosition'),
                       ('const wxSize&', 'size', 'wxDefaultSize'),
                       ('long', 'style', 'wxDEFAULT_DIALOG_STYLE')]
        awh = cppgen.add_widget_handler
        awh('wxDialog', lambda o: None, constructor, cpp_generate_properties)
