# codegen.py: code generator functions for wxDialog objects
#
# Copyright (c) 2002-2003 Alberto Griggio <albgrig@tiscalinet.it>
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

import common

class PythonCodeGenerator:
    def get_code(self, obj):
        return [], [], []

    def get_properties_code(self, dialog):
        prop = dialog.properties
        pygen = common.code_writers['python']
        out = []
        title = prop.get('title')
        if title: out.append('self.SetTitle(%s)\n' % pygen.quote_str(title))
        out.extend(pygen.generate_common_properties(dialog))
        return out

    def get_layout_code(self, dialog):
        return ['self.Layout()\n']

# end of class PythonCodeGenerator


class CppCodeGenerator:
    constructor = [('wxWindow*', 'parent'), ('int', 'id'),
                   ('const wxString&', 'title'),
                   ('const wxPoint&', 'pos', 'wxDefaultPosition'),
                   ('const wxSize&', 'size', 'wxDefaultSize'),
                   ('long', 'style', 'wxDEFAULT_DIALOG_STYLE')]

    def get_code(self, obj):
        return [], [], [], []
    
    def get_properties_code(self, dialog):
        """\
        generates the code for the various wxDialog specific properties.
        Returns a list of strings containing the generated code
        """
        prop = dialog.properties
        cppgen = common.code_writers['C++']
        out = []
        title = prop.get('title')
        if title: out.append('SetTitle(%s);\n' % cppgen.quote_str(title))
        out.extend(cppgen.generate_common_properties(dialog))
        return out

    def get_layout_code(self, dialog):
        return ['Layout();\n']

# end of class CppCodeGenerator


def initialize():
    cn = common.class_names
    cn['EditDialog'] = 'wxDialog'
    common.toplevels['EditDialog'] = 1
    
    pygen = common.code_writers.get('python')
    if pygen:
        pygen.add_widget_handler('wxDialog', PythonCodeGenerator())
    cppgen = common.code_writers.get('C++')
    if cppgen:
        cppgen.add_widget_handler('wxDialog', CppCodeGenerator())
