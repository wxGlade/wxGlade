# codegen.py: code generator functions for wxPanel objects
# $Id: codegen.py,v 1.11 2003/05/13 10:05:11 agriggio Exp $
#
# Copyright (c) 2002-2003 Alberto Griggio <albgrig@tiscalinet.it>
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

import common


class PythonCodeGenerator:
    def get_code(self, panel):
        pygen = common.code_writers['python']
        prop = panel.properties
        id_name, id = pygen.generate_code_id(panel)
        if not panel.parent.is_toplevel: parent = 'self.%s' % panel.parent.name
        else: parent = 'self'
        if panel.is_toplevel:
            l = []
            if id_name: l.append(id_name)
            l.append('self.%s = %s(%s, %s)\n' %
                     (panel.name, panel.klass, parent, id))
            return l, [], []
        init = []
        if id_name: init.append(id_name)
        style = prop.get("style", 'wxTAB_TRAVERSAL')
        if style != 'wxTAB_TRAVERSAL': style = ", style=%s" % style
        else: style = ''
        init.append('self.%s = wxPanel(%s, %s%s)\n' % \
                    (panel.name, parent, id, style))
        props_buf = pygen.generate_common_properties(panel)
        return init, props_buf, []

# end of class PythonCodeGenerator


class CppCodeGenerator:
    constructor = [('wxWindow*', 'parent'), ('int', 'id'),
                   ('const wxPoint&', 'pos', 'wxDefaultPosition'),
                   ('const wxSize&', 'size', 'wxDefaultSize'),
                   ('long', 'style', '0')]

    def get_code(self, panel):
        """\
        generates the C++ code for wxPanel objects
        """
        cppgen = common.code_writers['C++']
        prop = panel.properties
        id_name, id = cppgen.generate_code_id(panel)
        if id_name: ids = [ id_name ]
        else: ids = []
        if not panel.parent.is_toplevel: parent = '%s' % panel.parent.name
        else: parent = 'this'
        if panel.is_toplevel:
            l = ['%s = new %s(%s, %s);\n' %
                 (panel.name, panel.klass, parent, id)]
            return l, ids, [], []
        extra = ''
        style = prop.get("style", 'wxTAB_TRAVERSAL')
        if style != 'wxTAB_TRAVERSAL':
            extra = ', wxDefaultPosition, wxDefaultSize, %s' % style
        init = ['%s = new wxPanel(%s, %s%s);\n' %
                (panel.name, parent, id, extra) ]
        props_buf = cppgen.generate_common_properties(panel)
        return init, ids, props_buf, []

# end of class CppCodeGenerator


def initialize():
    common.class_names['EditPanel'] = 'wxPanel'
    common.class_names['EditTopLevelPanel'] = 'wxPanel'
    common.toplevels['EditPanel'] = 1
    common.toplevels['EditTopLevelPanel'] = 1

    # python code generation functions
    pygen = common.code_writers.get('python')
    if pygen:
        pygen.add_widget_handler('wxPanel', PythonCodeGenerator())
    cppgen = common.code_writers.get('C++')
    if cppgen:
        cppgen.add_widget_handler('wxPanel', CppCodeGenerator())
