"""\
Perl generator functions for wxNotebook objects

@copyright: 2002-2004 D.H. aka crazyinsomniac on sourceforge.net
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2017 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PerlNotebookGenerator(wcodegen.PerlWidgetCodeWriter):
    new_signature = ['$parent', '$id', '$pos', '$size', '$style', '$name']

    def get_code(self, window):
        self._reset_vars()
        wcodegen.PerlWidgetCodeWriter._prepare_tmpl_content(self, window)

        id_name, id = self.codegen.generate_code_id(window)

        parent = self.format_widget_access(window.parent_window)
        klass = window.get_instantiation_class(self.cn, self.cn_class)

        if window.IS_CLASS:
            l = []
            if id_name: l.append(id_name)
            l.append( '$self->{%s} = %s->new(%s, %s);\n' % ( window.name, klass, parent, id) )
            return l, []
        init = []
        if id_name: init.append(id_name)
        init.append( '$self->{%s} = %s->new(%s, %s%s);\n' % ( window.name, klass, parent, id, self.tmpl_dict['style']) )

        init += self.codegen.generate_code_common_properties(window)
        return init, []

    def get_layout_code(self, obj):
        return self.codegen.generate_code_common_properties(obj)

    def get_code_per_child(self, obj, child):
        i = obj.children.index(child)
        label = self.codegen.quote_str( obj.tabs[i][0] )
        notebook = self.format_widget_access(obj)  # '$self' or '$self->{%s}'%obj.name
        return ['%s->AddPage($self->{%s}, %s);\n' % (notebook, child.name, label)]


def initialize():
    klass = 'wxNotebook'
    common.class_names['EditNotebook'] = klass
    common.class_names['NotebookPane'] = 'wxPanel'
    common.register( 'perl', klass, PerlNotebookGenerator(klass) )
