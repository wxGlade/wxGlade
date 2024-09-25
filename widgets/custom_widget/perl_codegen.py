"""\
Perl generator functions for CustomWidget objects

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


import common
import wcodegen
from .codegen import format_ctor_arguments


class PerlCustomWidgetGenerator(wcodegen.PerlWidgetCodeWriter):
    def get_code(self, widget):
        init = []
        id_name, id = self.codegen.generate_code_id(widget)

        parent = widget.get_parent_window2(self.codegen)
        if parent.IS_SIZER:
            parent_access = '%s->GetStaticBox()' % self.format_widget_access(parent)
        elif not parent.IS_CLASS:
            parent_access = '$self->{%s}' % parent.name
        else:
            parent_access = '$self'

        if id_name: init.append(id_name)

        arguments = format_ctor_arguments(widget.arguments, parent_access, id, widget.size)
        ctor = widget.custom_ctor.strip() or (widget.instance_class + '->new')
        init.append( '$self->{%s} = %s(%s);\n' % (widget.name, ctor, ", ".join(arguments)) )
        init += self.codegen.generate_code_common_properties(widget)

        return init, []


def initialize():
    klass = 'CustomWidget'
    common.class_names[klass] = klass
    common.register( 'perl', klass, PerlCustomWidgetGenerator(klass) )
