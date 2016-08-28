"""\
Perl generator functions for CustomWidget objects

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


import common
import wcodegen
from .codegen import ArgumentsCodeHandler, format_ctor_arguments


class PerlCustomWidgetGenerator(wcodegen.PerlWidgetCodeWriter):
    def get_code(self, widget):
        init = []
        prop = widget.properties
        id_name, id = self.codegen.generate_code_id(widget)
        parent = self.format_widget_access(widget.parent)

        if id_name:
            init.append(id_name)
        arguments = format_ctor_arguments(prop.get('arguments', []), parent,
                                          id, prop.get('size', "-1, -1"))
        cust_ctor = prop.get('custom_ctor', '').strip()
        if cust_ctor:
            ctor = cust_ctor
        else:
            ctor = widget.klass + '->new'
        init.append('$self->{%s} = %s(%s);\n' %
                    (widget.name, ctor, ", ".join(arguments)))
        props_buf = self.codegen.generate_common_properties(widget)

        return init, props_buf, []

# end of class PerlCustomWidgetGenerator


def initialize():
    klass = 'CustomWidget'
    common.class_names[klass] = klass
    common.register('perl', klass, PerlCustomWidgetGenerator(klass),
                    'arguments', ArgumentsCodeHandler, klass)
