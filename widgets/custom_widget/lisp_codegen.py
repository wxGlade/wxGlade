"""\
Lisp generator functions for CustomWidget objects

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


import common
import wcodegen
from .codegen import ArgumentsCodeHandler, format_ctor_arguments


class LispCustomWidgetGenerator(wcodegen.LispWidgetCodeWriter):

    def get_code(self, widget):
        init = []
        prop = widget.properties
        id_name, id = self.codegen.generate_code_id(widget)

        if not widget.parent.is_toplevel:
            parent = '(object-%s self)' % widget.parent.name
        else:
            parent = 'nil'

        if id_name:
            init.append(id_name)
        arguments = format_ctor_arguments( prop.get('arguments', []), parent, id, prop.get('size', "-1, -1") )
        init.append( '(setf %s (%s_Create %s))\n' % (widget.name, widget.klass, " ".join(arguments)) )
        props_buf = self.codegen.generate_common_properties(widget)

        return init, props_buf, []

# end of class PerlCodeGenerator


def initialize():
    klass = 'CustomWidget'
    common.class_names[klass] = klass
    common.register('lisp', klass, LispCustomWidgetGenerator(klass),
                    'arguments', ArgumentsCodeHandler, klass)
