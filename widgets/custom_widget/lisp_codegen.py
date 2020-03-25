"""\
Lisp generator functions for CustomWidget objects

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2017 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


import common
import wcodegen
from .codegen import format_ctor_arguments


class LispCustomWidgetGenerator(wcodegen.LispWidgetCodeWriter):

    def get_code(self, widget):
        init = []
        id_name, id = self.codegen.generate_code_id(widget)

        if not widget.parent_window.IS_CLASS:
            parent = '(object-%s self)' % self.codegen._format_name(widget.parent_window.name)
        else:
            parent = 'nil'

        if id_name: init.append(id_name)
        arguments = format_ctor_arguments( widget.arguments, parent, id, widget.size )
        widget_name = self.codegen._format_name(widget.name)
        init.append( '(setf %s (%s_Create %s))\n' % (widget_name, widget.instance_class, " ".join(arguments)) )
        init += self.codegen.generate_code_common_properties(widget)

        return init, []


def initialize():
    klass = 'CustomWidget'
    common.class_names[klass] = klass
    common.register('lisp', klass, LispCustomWidgetGenerator(klass) )
