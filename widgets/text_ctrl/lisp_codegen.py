"""\
Lisp generator functions for wxTextCtrl objects

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class LispTextCtrlGenerator(wcodegen.LispWidgetCodeWriter):
    tmpl = '(setf %(name)s (%(klass)s_Create %(parent)s %(id)s ' \
           '%(value)s -1 -1 -1 -1 %(style)s))\n'

# end of class LispTextCtrlGenerator


def initialize():
    common.class_names['EditTextCtrl'] = 'wxTextCtrl'

    codegen = common.code_writers.get('lisp')
    if codegen:
        codegen.add_widget_handler('wxTextCtrl', LispTextCtrlGenerator())
