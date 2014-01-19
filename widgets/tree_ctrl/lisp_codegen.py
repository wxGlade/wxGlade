"""\
Lisp generator functions for wxTreeCtrl objects

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class LispTreeCtrlGenerator(wcodegen.LispWidgetCodeWriter):
    tmpl = '(setf %(name)s (%(klass)s_Create %(parent)s %(id)s ' \
           '-1 -1 -1 -1 %(style)s))\n'
    default_style = 'wxTR_HAS_BUTTONS'

# end of class LispTreeCtrlGenerator


def initialize():
    common.class_names['EditTreeCtrl'] = 'wxTreeCtrl'
    codegen = common.code_writers.get('lisp')

    if codegen:
        codegen.add_widget_handler('wxTreeCtrl', LispTreeCtrlGenerator())
