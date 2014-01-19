"""\
Lisp generator functions for wxSpinButton objects

@copyright: 2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class LispSpinButtonGenerator(wcodegen.LispWidgetCodeWriter):
    tmpl = '(setf %(name)s (%(klass)s_Create %(parent)s %(id)s ' \
           '-1 -1 -1 -1 %(style)s))\n'
    default_style = 'wxSP_HORIZONTAL'

# end of class LispSpinButtonGenerator


def initialize():
    common.class_names['EditSpinButton'] = 'wxSpinButton'

    codegen = common.code_writers.get('lisp')
    if codegen:
        codegen.add_widget_handler('wxSpinButton', LispSpinButtonGenerator())
