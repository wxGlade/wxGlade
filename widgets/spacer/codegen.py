"""\
Code generator functions for spacers

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PythonSpacerGenerator(wcodegen.PythonWidgetCodeWriter):

    # spacers are generally handled by a hack:
    # The the implementations of add_sizeritem() contains more details.
    # The code generation code is already implemented in base class.
    pass

# end of class PythonSpacerGenerator


class CppSpacerGenerator(wcodegen.CppWidgetCodeWriter):

    # spacers are generally handled by a hack:
    # The the implementations of add_sizeritem() contains more details.
    # The code generation code is already implemented in base class.
    pass

# end of class CppSpacerGenerator


def initialize():
    common.class_names['EditSpacer'] = 'spacer'

    # python code generation functions
    pygen = common.code_writers.get('python')
    if pygen:
        pygen.add_widget_handler('spacer', PythonSpacerGenerator())
    cppgen = common.code_writers.get('C++')
    if cppgen:
        cppgen.add_widget_handler('spacer', CppSpacerGenerator())
