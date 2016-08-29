"""\
Code generator functions for spacers

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PythonSpacerGenerator(wcodegen.PythonWidgetCodeWriter):
    # spacers are generally handled by a hack:
    # The the implementations of add_sizeritem() contains more details.
    # The code generation code is already implemented in base class.
    pass



class CppSpacerGenerator(wcodegen.CppWidgetCodeWriter):

    # spacers are generally handled by a hack:
    # The the implementations of add_sizeritem() contains more details.
    # The code generation code is already implemented in base class.
    pass



def initialize():
    klass = 'spacer'
    common.class_names['EditSpacer'] = klass
    common.register('python', klass, PythonSpacerGenerator(klass))
    common.register('C++',    klass, CppSpacerGenerator(klass))
