# codegen.py: code generator functions for spacers
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

import common

def python_code_generator(spacer):
    """\
    generates the python code for a spacer
    """
    prop = spacer.properties
    width = prop.get('width', '0')
    height = prop.get('height', '0')
    # we must use the hack in pygen.add_sizeritem (see py_codegen.py)
    spacer.name = '%s, %s' % (width, height)
    return [], [], []

def cpp_code_generator(spacer):
    """\
    generates the python code for a spacer
    """
    prop = spacer.properties
    width = prop.get('width', '0')
    height = prop.get('height', '0')
    # we must use the hack in pygen.add_sizeritem (see py_codegen.py)
    spacer.name = '%s, %s' % (width, height)
    return [], [], [], []

def initialize():
    common.class_names['EditSpacer'] = 'spacer'

    # python code generation functions
    pygen = common.code_writers.get('python')
    if pygen:
        pygen.add_widget_handler('spacer', python_code_generator)
    cppgen = common.code_writers.get('C++')
    if cppgen:
        cppgen.add_widget_handler('spacer', cpp_code_generator)
    
