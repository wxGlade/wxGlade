# codegen.py: code generator functions for CustomWidget objects
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: Python 2.2 license (see license.txt)

import common

class ArgumentsCodeHandler:
    def __init__(self):
        self.arguments = []
        self.curr_arg = []

    def start_elem(self, name, attrs):
        pass

    def end_elem(self, name, code_obj):
        if name == 'arguments':
            code_obj.properties['arguments'] = self.arguments
            return True
        elif name == 'argument':
            tab_name = "".join(self.curr_arg)
            self.arguments.append(tab_name)
            self.curr_arg = []
        return False

    def char_data(self, data):
        self.curr_arg.append(data)

# end of class ArgumentsCodeHandler


def _fix_arguments(arguments, parent, id):
    for i in range(len(arguments)):
        if arguments[i] == '$parent': arguments[i] = parent
        elif arguments[i] == '$id': arguments[i] = id
    return arguments

def python_code_generator(widget):
    """\
    generates the python code for CustomWidget objects
    """
    pygen = common.code_writers['python']
    prop = widget.properties
    id_name, id = pygen.generate_code_id(widget)
    if not widget.parent.is_toplevel: parent = 'self.%s' % widget.parent.name
    else: parent = 'self'
    init = []
    if id_name: init.append(id_name)
    arguments = _fix_arguments(prop.get('arguments', []), parent, id)
    init.append('self.%s = %s(%s)\n' % (widget.name, widget.klass,
                                        ", ".join(arguments)))
    props_buf = pygen.generate_common_properties(widget)
    return init, props_buf, []


def cpp_code_generator(widget):
    """\
    generates the python code for wxPanel objects
    """
    cppgen = common.code_writers['C++']
    prop = widget.properties
    id_name, id = cppgen.generate_code_id(widget)
    if id_name: ids = [ id_name ]
    else: ids = []
    if not widget.parent.is_toplevel: parent = '%s' % widget.parent.name
    else: parent = 'this'
    arguments = _fix_arguments(prop.get('arguments', []), parent, id)
    init = ['%s = new %s(%s);\n' % (widget.name, widget.klass,
                                    ", ".join(arguments)) ]
    props_buf = cppgen.generate_common_properties(widget)
    return init, ids, props_buf, []


def initialize():
    common.class_names['CustomWidget'] = 'CustomWidget'

    # python code generation functions
    pygen = common.code_writers.get('python')
    if pygen:
        pygen.add_widget_handler('CustomWidget', python_code_generator)
        pygen.add_property_handler('arguments', ArgumentsCodeHandler,
                                   'CustomWidget')
    cppgen = common.code_writers.get('C++')
    if cppgen:
        cppgen.add_widget_handler('CustomWidget', cpp_code_generator)
        cppgen.add_property_handler('arguments', ArgumentsCodeHandler,
                                    'CustomWidget')
    xrcgen = common.code_writers.get('XRC')
    if xrcgen:
        xrcgen.add_widget_handler('CustomWidget',
                                  xrcgen.NotImplementedXrcObject)
