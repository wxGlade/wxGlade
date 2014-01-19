"""\
Code generator functions for wxStaticText objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PythonStaticTextGenerator(wcodegen.PythonWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s(%(parent)s, %(id)s, ' \
           '%(label)s%(style)s)\n'

# end of class PythonStaticTextGenerator


class CppStaticTextGenerator(wcodegen.CppWidgetCodeWriter):
    tmpl = '%(name)s = new %(klass)s(%(parent)s, %(id)s, ' \
           '%(label)s%(style)s);\n'

# end of class CppStaticTextGenerator


def xrc_code_generator(obj):
    xrcgen = common.code_writers['XRC']

    class XrcCodeGenerator(xrcgen.DefaultXrcObject):
        def write(self, *args, **kwds):
            try:
                del self.properties['attribute']
            except KeyError:
                pass
            xrcgen.DefaultXrcObject.write(self, *args, **kwds)

    return XrcCodeGenerator(obj)


def initialize():
    common.class_names['EditStaticText'] = 'wxStaticText'

    pygen = common.code_writers.get("python")
    if pygen:
        pygen.add_widget_handler('wxStaticText', PythonStaticTextGenerator())
    cppgen = common.code_writers.get('C++')
    if cppgen:
        cppgen.add_widget_handler('wxStaticText', CppStaticTextGenerator())
    xrcgen = common.code_writers.get('XRC')
    if xrcgen:
        xrcgen.add_widget_handler('wxStaticText', xrc_code_generator)
