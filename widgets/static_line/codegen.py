"""\
Code generator functions for wxStaticLine objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PythonStaticLineGenerator(wcodegen.PythonWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s(%(parent)s, %(id)s%(style)s)\n'
    default_style = 'wxLI_HORIZONTAL'

# end of class PythonStaticLineGenerator


class CppStaticLineGenerator(wcodegen.CppWidgetCodeWriter):
    extra_headers = ['<wx/statline.h>']
    tmpl = '%(name)s = new %(klass)s(%(parent)s, %(id)s%(style)s);\n'
    default_style = 'wxLI_HORIZONTAL'

# end of class CppStaticLineGenerator


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
    common.class_names['EditStaticLine'] = 'wxStaticLine'

    pygen = common.code_writers.get("python")
    if pygen:
        pygen.add_widget_handler('wxStaticLine', PythonStaticLineGenerator())
    cppgen = common.code_writers.get("C++")
    if cppgen:
        cppgen.add_widget_handler('wxStaticLine', CppStaticLineGenerator())
    xrcgen = common.code_writers.get('XRC')
    if xrcgen:
        xrcgen.add_widget_handler('wxStaticLine', xrc_code_generator)
