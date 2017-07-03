"""\
Code generator functions for wxStaticLine objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PythonStaticLineGenerator(wcodegen.PythonWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s(%(parent)s, %(id)s%(style)s)\n'



class CppStaticLineGenerator(wcodegen.CppWidgetCodeWriter):
    import_modules = ['<wx/statline.h>']
    tmpl = '%(name)s = new %(klass)s(%(parent)s, %(id)s%(style)s);\n'



def xrc_code_generator(obj):
    xrcgen = common.code_writers['XRC']

    class XrcCodeGenerator(xrcgen.DefaultXrcObject):
        def write(self, out_file, ntabs):
            style = self.widget.properties["style"].get_string_value()
            properties = {"attribute":None, "style":style}
            xrcgen.DefaultXrcObject.write(self, out_file, ntabs, properties)

    return XrcCodeGenerator(obj)


def initialize():
    klass = 'wxStaticLine'
    common.class_names['EditStaticLine'] = klass
    common.register('python', klass, PythonStaticLineGenerator(klass))
    common.register('C++',    klass, CppStaticLineGenerator(klass))
    common.register('XRC',    klass, xrc_code_generator)
