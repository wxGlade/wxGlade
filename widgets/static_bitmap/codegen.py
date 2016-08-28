"""\
Code generator functions for wxStaticBitmap objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PythonStaticBitmapGenerator(wcodegen.PythonWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s(%(parent)s, %(id)s, %(bitmap)s%(style)s)\n'
    tmpl_SetBestSize = ''



class CppStaticBitmapGenerator(wcodegen.CppWidgetCodeWriter):
    tmpl = '%(name)s = new %(klass)s(%(parent)s, %(id)s, ' \
           '%(bitmap)s%(style)s);\n'
    tmpl_SetBestSize = ''



def xrc_code_generator(obj):
    xrcgen = common.code_writers['XRC']

    class StaticBitmapXrcObject(xrcgen.DefaultXrcObject):
        def write(self, *args, **kwds):
            try:
                del self.properties['attribute']
            except KeyError:
                pass
            xrcgen.DefaultXrcObject.write(self, *args, **kwds)

    return StaticBitmapXrcObject(obj)


def initialize():
    klass = 'wxStaticBitmap'
    common.class_names['EditStaticBitmap'] = klass
    common.register('python', klass, PythonStaticBitmapGenerator(klass))
    common.register('C++', klass, CppStaticBitmapGenerator(klass))
    common.register('XRC', klass, xrc_code_generator)
