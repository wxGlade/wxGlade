"""\
Code generator functions for wxBitmapButton objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PythonBitmapButtonGenerator(wcodegen.PythonWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s(%(parent)s, %(id_number)s, %(bitmap)s%(style)s)\n'


class CppBitmapButtonGenerator(wcodegen.CppWidgetCodeWriter):
    tmpl = '%(name)s = new %(klass)s(%(parent)s, %(id_number)s, %(bitmap)s%(style)s);\n'


def xrc_code_generator(obj):
    xrcgen = common.code_writers['XRC']

    class BitmapButtonXrcObject(xrcgen.DefaultXrcObject):
        def write_property(self, name, val, output, tabs):
            if name == 'disabled_bitmap':
                name = 'disabled'

            if name in ['bitmap', 'selected', 'focus', 'disabled', 'hover']:
                prop = self._format_bitmap_property(name, val, tabs)
                if prop: output.append(prop)
                return

            xrcgen.DefaultXrcObject.write_property(self, name, val, output, tabs)
    # end of class BitmapButtonXrcObject

    return BitmapButtonXrcObject(obj)


def initialize():
    klass = 'wxBitmapButton'
    common.class_names['EditBitmapButton'] = klass
    common.register('python', klass, PythonBitmapButtonGenerator(klass))
    common.register('C++', klass, CppBitmapButtonGenerator(klass))
    common.register('XRC', klass, xrc_code_generator)
