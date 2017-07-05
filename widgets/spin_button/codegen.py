"""\
Code generator functions for wxSpinButton objects

@copyright: 2004 D.H. aka crazyinsomniac at users.sourceforge.net
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PythonSpinButtonGenerator(wcodegen.PythonWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s(%(parent)s, %(id)s %(style)s)\n'


class CppSpinButtonGenerator(wcodegen.CppWidgetCodeWriter):
    import_modules = ['<wx/spinbutt.h>']
    tmpl = '%(name)s = new %(klass)s(%(parent)s, %(id)s%(style)s);\n'


def xrc_code_generator(obj):
    xrcgen = common.code_writers['XRC']

    class SpinButtonXrcObject(xrcgen.DefaultXrcObject):
        def write_property(self, name, val, output, tabs):
            if name == 'range':
                try:
                    min, max = val.split(',')
                except ValueError:
                    pass
                else:
                    tab_s = '    ' * tabs
                    output.append(tab_s + '<min>%s</min>\n' % min)
                    output.append(tab_s + '<max>%s</max>\n' % max)
            else:
                xrcgen.DefaultXrcObject.write_property(self, name, val, output, tabs)
    # end of class SpinButtonXrcObject

    return SpinButtonXrcObject(obj)


def initialize():
    klass = 'wxSpinButton'
    common.class_names['EditSpinButton'] = klass
    common.register('python', klass, PythonSpinButtonGenerator(klass))
    common.register('C++', klass, CppSpinButtonGenerator(klass))
    common.register('XRC', klass, xrc_code_generator)
