"""\
Code generator functions for wxSpinButton objects

@copyright: 2004 D.H. aka crazyinsomniac at users.sourceforge.net
@copyright: 2014 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PythonSpinButtonGenerator(wcodegen.PythonWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s(%(parent)s, %(id)s %(style)s)\n'
    default_style = 'wxSP_HORIZONTAL'

# end of class PythonSpinButtonGenerator


class CppSpinButtonGenerator(wcodegen.CppWidgetCodeWriter):
    extra_headers = ['<wx/spinbutt.h>']
    tmpl = '%(name)s = new %(klass)s(%(parent)s, %(id)s%(style)s);\n'
    default_style = 'wxSP_HORIZONTAL'

    def get_events(self, obj):
        return self.codegen.get_events_with_type(obj, 'wxSpinEvent')

# end of class CppSpinButtonGenerator


def xrc_code_generator(obj):
    xrcgen = common.code_writers['XRC']

    class SpinButtonXrcObject(xrcgen.DefaultXrcObject):
        def write_property(self, name, val, outfile, tabs):
            if name == 'range':
                try:
                    min, max = val.split(',')
                except ValueError:
                    pass
                else:
                    tab_s = '    ' * tabs
                    outfile.write(tab_s + '<min>%s</min>\n' % min)
                    outfile.write(tab_s + '<max>%s</max>\n' % max)
            else:
                xrcgen.DefaultXrcObject.write_property(self, name, val,
                                                       outfile, tabs)

    # end of class SpinButtonXrcObject

    return SpinButtonXrcObject(obj)


def initialize():
    common.class_names['EditSpinButton'] = 'wxSpinButton'

    pygen = common.code_writers.get('python')
    if pygen:
        pygen.add_widget_handler('wxSpinButton', PythonSpinButtonGenerator())
    xrcgen = common.code_writers.get("XRC")
    if xrcgen:
        xrcgen.add_widget_handler('wxSpinButton', xrc_code_generator)
    cppgen = common.code_writers.get('C++')
    if cppgen:
        cppgen.add_widget_handler('wxSpinButton', CppSpinButtonGenerator())
