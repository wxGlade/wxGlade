"""\
Code generator functions for spacers

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2019 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen
from . import spacer_base


class PythonSpacerGenerator(spacer_base.SpacerMixin, wcodegen.PythonWidgetCodeWriter):
    pass

class CppSpacerGenerator(spacer_base.SpacerMixin, wcodegen.CppWidgetCodeWriter):
    def get_code(self, obj):
        init, final = spacer_base.SpacerMixin.get_code(self, obj)
        return init, [], final


def xrc_code_generator(obj):
    xrcgen = common.code_writers['XRC']

    class SpacerXrcObject(xrcgen.XrcObject):
        "XrcObject to handle widgets"
    
        def __init__(self, obj):
            xrcgen.XrcObject.__init__(self)
            self.obj = obj
    
        def write(self, output, ntabs):
            obj = self.obj
    
            tabs = self.tabs(ntabs)
            tabs1 = self.tabs(ntabs + 1)
    
            if obj is None or obj.name == "spacer":
                output.append( tabs + '<object class="spacer">\n' )
            else:
                output.append( tabs + '<object class="spacer" name=%s>\n'%quoteattr(obj.name) )
            if obj is not None:
                # a real spacer
                output.append( tabs1 + '<size>%s, %s</size>\n' % (obj.width, obj.height) )
                if obj.proportion:
                    output.append(tabs1 + '<option>%s</option>\n' % obj.proportion)
                if obj.flag:
                    flag = obj.properties["flag"].get_string_value()
                    output.append(tabs1 + '<flag>%s</flag>\n' % self.cn_f(flag))
                if obj.border:
                    output.append(tabs1 + '<border>%s</border>\n' % obj.border)
            else:
                # just an empty sizer slot
                output.append( tabs1 + '<size>0, 0</size>\n' )
            output.append(tabs + '</object>\n')

    return SpacerXrcObject(obj)


def initialize():
    klass = 'spacer'
    common.class_names['EditSpacer'] = klass
    common.register('python', klass, PythonSpacerGenerator(klass))
    common.register('C++',    klass, CppSpacerGenerator(klass))

    xrcgen = common.code_writers.get("XRC")
    if xrcgen:
        xrcgen.register_widget_code_generator( 'spacer', xrc_code_generator )

