"""
XRC generator functions for the various wxSizers

@copyright: 2014-2016 Carsten Grohmann
@copyright: 2017 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common


def xrc_wxSizer_builder(obj):
    xrcgen = common.code_writers['XRC']

    class SizerXrcObject(xrcgen.DefaultXrcObject):
        def write(self, output, ntabs, properties=None):
            if properties is None: properties = {}
            properties["class_orient"] = None # don't write
            xrcgen.DefaultXrcObject.write(self, output, ntabs, properties)
    # end of class SizerXrcObject

    return SizerXrcObject(obj)


def xrc_wxFlexGridSizer_builder(obj):
    xrcgen = common.code_writers['XRC']

    class FlexGridSizerXrcObject(xrcgen.DefaultXrcObject):
        def write(self, output, ntabs, properties=None):
            if properties is None: properties = {}
            properties["class_orient"] = None # don't write
            xrcgen.DefaultXrcObject.write(self, output, ntabs, properties)
        def write_property(self, name, val, output, tabs):
            if val and name in ('growable_rows', 'growable_cols'):
                if name == 'growable_rows':
                    name2 = 'growablerows'
                else:
                    name2 = 'growablecols'
                output.append( '    ' * tabs + '<%s>%s</%s>\n' % (name2, val, name2) )
            else:
                xrcgen.DefaultXrcObject.write_property(self, name, val, output, tabs)
    # end of class FlexGridSizerXrcObject

    return FlexGridSizerXrcObject(obj)


def xrc_code_generator(obj):
    xrcgen = common.code_writers['XRC']

    class SizerSlotXrcObject(xrcgen.XrcObject):
        "XrcObject to handle widgets"

        def __init__(self, obj):
            xrcgen.XrcObject.__init__(self)
            self.obj = obj

        def write(self, output, ntabs):
            if self.obj: return
            tabs = self.tabs(ntabs)
            tabs1 = self.tabs(ntabs + 1)
            output.append( tabs + '<object class="spacer">\n' )
            output.append( tabs1 + '<size>0, 0</size>\n' )
            output.append( tabs + '</object>\n')

    return SizerSlotXrcObject(obj)


def initialize():
    cn = common.class_names
    cn['EditBoxSizer'] = 'wxBoxSizer'
    cn['EditWrapSizer'] = 'wxWrapSizer'
    cn['EditStaticBoxSizer'] = 'wxStaticBoxSizer'
    cn['EditGridSizer'] = 'wxGridSizer'
    cn['EditFlexGridSizer'] = 'wxFlexGridSizer'

    xrcgen = common.code_writers.get("XRC")
    if xrcgen:
        xrcgen.register_widget_code_generator( 'wxBoxSizer', xrc_wxSizer_builder )
        xrcgen.register_widget_code_generator( 'wxWrapSizer', xrc_wxSizer_builder )
        xrcgen.register_widget_code_generator( 'wxStaticBoxSizer', xrc_wxSizer_builder )
        xrcgen.register_widget_code_generator( 'wxGridSizer', xrc_wxSizer_builder )
        xrcgen.register_widget_code_generator( 'wxFlexGridSizer', xrc_wxFlexGridSizer_builder )
        xrcgen.register_widget_code_generator( 'sizerslot', xrc_code_generator )

