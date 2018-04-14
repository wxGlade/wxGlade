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


def initialize():
    cn = common.class_names
    cn['EditBoxSizer'] = 'wxBoxSizer'
    cn['EditWrapSizer'] = 'wxWrapSizer'
    cn['EditStaticBoxSizer'] = 'wxStaticBoxSizer'
    cn['EditGridSizer'] = 'wxGridSizer'
    cn['EditFlexGridSizer'] = 'wxFlexGridSizer'

    xrcgen = common.code_writers.get("XRC")
    if xrcgen:
        xrcgen.add_widget_handler( 'wxBoxSizer', xrc_wxSizer_builder )
        xrcgen.add_widget_handler( 'wxWrapSizer', xrc_wxSizer_builder )
        xrcgen.add_widget_handler( 'wxStaticBoxSizer', xrc_wxSizer_builder )
        xrcgen.add_widget_handler( 'wxGridSizer', xrc_wxSizer_builder )
        xrcgen.add_widget_handler( 'wxFlexGridSizer', xrc_wxFlexGridSizer_builder )
