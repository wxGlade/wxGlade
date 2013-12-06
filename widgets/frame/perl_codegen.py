"""\
Perl code generator functions for wxFrame objects

@copyright: 2002-2007 Alberto Griggio <agriggio@users.sourceforge.net>
@copyright: 2013 Carsten Grohmann <mail@carstengrohmann.de>
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen
from MenuTree import *
from codegen import StatusFieldsHandler


class PerlStatusBarCodeGenerator(wcodegen.BaseWidgetCodeWriter):

    def get_code(self, obj):
        """\
        Function that generates code for the statusbar of a wxFrame.
        """
        plgen = common.code_writers['perl']
        labels, widths = obj.properties['statusbar']
        style = obj.properties.get("style")
        if not style:
            style = '0'
        init = [ '$self->{%s} = $self->CreateStatusBar(%s, %s);\n'
                 % (obj.name, len(labels), style) ]
        props = []
        sep = ',\n%s' % plgen.tabs(1)
        labels = sep.join([plgen.quote_str(l) for l in labels])
        stmt = """\
$self->{%(obj_name)s}->SetStatusWidths(%(widths)s);

my( @%(obj_name)s_fields ) = (
%(tab)s%(labels)s
);

if( @%(obj_name)s_fields ) {
%(tab)s$self->{%(obj_name)s}->SetStatusText($%(obj_name)s_fields[$_], $_) 
%(tab)sfor 0 .. $#%(obj_name)s_fields ;
}""" % {
            'labels':   labels,
            'obj_name': obj.name,
            'tab':    plgen.tabs(1),
            'widths': ','.join(map(str, widths)),
            }
        props.extend(self.stmt2list(stmt))
        return init, props, []

# end of class PerlStatusBarCodeGenerator


class PerlFrameCodeGenerator:
#wxFrame(  parent, id, title, pos , size , style , name )
    new_signature = [
        '$parent', '$id', '$title', '$pos', '$size', '$style', '$name'
    ]

    def get_code(self, obj):
        return [], [], [], [] # the frame can't be a children

    def get_properties_code(self, frame):
        """\
        generates the code for the various wxFrame specific properties.
        Returns a list of strings containing the generated code
        """
        prop = frame.properties
        plgen = common.code_writers['perl']
        out = []
        title = prop.get('title')
        if title:
            out.append('$self->SetTitle(%s);\n' % plgen.quote_str(title))

        icon = prop.get('icon')
        if icon:
            out.append('my $icon = Wx::Icon->new();\n')
            out.append('$icon->CopyFromBitmap(Wx::Bitmap->new(%s, '
                       'wxBITMAP_TYPE_ANY));\n' % plgen.quote_str(icon))
            out.append('$self->SetIcon($icon);\n')
            
        out.extend(plgen.generate_common_properties(frame))
        return out

    def get_layout_code(self, frame):
        ret = ['$self->Layout();\n']
        try:
            if int(frame.properties['centered']):
                ret.append('$self->Centre();\n')
        except (KeyError, ValueError):
            pass
        plgen = common.code_writers['perl']
        if frame.properties.get('size', '').strip() and \
               plgen.for_version < (2, 8):
            ret.append(plgen.generate_code_size(frame))
        return ret

# end of class PerlFrameCodeGenerator


class PerlMDIChildFrameCodeGenerator(PerlFrameCodeGenerator):
    extra_headers = ['Wx::MDI']
#wxMDIChildFrame(parent, id, title, pos, size, style, name )

# end of class PerlMDIChildFrameCodeGenerator


def initialize():
    cn = common.class_names
    cn['EditFrame'] = 'wxFrame'
    cn['EditMDIChildFrame'] = 'wxMDIChildFrame'
    cn['EditStatusBar'] = 'wxStatusBar'
    common.toplevels['EditFrame'] = 1
    common.toplevels['EditMDIChildFrame'] = 1

    plgen = common.code_writers.get('perl')
    if plgen:
        plgen.add_widget_handler('wxFrame', PerlFrameCodeGenerator())
        plgen.add_widget_handler('wxMDIChildFrame',
                                  PerlMDIChildFrameCodeGenerator())
        
        plgen.add_widget_handler('wxStatusBar', PerlStatusBarCodeGenerator())
        
        plgen.add_property_handler('fields', StatusFieldsHandler)
        plgen.add_property_handler('menubar', plgen.DummyPropertyHandler)
        plgen.add_property_handler('statusbar', plgen.DummyPropertyHandler)

