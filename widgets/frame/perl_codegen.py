# codegen.py: code generator functions for wxFrame objects
# $Id: perl_codegen.py,v 1.3 2003/06/26 08:48:11 crazyinsomniac Exp $
#
# Copyright (c) 2002-2003 Alberto Griggio <albgrig@tiscalinet.it>
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

import common
from MenuTree import *
from codegen import StatusFieldsHandler


class PerlStatusBarCodeGenerator:
    def get_code(self, obj):
        """\
        function that generates code for the statusbar of a wxFrame.
        """
        plgen = common.code_writers['perl']
        labels, widths = obj.properties['statusbar']
        init = [ '\t$self->{%s} = $self->CreateStatusBar(%s);\n'
            % (obj.name, len(labels)) ]
        props = []
        append = props.append
        append('\t$self->{%s}->SetStatusWidths(%s);\n'
            %  (obj.name, ','.join(map(str, widths))))
        labels = ',\n\t\t'.join([plgen.quote_str(l) for l in labels])
        append('\n\tmy( @%s_fields ) = (\n\t\t%s\n\t);\n\n' %
               (obj.name, labels))
        append('\tif( @%s_fields ) {\n' % obj.name)
        append('\t\t$self->{%s}->SetStatusText($%s_fields[$_], $_) '
            % (obj.name, obj.name) )
        append('\n\t\t\tfor 0 .. $#%s_fields ;\n\t}\n' % obj.name)
        return init, props, []

# end of class PerlStatusBarCodeGenerator


class PerlFrameCodeGenerator:
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
            out.append('\t$self->SetTitle(%s);\n' % plgen.quote_str(title))

        icon = prop.get('icon')
        if icon:
            out.append('\tmy $icon = Wx::Icon->new();\n')
            out.append('\t$icon->CopyFromBitmap(Wx::Bitmap->new(%s, '
                       'wxBITMAP_TYPE_ANY));\n' % plgen.quote_str(icon))
            out.append('\t$self->SetIcon($icon);\n')
            
        out.extend(plgen.generate_common_properties(frame))
        return out

    def get_layout_code(self, frame):
        ret = ['\t$self->Layout();\n']
        try:
            if int(frame.properties['centered']):
                ret.append('\t$self->Centre();\n')
        except (KeyError, ValueError):
            pass
        return ret

# end of class PerlFrameCodeGenerator


class PerlMDIChildFrameCodeGenerator(PerlFrameCodeGenerator):
    extra_headers = ['Wx::MDI']

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

