# codegen.py: code generator functions for wxFrame objects
# $Id: lisp_codegen.py,v 1.3 2007/03/27 07:02:00 agriggio Exp $
#
# Copyright (c) 2002-2007 Alberto Griggio <agriggio@users.sourceforge.net>
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

import common
from MenuTree import *
from codegen import StatusFieldsHandler


class LispStatusBarCodeGenerator:
    def get_code(self, obj):
        """\
        function that generates code for the statusbar of a wxFrame.
        """
        lispgen = common.code_writers['lisp']
        labels, widths = obj.properties['statusbar']
        style = obj.properties.get("style")
        if not style: style = '0'
        init = [ '(setf (slot-%s obj) (wxFrame_CreateStatusBar (slot-top-window obj) %s %s))\n'
                 % (obj.name, len(labels), style) ]
        props = []

        append = props.append
        append('(wxStatusBar_SetStatusWidths (slot-%s obj) %s (vector %s))\n'
            %  (obj.name, len(widths),' '.join(map(str, widths))))

        i = 0
        for l in labels:
            append('\t (wxStatusBar_SetStatusText (slot-%s obj) %s %s)\n'
                   % (obj.name, lispgen.quote_str(l),i) )
            i=i+1
        return init, props, []

# end of class LispStatusBarCodeGenerator


class LispFrameCodeGenerator:
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
        lispgen = common.code_writers['lisp']
        out = []
        title = prop.get('title')
        if title:
            out.append('(wxFrame_SetTitle (slot-top-window obj) %s)\n' % \
                    lispgen.quote_str(title))

        icon = prop.get('icon')
        if icon:
            out.append(
                ';;; generating code for setting icons is not implemented\n'
                )
            
        out.extend(lispgen.generate_common_properties(frame))
        return out

    def get_layout_code(self, frame):
        ret = ['(wxFrame_layout (slot-%s slef))\n' % frame.name]
        try:
            if int(frame.properties['centered']):
                ret.append('(wxFrame_Centre (slot-top-window obj) wxBOTH)\n')
        except (KeyError, ValueError):
            pass
        return ret

# end of class LispFrameCodeGenerator


class LispMDIChildFrameCodeGenerator(LispFrameCodeGenerator):
    extra_headers = ['Wx::MDI']
#wxMDIChildFrame(parent, id, title, pos, size, style, name )

# end of class LispMDIChildFrameCodeGenerator


def initialize():
    cn = common.class_names
    cn['EditFrame'] = 'wxFrame'
    cn['EditMDIChildFrame'] = 'wxMDIChildFrame'
    cn['EditStatusBar'] = 'wxStatusBar'
    common.toplevels['EditFrame'] = 1
    common.toplevels['EditMDIChildFrame'] = 1

    lispgen = common.code_writers.get('lisp')
    if lispgen:
        lispgen.add_widget_handler('wxFrame', LispFrameCodeGenerator())
        lispgen.add_widget_handler('wxMDIChildFrame',
                                  LispMDIChildFrameCodeGenerator())
        
        lispgen.add_widget_handler('wxStatusBar', LispStatusBarCodeGenerator())
        
        lispgen.add_property_handler('fields', StatusFieldsHandler)
        lispgen.add_property_handler('menubar', lispgen.DummyPropertyHandler)
        lispgen.add_property_handler('statusbar', lispgen.DummyPropertyHandler)

