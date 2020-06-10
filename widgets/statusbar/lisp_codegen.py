"""\
Code generator functions for wxStatusBar objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class LispStatusBarCodeGenerator(wcodegen.LispWidgetCodeWriter):
    tmpl = '(setf %(name)s (wxFrame_CreateStatusBar (slot-top-window obj) %(labels_len)s %(style)s))\n'

    def _prepare_tmpl_content(self, obj):
        wcodegen.LispWidgetCodeWriter._prepare_tmpl_content(self, obj)

        fields = obj.fields # properties['statusbar']
        labels = [f[0] for f in fields]
        widths = [int(f[1]) for f in fields]

        self.tmpl_dict['labels_len'] = len(labels)
        self.tmpl_dict['widths'] = ' '.join([str(w) for w in widths])
        self.tmpl_dict['widths_len'] = len(widths)

        self.tmpl_props.append( '(wxStatusBar_SetStatusWidths %(name)s %(widths_len)s (vector %(widths)s))\n' )

        # don't add statusbar fields without labels
        if [lb for lb in labels if lb]:
            for i, lb in enumerate(labels):
                stmt = '(wxStatusBar_SetStatusText %%(name)s %s %d)\n' % (self.codegen.quote_str(lb), i)
                self.tmpl_props.append(stmt)



def initialize():
    klass = 'wxStatusBar'
    common.class_names['EditStatusBar'] = klass

    lispgen = common.code_writers.get('lisp')
    if lispgen:
        lispgen.register_widget_code_generator('wxStatusBar', LispStatusBarCodeGenerator(klass))
