"""\
Code generator functions for wxDialog objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PythonDialogGenerator(wcodegen.PythonWidgetCodeWriter):
    def get_code(self, obj):
        return [], [], []

    def get_properties_code(self, obj):
        out = []
        title = obj.properties.get('title')
        if title:
            out.append('self.SetTitle(%s)\n' % self.codegen.quote_str(title))
        icon = obj.properties.get('icon')
        if icon:
            if icon.startswith('var:'):
                if not obj.preview:
                    out.append('_icon = ' + self.cn('wxEmptyIcon') + '()\n')
                    out.append(('_icon.CopyFromBitmap(' + self.cn('wxBitmap') +
                                '(%s, ' + self.cn('wxBITMAP_TYPE_ANY') + '))\n') %
                               icon[4:].strip())
                    out.append('self.SetIcon(_icon)\n')
            elif icon.startswith('code:'):
                if not obj.preview:
                    out.append('_icon = ' + self.cn('wxEmptyIcon') + '()\n')
                    out.append(('_icon.CopyFromBitmap(%s)\n') %
                               icon[5:].strip())
                    out.append('self.SetIcon(_icon)\n')
            else:
                if obj.preview:
                    import misc
                    icon = misc.get_relative_path(icon, True)
                out.append('_icon = ' + self.cn('wxEmptyIcon') + '()\n')
                out.append(('_icon.CopyFromBitmap(' + self.cn('wxBitmap') +
                            '(%s, ' + self.cn('wxBITMAP_TYPE_ANY') + '))\n') %
                           self.codegen.quote_path(icon))
                out.append('self.SetIcon(_icon)\n')
        out.extend(self.codegen.generate_common_properties(obj))
        return out

    def get_layout_code(self, obj):
        ret = ['self.Layout()\n']
        try:
            if int(obj.properties['centered']):
                ret.append('self.Centre()\n')
        except (KeyError, ValueError):
            pass
        return ret

# end of class PythonDialogGenerator


class CppDialogGenerator(wcodegen.CppWidgetCodeWriter):
    constructor = [('wxWindow*', 'parent'), ('int', 'id'),
                   ('const wxString&', 'title'),
                   ('const wxPoint&', 'pos', 'wxDefaultPosition'),
                   ('const wxSize&', 'size', 'wxDefaultSize'),
                   ('long', 'style', 'wxDEFAULT_DIALOG_STYLE')]

    def get_code(self, obj):
        return [], [], [], []

    def get_properties_code(self, obj):
        """\
        generates the code for the various wxDialog specific properties.
        Returns a list of strings containing the generated code
        """
        out = []
        title = obj.properties.get('title')
        if title:
            out.append('SetTitle(%s);\n' % self.codegen.quote_str(title))
        icon = obj.properties.get('icon')
        if icon:
            out.append('wxIcon _icon;\n')
            if icon.startswith('var:'):
                out.append('_icon.CopyFromBitmap(wxBitmap(' +
                           '%s, wxBITMAP_TYPE_ANY));\n' %
                           icon[4:].strip())
            elif icon.startswith('code:'):
                out.append('_icon.CopyFromBitmap(%s);\n' %
                           icon[5:].strip())
            else:
                out.append('_icon.CopyFromBitmap(wxBitmap(%s, '
                           'wxBITMAP_TYPE_ANY));\n' %
                           self.codegen.quote_path(icon))
            out.append('SetIcon(_icon);\n')
        out.extend(self.codegen.generate_common_properties(obj))
        return out

    def get_layout_code(self, obj):
        ret = ['Layout();\n']
        try:
            if int(obj.properties['centered']):
                ret.append('Centre();\n')
        except (KeyError, ValueError):
            pass
        return ret

# end of class CppDialogGenerator


def xrc_code_generator(obj):
    xrcgen = common.code_writers['XRC']

    class DialogXrcObject(xrcgen.DefaultXrcObject):
        def write_property(self, name, val, outfile, ntabs):
            if name != 'sizehints':
                xrcgen.DefaultXrcObject.write_property(
                    self, name, val, outfile, ntabs)
    # end of class DialogXrcObject

    return DialogXrcObject(obj)


def initialize():
    klass = 'wxDialog'
    common.class_names['EditDialog'] = klass
    common.toplevels['EditDialog'] = 1
    common.register('python', klass, PythonDialogGenerator(klass))
    common.register('C++', klass, CppDialogGenerator(klass))
    common.register('XRC', klass, xrc_code_generator)
