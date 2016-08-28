"""\
Handler for the 'choices' property of various elements

@copyright: 2002-2007 Alberto Griggio
@copyright: 2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

from wcodegen.taghandler import BaseCodeWriterTagHandler


class ChoicesCodeHandler(BaseCodeWriterTagHandler):
    "handler for the 'choices' property of various elements"

    def __init__(self):
        super(ChoicesCodeHandler, self).__init__()
        self.choices = []
        self.cur_checked = None

    def start_elem(self, name, attrs):
        if name == 'choice':
            try:
                self.cur_checked = int(attrs['checked'])
            except (KeyError, ValueError):
                self.cur_checked = None

    def end_elem(self, name, code_obj):
        if name == 'choice':
            label = self.get_char_data()
            if self.cur_checked is None:
                self.choices.append(label)
            else:
                self.choices.append((label, self.cur_checked))
            self.cur_checked = None
        elif name == 'choices':
            code_obj.properties['choices'] = self.choices
            return True



def xrc_write_choices_property(xrc_obj, outfile, tabs):
    """\
    function used to write the XRC code for a ``choices'' property
    """
    from xml.sax.saxutils import escape
    choices = xrc_obj.properties['choices']
    write = outfile.write
    write('    '*tabs + '<content>\n')
    tab_s = '    ' * (tabs+1)
    for choice in choices:
        if isinstance(choice, tuple):
            write(tab_s + '<item checked="%d">%s</item>\n' % \
                  (choice[1], escape(choice[0])))
        else:
            write(tab_s + '<item>%s</item>\n' % escape(choice))
    write('    '*tabs + '</content>\n')

