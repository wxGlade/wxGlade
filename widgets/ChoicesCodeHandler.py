# ChoicesCodeHandler.py: handler for the 'choices' property of various elements
# $Id: ChoicesCodeHandler.py,v 1.8 2007/03/27 07:02:05 agriggio Exp $
#
# Copyright (c) 2002-2007 Alberto Griggio <agriggio@users.sourceforge.net>
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

class ChoicesCodeHandler:
    """\
    handler for the 'choices' property of various elements
    """
    def __init__(self):
        self.choices = []
        self.curr_choice = []
        
    def start_elem(self, name, attrs): pass
            
    def end_elem(self, name, code_obj):
        if name == 'choice':
            c = "".join(self.curr_choice)
            self.choices.append("".join(self.curr_choice))
            self.curr_choice = []
        elif name == 'choices':
            code_obj.properties['choices'] = self.choices
            return True

    def char_data(self, data):
        self.curr_choice.append(data)

# end of class ChoicesCodeHandler


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
        write(tab_s + '<item>%s</item>\n' % escape(choice))
    write('    '*tabs + '</content>\n')

