# ChoicesCodeHandler.py: handler for the 'choices' property of various elements
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: GPL (see license.txt)

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
