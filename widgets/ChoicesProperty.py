"""\
Defines a Property and two handlers used by choice, combo_box, radio_box, list_box

@copyright: 2002-2007 Alberto Griggio
@copyright: 2016 Carsten Grohmann
@copyright: 2016-2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import new_properties as np
import common
from wcodegen.taghandler import BaseXmlBuilderTagHandler

__all__ = ['ChoicesProperty', 'ChoicesHandler']

class ChoicesProperty(np.GridProperty):
    def write(self, output, tabs):
        inner_xml = []
        #for val in self.get_value():
        for val in self.get():
            value = common.encode_to_unicode(val[0])  # only first column is used
            try:
                checked = int(val[1])
            except (IndexError, ValueError):
                checked = None
            if checked is None:
                inner_xml += common.format_xml_tag(u'choice', value, tabs+1)
            else:
                inner_xml += common.format_xml_tag(u'choice', value, tabs+1, checked="%s" % checked)
        output.extend( common.format_xml_tag(u'choices', inner_xml, tabs, is_xml=True) )



class ChoicesHandler(BaseXmlBuilderTagHandler):
    def __init__(self, owner):
        super(ChoicesHandler, self).__init__()
        self.choices = []
        self.cur_checked = None
        self.owner = owner

    def start_elem(self, name, attrs):
        super(ChoicesHandler, self).start_elem(name, attrs)
        if name == 'choice':
            try:
                self.cur_checked = int(attrs['checked'])
            except (KeyError, ValueError):
                self.cur_checked = None

    def end_elem(self, name):
        if name == 'choice':
            char_data = self.get_char_data()
            if self.cur_checked is None:
                self.choices.append([char_data])
            else:
                self.choices.append([char_data, self.cur_checked])
            self.cur_checked = None
        elif name == 'choices':
            self.owner.properties['choices'].load(self.choices)
            self.choices = []
            return True  # remove the handler

