"""\
Defines a Property and two handlers used by choice, combo_box, radio_box,
list_box

@copyright: 2002-2007 Alberto Griggio
@copyright: 2015 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import widget_properties
import common
from wcodegen.taghandler import BaseXmlBuilderTagHandler


class ChoicesProperty(widget_properties.GridProperty):
    def write(self, outfile, tabs):
        inner_xml = u''
        for val in self.get_value():
            value = common.encode_to_unicode(val[0])
            try:
                checked = int(val[1])
            except (IndexError, ValueError):
                checked = None
            if checked is None:
                inner_xml += common.format_xml_tag(
                    u'choice', value, tabs + 1)
            else:
                inner_xml += common.format_xml_tag(
                    u'choice', value, tabs + 1, checked="%s" % checked)
        stmt = common.format_xml_tag(
            u'choices', inner_xml, tabs, is_xml=True)
        outfile.write(stmt)

# end of class ChoicesProperty


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
            self.owner.set_choices(self.choices)
            self.owner.properties['choices'].set_value(
                self.owner.get_choices())
            self.choices = []
            return True  # remove the handler

# end of class ChoicesHandler

