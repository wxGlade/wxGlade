"""\
Tool objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

from common import format_xml_tag


class Tool(object):
    def __init__(self, id='', label='', type=0, short_help='',
                 long_help='', bitmap1='', bitmap2='', handler=''):
        self.id = id
        self.label = label
        self.type = type
        self.short_help = short_help
        self.long_help = long_help
        self.bitmap1 = bitmap1
        self.bitmap2 = bitmap2
        self.handler = handler

    def write(self, outfile, tabs):
        inner_xml = format_xml_tag(u'id', self.id, tabs + 1)
        inner_xml += format_xml_tag(u'label', self.label, tabs + 1)
        inner_xml += format_xml_tag(u'type', self.type, tabs + 1)
        inner_xml += format_xml_tag(u'short_help', self.short_help, tabs + 1)
        inner_xml += format_xml_tag(u'long_help', self.long_help, tabs + 1)
        inner_xml += format_xml_tag(u'bitmap1', self.bitmap1, tabs + 1)
        inner_xml += format_xml_tag(u'bitmap2', self.bitmap2, tabs + 1)
        if self.handler:
            inner_xml += format_xml_tag(u'handler', self.handler, tabs + 1)
        stmt = format_xml_tag(u'tool', inner_xml, tabs, is_xml=True)
        outfile.write(stmt)

# end of class Tool
