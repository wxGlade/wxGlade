"""\
Tool objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2015 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

from xml.sax.saxutils import escape
from common import encode_to_unicode


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
        fwrite = outfile.write
        fwrite("    " * tabs + '<tool>\n')
        tab_s = "    " * (tabs+1)
        fwrite(tab_s + '<id>%s</id>\n' % escape(encode_to_unicode(self.id)))
        fwrite(tab_s + '<label>%s</label>\n' % \
               escape(encode_to_unicode(self.label)))
        fwrite(tab_s + '<type>%s</type>\n' % escape(str(self.type)))
        fwrite(tab_s + '<short_help>%s</short_help>\n' % \
               escape(encode_to_unicode(self.short_help)))
        fwrite(tab_s + '<long_help>%s</long_help>\n' % \
               escape(encode_to_unicode(self.long_help)))
        fwrite(tab_s + '<bitmap1>%s</bitmap1>\n' % \
               escape(encode_to_unicode(self.bitmap1)))
        fwrite(tab_s + '<bitmap2>%s</bitmap2>\n' % \
               escape(encode_to_unicode(self.bitmap2)))
        if self.handler:
            fwrite(tab_s + '<handler>%s</handler>\n' % \
                   escape(encode_to_unicode(self.handler.strip())))
        fwrite("    " * tabs + '</tool>\n')

# end of class Tool
