# tool.py: Tool objects
#
# Copyright (c) 2002-2003 Alberto Griggio <albgrig@tiscalinet.it>
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

class Tool:
    def __init__(self, id='', label='', type=0, short_help='',
                 long_help='', bitmap1='', bitmap2=''):
        self.id = id
        self.label = label
        self.type = type
        self.short_help = short_help
        self.long_help = long_help
        self.bitmap1 = bitmap1
        self.bitmap2 = bitmap2

    def write(self, outfile, tabs):
        from xml.sax.saxutils import escape, quoteattr
        fwrite = outfile.write
        fwrite("    " * tabs + '<tool>\n')
        tab_s = "    " * (tabs+1)
        fwrite(tab_s + '<id>%s</id>\n' % escape(self.id))
        fwrite(tab_s + '<label>%s</label>\n' % escape(self.label))
        fwrite(tab_s + '<type>%s</type>\n' % escape(str(self.type)))
        fwrite(tab_s + '<short_help>%s</short_help>\n' % \
               escape(self.short_help))
        fwrite(tab_s + '<long_help>%s</long_help>\n' % escape(self.long_help))
        fwrite(tab_s + '<bitmap1>%s</bitmap1>\n' % escape(self.bitmap1))
        fwrite(tab_s + '<bitmap2>%s</bitmap2>\n' % escape(self.bitmap2))
        fwrite("    " * tabs + '</tool>\n')

# end of class Tool
