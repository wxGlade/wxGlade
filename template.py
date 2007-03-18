# template.py: handles the template tags and description
# $Header: /home/alb/tmp/wxglade_cvs_backup/wxGlade/template.py,v 1.3 2007/03/18 15:26:16 guyru Exp $
# 
# Copyright (c) 2002-2005 Alberto Griggio <agriggio@users.sourceforge.net>
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY
#
# Author: Guy Rutenberg.

from xml.dom import minidom
from xml.sax import saxutils

class Template:
    """ \
    A class that handles the specific aspects of template files.
    """
    
    def __init__(self, file):
        self.filexml = minidom.parse(file)
        #we have no use for all the xml data in the file. We only care about what is between the "description" tags
        templatedata = self.filexml.getElementsByTagName('templatedata')
        if len(templatedata):
            self.desc_xml = templatedata[0]
            try:
                self.author = saxutils.unescape(self.desc_xml.getElementsByTagName('author')[0].firstChild.data)
            except IndexError: self.author = ''
            try:
                self.description = saxutils.unescape(self.desc_xml.getElementsByTagName('description')[0].firstChild.data)
            except IndexError: self.description = ''
            try:
                self.instructions = saxutils.unescape(self.desc_xml.getElementsByTagName('instructions')[0].firstChild.data)
            except IndexError: self.instructions = ''
            
        else:
            self.author = ''
            self.description=''
            self.instructions=''
        
        print "Author: ", self.author
        print "Description: ", self.description
        print "Instructions: ", self.instructions
        
