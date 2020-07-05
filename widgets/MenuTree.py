"""\
A class to represent a menu on a wxMenuBar

@copyright: 2002-2007 Alberto Griggio
@copyright: 2016 Carsten Grohmann
@copyright: 2017 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

from common import format_xml_tag
import compat

__all__ = ['MenuTree']

class MenuTree(object):
    "A class to represent a menu on a wxMenuBar"
    class Node:
        # the following are for compatibility with the tree structure, used by format_generic_access
        IS_CLASS = IS_SIZER = False
        klass = classname = None
        properties = []
        IS_NAMED = True
        WX_CLASS = None  # for Perl
        def __init__(self, label="", id="", name="", help_str="", checkable="", radio="", handler=""):
            self.label = label
            self.id = id
            self.name = name
            self.help_str = help_str
            self.checkable = checkable
            self.radio = radio
            self.handler = handler
            self.children = []
            self.parent = None

        def write(self, output, tabs, top=False):
            inner_xml = []
            if not top and not self.children:
                if self.label:
                    inner_xml += format_xml_tag(u'label', self.label, tabs+1)
                if self.id:
                    inner_xml += format_xml_tag(u'id', self.id, tabs+1)
                if self.name:
                    inner_xml += format_xml_tag(u'name', self.name, tabs+1)
                if self.help_str:
                    inner_xml += format_xml_tag(u'help_str', self.help_str, tabs+1)
                try:
                    checkable = int(self.checkable)
                except ValueError:
                    checkable = 0
                if checkable:
                    inner_xml += format_xml_tag(u'checkable', checkable, tabs+1)

                try:
                    radio = int(self.radio)
                except ValueError:
                    radio = 0
                if radio:
                    inner_xml += format_xml_tag(u'radio', radio, tabs+1)

                if self.handler:
                    inner_xml += format_xml_tag(u'handler', self.handler, tabs+1)
                output.extend( format_xml_tag(u'item', inner_xml, tabs, is_xml=True) )
            else:
                attrs = {'name': self.name}
                if self.id:
                    attrs[u'itemid'] = self.id
                if self.handler:
                    attrs[u'handler'] = self.handler
                attrs[u'label'] = self.label
                inner_xml = []
                for c in self.children:
                    c.write(inner_xml, tabs + 1)
                output.extend( format_xml_tag( u'menu', inner_xml, tabs, is_xml=True, **attrs ) )

    #end of class Node

    def __init__(self, name, label, id="", help_str="", handler=""):
        self.root = self.Node(label, id, name, help_str, handler=handler)

    def write(self, output, tabs):
        self.root.write(output, tabs, top=True)

