"""\
Base classes for custom tag handlers.

The custon tag handlers are called within the XML parsers in xml_parse.

@copyright: 2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import xml.sax.saxutils
import compat


class BaseTagHandler(object):
    """Base for all custom tag handler classes; all handler are called during the XML parse process."""

    strip_char_data = False

    def __init__(self):
        self._content = []  # Tag content as list of unicode strings

    def start_elem(self, name, attrs):
        """Start a new elements

        name: Element name
        attrs: Element attributes as dict"""
        pass

    def char_data(self, data):
        """Process tag content; if returning False, we don't have to call add_property().

        data: (Partial) tag content as unicode

        see: self._content, xml_parse.XmlWidgetObject.add_property()"""
        assert isinstance(data, compat.unicode)
        if self.strip_char_data:
            data = data.strip()
        if data:
            self._content.append(data)
        return False

    def get_char_data(self):
        "Return the whole content of a tag"
        if not self._content:
            return u''
        data = "".join(self._content)
        assert isinstance(data, compat.unicode)
        data = xml.sax.saxutils.unescape(data)
        self._content = []
        return data


class BaseCodeWriterTagHandler(BaseTagHandler):
    "Custom tag handler interface called by xml_parse.CodeWriter"

    def end_elem(self, name, code_obj):
        """End the current element

        name: Element name as unicode
        code_obj: Object to generate code for as CodeObject

        return: True to remove this handler from XML parser stack"""
        return True


class BaseXmlBuilderTagHandler(BaseTagHandler):
    "Custom tag handler interface called by xml_parse.XmlWidgetBuilder"

    def end_elem(self, name):
        """End the current element

        name: Element name as unicode

        return: True to remove this handler from XML parser stack"""
        return True
