"""\
Base classes for custom tag handlers.

The custon tag handlers are called within the XML parsers in L{xml_parse}.

@copyright: 2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import xml.sax.saxutils
import compat


class BaseTagHandler(object):
    """\
    Base for all custom tag handler classes.

    All handler are called during the XML parse process.

    @ivar _content: Tag content
    @type _content: list[Unicode]
    """

    strip_char_data = False

    def __init__(self):
        self._content = []

    def start_elem(self, name, attrs):
        """\
        Start a new element

        @param name: Element name
        @type name: Unicode
        @param attrs: Element attributes
        @type attrs: dict
        """
        pass

    def char_data(self, data):
        """\
        Process tag content

        If returning False, we don't have to call add_property().

        @param data: (Partial) tag content
        @type data: Unicode

        @rtype: bool
        @see: L{self._content}
        see: L{xml_parse.XmlWidgetObject.add_property()}
        """
        assert isinstance(data, compat.unicode)
        if self.strip_char_data:
            data = data.strip()
        if data:
            self._content.append(data)
        return False

    def get_char_data(self):
        """\
        Return the whole content of a tag

        @rtype: Unicode
        """
        if not self._content:
            return u''
        data = "".join(self._content)
        assert isinstance(data, compat.unicode)
        data = xml.sax.saxutils.unescape(data)
        self._content = []
        return data

# end of class BaseTagHandler


class BaseCodeWriterTagHandler(BaseTagHandler):
    """\
    Custom tag handler interface called by L{xml_parse.CodeWriter}
    """

    def end_elem(self, name, code_obj):
        """\
        End the current element

        @param name: Element name
        @type name:  Unicode

        @param code_obj: Object to generate code for
        @type code_obj: CodeObject

        @return: True to remove this handler from XML parser stack
        @rtype: bool
        """
        return True

# end of class BaseCodeWriterTagHandler


class BaseXmlBuilderTagHandler(BaseTagHandler):
    """\
    Custom tag handler interface called by L{xml_parse.XmlWidgetBuilder}
    """

    def end_elem(self, name):
        """\
        End the current element

        @param name: Element name
        @type name:  Unicode

        @return: True to remove this handler from XML parser stack
        @rtype: bool
        """
        return True

# end of class BaseXmlBuilderTagHandler
