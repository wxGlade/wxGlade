"""\
Common code used by all widget code generators

@copyright: 2013 Carsten Grohmann <mail@carstengrohmann.de>
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import logging


class BaseCodeWriterBase(object):
    """\
    Base class for all code writer classes.
    
    @ivar _logger: Instance specific logger
    """

    def __init__(self):
        """\
        Initialise only instance variables using there defaults.
        """
        # initialise instance logger
        self._logger = logging.getLogger(self.__class__.__name__)

        # initialise instance
        pass

# end of class BaseCodeWriterBase

class BaseWidgetCodeWriter(BaseCodeWriterBase):
    """\
    Base class for all widget code writer classes.
    """

    def stmt2list(self, stmt):
        """\
        Split a code statement into a list by conserving tailing newlines

        Example::
            >>> tmpl2list('line 1\\nline 2\\nline 3\\n')
            ['line 1\\n', 'line 2\\n', 'line 3\\n', '\\n']
        
        @param stmt: Code statement
        @type stmt:  String
        
        @rtype: List of strings
        """
        temp = ['%s\n' % line for line in stmt.split('\n')] 
        return temp
        
    
# end of class BaseCodeWriterBase
