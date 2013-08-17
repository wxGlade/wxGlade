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
    pass
    
# end of class BaseCodeWriterBase
