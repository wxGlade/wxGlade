"""
Functions and classes to record and print out log messages.

This module provides a own logger class as well as specific funtions to
improve Pythons logging facility.

wxGlade uses the python logging instance with three log handler attached.

The first handler L{StringHandler} is used to cache messages later
displaying calling getBufferAsList() or getBufferAsString().

The second handler C{logging.StreamHandler} to print error messages to
sys.stderr.

The third handler C{logging.FileHandler} writes all messages into a file. This
behaviour is useful to store logged expections permanently.

@note: Python versions older then 2.6.6 (released 24th August 2010) contains
logging implementation that are not Unicode aware.
The Python bug report #8924 contains the details. A fix has been committed
in revision 81919 (27.12.2010) in the public Python repository.

@todo: Integrate Unicode logging fix.

@copyright: 2013 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import atexit
import cStringIO
import logging
import logging.handlers
import os
import pprint
import sys
import traceback
import types


stringLoggerInstance = None
"""\
Reference to the active StringHandler instance
"""

_orig_exec_handler = None
"""\
Contains the original exception handler

@see: L{installExceptionHandler()}
@see: L{deinstallExceptionHandler()}
"""


class StringHandler(logging.handlers.MemoryHandler):
    """\
    Stores the log records as a list of strings.
    """

    storeAsUnicode = True
    """\
    Stores the log records as unicode strings

    @type: Boolean
    """

    buffer = []
    """\
    The message buffer itself
    """

    encoding = sys.stdout.encoding or sys.getfilesystemencoding()
    """\
    Encoding of all character strings

    The default encoding is used to convert character strings into unicode
    strings.

    @see: L{storeAsUnicode}
    """

    def __init__(self, storeAsUnicode=True):
        """\
        Constructor

        @param storeAsUnicode: Store recorded log records as unicode strings
        """
        logging.handlers.MemoryHandler.__init__(self, sys.maxint, 99)
        self.storeAsUnicode = storeAsUnicode

    def _toUnicode(self, msg):
        """\
        Convert a non unicode string into a unicode string

        @return: Unicode string
        @see: L{self.encoding}
        """
        # return msg if is already a unicode string or None
        if type(msg) in [types.UnicodeType, types.NoneType]:
            return msg

        # convert character string into a unicode string
        if type(msg) != types.UnicodeType:
            msg = msg.decode(self.encoding, 'replace')
        return msg

    def getBufferAsList(self, clean=True):
        """\
        Returns all buffered messages

        @param clean: Clean the internal message buffer
        @return:      Message buffer
        @rtype:       List of strings

        @see: L{getBufferAsString()}
        """
        self.acquire()
        try:
            messages = self.buffer[:]
            if clean:
                self.flush()
        finally:
            self.release()
        return messages

    def getBufferAsString(self, clean=True):
        """\
        Returns all buffered messages

        @param clean: Clean the internal message buffer
        @return:      Concatenated messages
        @rtype:       String

        @see: L{getBufferAsList()}
        """
        msg_list = self.getBufferAsList(clean)
        if self.storeAsUnicode:
            return u'\n'.join(msg_list)
        return '\n'.join(msg_list)

    def emit(self, record):
        """\
        Emit a record.

        Add a formatted log record to the buffer.
        """
        msg = self.format(record)
        if self.storeAsUnicode:
            msg = self._toUnicode(msg)
        self.buffer.append(msg)
        if self.shouldFlush(record):
            self.flush()

    def flush(self):
        """\
        Empty the buffer
        """
        self.buffer = []

# end of class StringHandler


class wxGladeFormatter(logging.Formatter):
    """\
    Extended formatter to include more exception details automatically.
    """

    def formatException(self, ei):
        """
        Format and return the specified exception information as a string.
        """
        sio = cStringIO.StringIO()
        exc_type = ei[0]
        exc_value = ei[1]
        exc_tb = ei[2]
        s = ''
        tb = None
        frame_locals = None
        var = None
        vartype = None
        varvalue = None
        try:
            # log exception details
            sio.write(_('An unexpected error occurs!\n'))
            sio.write(_('Error type:    %s\n') % exc_type)
            sio.write(_('Error details: %s\n') % exc_value)
            sio.write(_('Stack Trace:\n'))
            traceback.print_exception(exc_type, exc_value, exc_tb, None, sio)
            
            if exc_tb:
                tb = exc_tb
                while tb.tb_next:
                    tb = tb.tb_next
                frame_locals = tb.tb_frame.f_locals

                sio.write(_('Local variables of the last stack entry:\n'))
                for varname in frame_locals.keys():
                    # convert variablen name and value to ascii
                    var = frame_locals[varname]
                    vartype = type(var)
                    if vartype == types.UnicodeType:
                        varvalue = frame_locals[varname]
                        varvalue = varvalue.encode('unicode_escape')
                    elif vartype == types.StringType:
                        varvalue = frame_locals[varname]
                        varvalue = varvalue.encode('string-escape')
                    else:
                        varvalue = pprint.pformat(frame_locals[varname])
                        varvalue = varvalue
                    sio.write(_('%s (%s): %s\n') % (varname, vartype, varvalue))

        # delete local references of tracebacks or part of tracebacks
        # to avoid cirular references
        finally:
            del tb
            del frame_locals
            del ei
            del exc_type,
            del exc_value,
            del exc_tb
            del var
            del vartype
            del varvalue

        s = sio.getvalue()
        sio.close()
        if s[-1] == "\n":
            s = s[:-1]
        return s

# end of class wxGladeFormatter

def init(filename='wxglade.log', encoding=None, level=None):
    """\
    Initialise the logging facility

    Initialise and configure the logging itself as well as the handlers
    described above.

    Our own execption handler will be installed finally.

    @param filename: Name of the log file
    @type filename:  String
    @param encoding: Encoding of the log file
    @type encoding:  String
    @param level:    Verbosity of messages written in log file e.g. "INFO"
    @type level:     String

    @see: L{StringHandler}
    @see: L{stringLoggerInstance}
    @see: L{installExceptionHandler()}
    """
    global stringLoggerInstance

    default_formatter = wxGladeFormatter(
        '%(levelname)-8s: %(message)s'
        )
    file_formatter = wxGladeFormatter(
        '%(asctime)s %(name)s %(levelname)s: %(message)s'
        )
    logger = logging.getLogger()

    # set newline sequence
    if os.name == 'nt':
        logging.StreamHandler.terminator = '\r\n'
    elif os.name == 'mac':
        logging.StreamHandler.terminator = '\r'
    else:
        logging.StreamHandler.terminator = '\n'

    # inject own function
    logging.LogRecord.getMessage = getMessage

    # install own exception handler
    installExceptionHandler()

    # instanciate own handler
    console = logging.StreamHandler()
    console.setLevel(logging.INFO)
    console.setFormatter(default_formatter)

    fileLogger = logging.FileHandler(filename, encoding=encoding)
    fileLogger.setFormatter(file_formatter)
    fileLogger.setLevel(logging.NOTSET)

    stringLoggerInstance = StringHandler(storeAsUnicode=False)
    stringLoggerInstance.setLevel(logging.WARNING)
    stringLoggerInstance.setFormatter(default_formatter)

    # check for installed handlers and remove them
    for handler in logger.handlers[:]:
        logger.removeHandler(handler)

    # add new handler
    logger.addHandler(stringLoggerInstance)
    logger.addHandler(console)
    logger.addHandler(fileLogger)
    logger.setLevel(logging.NOTSET)

    # Set loglevel for file logger only
    if level:
        if level.upper() in logging._levelNames:                     # pylint: disable=W0212
            logger.setLevel(logging._levelNames[level.upper()])  # pylint: disable=W0212
        else:
            logging.warning(
                _('Invalid log level "%s". Use "WARNING" instead.'),
                level.upper(),
                )
            logger.setLevel(logging.WARNING)
    else:
        logger.setLevel(logging.NOTSET)

def deinit():
    """\
    Reactivate system exception handler

    @see: L{deinstallExceptionHandler()}
    """
    deinstallExceptionHandler()
    if deinit in atexit._exithandlers:
        atexit._exithandlers.remove(deinit)


def getBufferAsList(clean=True):
    """\
    Returns all messages bufferred by L{stringLoggerInstance}.

    @param clean: Clean the internal message buffer
    @return:      Message buffer
    @rtype:       List of strings

    @see: L{StringHandler.getBufferAsList()}
    @see: L{stringLoggerInstance}
    """
    return stringLoggerInstance.getBufferAsList(clean)


def getBufferAsString(clean=True):
    """\
    Returns all messages bufferred by L{stringLoggerInstance}.

    @param clean: Clean the internal message buffer
    @return:      Concatenated messages
    @rtype:       String

    @see: L{StringHandler.getBufferAsString()}
    @see: L{stringLoggerInstance}
    """
    return stringLoggerInstance.getBufferAsString(clean)


def flush():
    """\
    Empty the buffer of the L{stringLoggerInstance}.

    @see: L{StringHandler.flush()}
    @see: L{stringLoggerInstance}
    """
    stringLoggerInstance.flush()


def installExceptionHandler():
    """\
    Install own exception handler

    The original exception handler is saved in L{_orig_exec_handler}.

    @see: L{_orig_exec_handler}
    @see: L{deinstallExceptionHandler()}
    """
    global _orig_exec_handler
    if _orig_exec_handler:
        logging.debug(
            _('The exception handler has been installed already.'),
            )
        return

    _orig_exec_handler = sys.excepthook
    sys.excepthook = exceptionHandler


def deinstallExceptionHandler():
    """\
   Restore the original exception handler

   The original exception handler has been saved in L{_orig_exec_handler}.

   @see: L{_orig_exec_handler}
   @see: L{installExceptionHandler()}
    """
    global _orig_exec_handler
    if not _orig_exec_handler:
        logging.debug(
            _('The exception handler has not been installed.'
              'Thereby it can not be deinstalled.'),
            )
        return

    sys.excepthook = _orig_exec_handler
    _orig_exec_handler = None


def exceptionHandler(exc_type, exc_value, exc_tb):
    """\
    Logs detailed information about uncatched exceptions

    @param exc_type:  Type of the exception (normally a class object)
    @param exc_value: The "value" of the exception
    @param exc_tb:    Call stack of the exception
    """
    tb = None
    frame_locals = None
    var = None
    vartype = None
    varvalue = None
    try:
        # log exception details
        logging.error(_('An unexpected error occurs!'))
        logging.error(_('Error type:    %s') % exc_type)
        logging.error(_('Error details: %s') % exc_value)
        logging.error(_('Stack Trace:'))
        logging.error(_(u'Stack Trace:'))
        lines = '\n'.join(traceback.format_exception(exc_type, exc_value, exc_tb)).split('\n')
        for line in lines:
            if not line:
                continue
            logging.error(line.decode('ascii', 'replace'))

        if exc_tb:
            tb = exc_tb
            while tb.tb_next:
                tb = tb.tb_next
            frame_locals = tb.tb_frame.f_locals
            
            logging.error(_('Local variables of the last stack entry:'))
            for varname in frame_locals.keys():
                # convert variablen name and value to ascii
                var = frame_locals[varname]
                vartype = type(var)
                if vartype == types.UnicodeType:
                    varvalue = frame_locals[varname]
                    varvalue = varvalue.encode('unicode_escape')
                elif vartype == types.StringType:
                    varvalue = frame_locals[varname]
                    varvalue = varvalue.encode('string-escape')
                else:
                    varvalue = pprint.pformat(frame_locals[varname])
                    varvalue = varvalue
                logging.error(_('%s (%s): %s') % (varname, vartype, varvalue))

    # delete local references of tracebacks or part of tracebacks
    # to avoid cirular references
    finally:
        del tb
        del frame_locals
        del exc_type,
        del exc_value,
        del exc_tb
        del var
        del vartype
        del varvalue


def getMessage(self):
    """\
    Return the message for this LogRecord.

    Return the message for this LogRecord after merging any user-supplied
    arguments with the message.

    This specific version tries to handle Unicode user-supplied arguments.
    """
    if not hasattr(types, "UnicodeType"):  # if no unicode support...
        msg = str(self.msg)
    else:
        msg = self.msg
        if type(msg) not in (types.UnicodeType, types.StringType):
            try:
                msg = str(self.msg)
            except UnicodeError:
                msg = self.msg      # Defer encoding till later
    if self.args:
        try:
            msg = msg % self.args
        except UnicodeError:
            # TODO it's still an hack :-/
            logging.error(_('Unknown format of arguments: %s'), self.args)
        except TypeError:
            # Errors caused by wrong message formatting
            logging.exception(_('Wrong format of a log message'))
    return msg
