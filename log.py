"""
Functions and classes to record and print out log messages.

This module provides a own logger class as well as specific functions to
improve Pythons logging facility.

wxGlade uses the python logging instance with three log handler attached.

The first handler L{StringHandler} is used to cache messages later
displaying calling getBufferAsList() or getBufferAsString().

The second handler C{logging.StreamHandler} to print error messages to
sys.stderr.

The third handler C{logging.FileHandler} writes all messages into a file. This
behaviour is useful to store logged exceptions permanently.

@note: Python versions older then 2.6.6 (released 24th August 2010) contains
logging implementation that are not Unicode aware.
The Python bug report #8924 contains the details. A fix has been committed
in revision 81919 (27.12.2010) in the public Python repository.

@todo: Integrate Unicode logging fix.

@copyright: 2013-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import datetime
import inspect
import locale
import logging
import logging.handlers
import os
import pprint
import sys
import types

try:
    import StringIO
    _nameToLevel = logging._levelNames
except:
    import io as StringIO
    _nameToLevel = logging._nameToLevel


import config, compat


stringLoggerInstance = None
"Reference to the active L{StringHandler} instance"

exception_orig = logging.exception
"Reference to the original implementation of C{logging.exception}"


class StringHandler(logging.handlers.MemoryHandler):
    """\
    Stores the log records as a list of strings.

    @ivar buffer: The message buffer itself
    @type buffer: list[str]
    """

    storeAsUnicode = True
    """\
    Stores the log records as unicode strings

    @type: bool
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
        self.buffer = []
        #logging.handlers.MemoryHandler.__init__(self, sys.maxint, 99)
        logging.handlers.MemoryHandler.__init__(self, 2**31-1, 99)
        self.storeAsUnicode = storeAsUnicode

    def _toUnicode(self, msg):
        """\
        Convert a non unicode string into a unicode string

        @return: Unicode string
        @see: L{self.encoding}
        """
        # return msg if is already a unicode string or None
        if msg is None or isinstance(msg, compat.unicode):
            return msg

        # convert character string into a unicode string
        if not isinstance(msg, compat.unicode):
            msg = msg.decode(self.encoding, 'replace')
        return msg

    def getBufferAsList(self, clean=True):
        """\
        Returns all buffered messages

        @param clean: Clean the internal message buffer
        @return:      Message buffer
        @rtype:       list[str]

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
        "Empty the buffer"
        self.buffer = []

# end of class StringHandler


class ExceptionFormatter(logging.Formatter):
    "Extended formatter to include more exception details automatically"

    def formatException(self, ei):
        """
        Returns a detailed exception

        @param ei: Tuple or list of exc_type, exc_value, exc_tb
        @return: Formatted exception
        @rtype:  String
        """
        context = None
        exc_tb = ei[2]
        exc_type = ei[0]
        exc_value = ei[1]
        filename = None
        frame = None
        func_args = None
        func_name = None
        index = None
        lineno = None
        loc_langcode = None
        loc_encoding = None
        loc_filesystem = None
        sio = StringIO.StringIO()
        stack_level = 0
        stack_list = []
        var = None
        var_name = None
        var_type = None
        var_value = None
        try:
            try:
                # log exception details
                now = datetime.datetime.now().isoformat()
                py_version = getattr(config, 'py_version', 'not found')
                wx_version = getattr(config, 'wx_version', 'not found')
                platform = getattr(config, 'platform', 'not found')
                app_version = getattr(config, 'version', 'not found')
                loc_langcode, loc_encoding = locale.getlocale()
                loc_filesystem = sys.getfilesystemencoding()

                sio.write('An unexpected error occurred!\n')
                sio.write('\n')
                sio.write('Date and time:       %s\n' % now)
                sio.write('Python version:      %s\n' % py_version)
                sio.write('wxPython version:    %s\n' % wx_version)
                sio.write('wxWidgets platform:  %s\n' % platform)
                sio.write('wxGlade version:     %s\n' % app_version)
                sio.write('Language code:       %s\n' % loc_langcode)
                sio.write('Encoding:            %s\n' % loc_encoding)
                sio.write('Filesystem encoding: %s\n' % loc_filesystem)
                sio.write('\n')
                sio.write('Exception type:      %s\n' % exc_type)
                sio.write('Exception details:   %s\n' % exc_value)
                sio.write('Application stack trace:\n')

                # leave the exception handler if no traceback is available
                if not exc_tb:
                    return

                # get stack frames
                stack_list = inspect.getinnerframes(exc_tb, 7)
                stack_list.reverse()
                stack_level = -1

                for frame, filename, lineno, func_name, context, index in stack_list:

                    stack_level += 1
                    try:
                        func_args = inspect.formatargvalues( *inspect.getargvalues(frame) )
                    except:
                        # sometimes frames contains non-printable values:
                        # e.g. TypeError: __repr__ returned non-string
                        # (type NoneType)
                        func_args = '(<unknown arguments>)'

                    msg = 'Stack frame at level %d' % stack_level
                    sio.write('%s\n' % msg)
                    msg = '=' * len(msg)
                    sio.write('%s\n' % msg)
                    sio.write('  File "%s", line %d\n' % (filename, lineno))
                    sio.write('  Function "%s%s"\n' % (func_name, func_args))
                    sio.write('  Source code context:\n')

                    pos = 0
                    for line in context:
                        line = line.rstrip()
                        if pos == index:
                            sio.write('  ->  %s\n' % line)
                        else:
                            sio.write('      %s\n' % line)
                        pos += 1

                    if frame.f_locals:
                        sio.write('  Local variables:\n')
                        for var_name in frame.f_locals:
                            # convert name and value to ascii characters
                            var = frame.f_locals[var_name]
                            var_type = type(var)
                            if isinstance(var_type, compat.unicode):
                                var_value = frame.f_locals[var_name]
                                var_value = var_value.encode('unicode_escape')
                            elif isinstance(var_type, compat.basestring):
                                var_value = frame.f_locals[var_name]
                                var_value = var_value.encode('string-escape')
                            else:
                                try:
                                    var_value = pprint.pformat( frame.f_locals[var_name] )
                                    var_value = var_value.encode( 'ascii', 'replace')
                                except:
                                    var_value = '<unknown content>'
                            sio.write('  -> %s (%s): %s\n' % ( var_name, var_type, var_value) )
                    else:
                        sio.write('  No local variables\n')
                    sio.write('\n')
            except Exception as e:
                # This code should NEVER be executed!
                logging.error('An exception has been raised inside the exception handler: %s', e)
                sys.exit(1)

        # delete local references of trace backs or part of them  to avoid
        # circular references
        finally:
            del context
            del ei
            del exc_tb
            del exc_type,
            del exc_value,
            del filename
            del frame
            del func_args
            del func_name
            del index
            del lineno
            del loc_encoding
            del loc_filesystem
            del loc_langcode
            del stack_level
            del stack_list
            del var
            del var_name
            del var_type
            del var_value

        s = sio.getvalue()
        sio.close()
        if s[-1] == "\n":
            s = s[:-1]
        return s

# end of class ExceptionFormatter


def init(filename='wxglade.log', encoding='utf-8', level=None):
    """\
    Initialise the logging facility

    Initialise and configure the logging itself as well as the handlers
    described above.

    Our own exception handler will be installed finally.

    The file logger won't be instantiate if not file name is given.

    @param filename: Name of the log file
    @type filename:  str
    @param encoding: Encoding of the log file
    @type encoding:  str
    @param level:    Verbosity of messages written in log file e.g. "INFO"
    @type level:     str

    @see: L{StringHandler}
    @see: L{stringLoggerInstance}
    @see: L{installExceptionHandler()}
    """
    default_formatter = ExceptionFormatter('%(levelname)-8s: %(message)s')
    file_formatter = ExceptionFormatter( '%(asctime)s %(name)s %(levelname)s: %(message)s' )
    logger = logging.getLogger()

    # check for installed handlers and remove them
    for handler in logger.handlers[:]:
        logger.removeHandler(handler)

    # set newline sequence
    if os.name == 'nt':
        logging.StreamHandler.terminator = '\r\n'
    elif os.name == 'mac':
        logging.StreamHandler.terminator = '\r'
    else:
        logging.StreamHandler.terminator = '\n'

    # instantiate console handler
    console_logger = logging.StreamHandler()
    console_logger.setLevel(logging.INFO)
    console_logger.setFormatter(default_formatter)
    logger.addHandler(console_logger)

    # instantiate file handler
    if filename:
        log_directory = os.path.dirname(filename)
        if not os.path.isdir(log_directory):
            logging.warning(_('Logging directory "%s" does not exists. Skip '
                              'file logger initialisation!'), log_directory)
        else:
            file_logger = logging.handlers.RotatingFileHandler( filename, maxBytes=100 * 1024,
                                                                encoding=encoding, backupCount=1 )
            file_logger.setFormatter(file_formatter)
            file_logger.setLevel(logging.NOTSET)
            logger.addHandler(file_logger)

    # instantiate string handler
    string_logger = StringHandler(storeAsUnicode=False)
    string_logger.setLevel(logging.WARNING)
    string_logger.setFormatter(default_formatter)
    logger.addHandler(string_logger)

    # store string logger globally
    global stringLoggerInstance
    stringLoggerInstance = string_logger

    # don't filter log levels in root logger
    logger.setLevel(logging.NOTSET)

    # Set log level for root logger only
    if level:
        if level.upper() in _nameToLevel:                 # pylint: disable=W0212
            logger.setLevel(_nameToLevel[level.upper()])  # pylint: disable=W0212
        else:
            logging.warning( _('Invalid log level "%s". Use "WARNING" instead.'), level.upper() )
            logger.setLevel(logging.WARNING)
    else:
        logger.setLevel(logging.NOTSET)

    # Install own exception handler at the end because it can raise a debug
    # messages. This debug messages triggers the creation of a default
    # StreamHandler if no log handler exists.
    # There is a period of time with no log handler during this log
    # initialisation.
    installExceptionHandler()


def deinit():
    """\
    Reactivate system exception handler

    @see: L{deinstallExceptionHandler()}
    """
    deinstallExceptionHandler()


def setDebugLevel():
    "Set the log level to DEBUG for all log handlers"
    logger = logging.getLogger()
    logger.setLevel(logging.DEBUG)


def getBufferAsList(clean=True):
    """\
    Returns all messages buffered by L{stringLoggerInstance}.

    @param clean: Clean the internal message buffer
    @return:      Message buffer
    @rtype:       list[str]

    @see: L{StringHandler.getBufferAsList()}
    @see: L{stringLoggerInstance}
    """
    return stringLoggerInstance.getBufferAsList(clean)


def getBufferAsString(clean=True):
    """\
    Returns all messages buffered by L{stringLoggerInstance}.

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

    @see: L{deinstallExceptionHandler()}
    """
    if sys.excepthook == exceptionHandler:
        logging.debug(
            _('The exception handler has been installed already.'),
            )
        return
    sys.excepthook = exceptionHandler


def deinstallExceptionHandler():
    """\
   Restore the original exception handler from C{sys.__excepthook__}.

   @see: L{installExceptionHandler()}
    """
    sys.excepthook = sys.__excepthook__


def exceptionHandler(exc_type, exc_value, exc_tb):
    """\
    Log detailed information about uncaught exceptions. The exception
    information will be cleared after that.

    @param exc_type:  Type of the exception (normally a class object)
    @param exc_value: The "value" of the exception
    @param exc_tb:    Call stack of the exception
    """
    logging.error( _("An unhandled exception occurred"), exc_info=(exc_type, exc_value, exc_tb) )
    sys.exc_clear()


def getMessage(self):
    """\
    Return the message for this LogRecord.

    Return the message for this LogRecord after merging any user-supplied
    arguments with the message.

    This specific version tries to handle Unicode user-supplied arguments.
    """
    if not hasattr(types, "UnicodeType"):  # if no unicode support... # XXX
        msg = str(self.msg)
    else:
        msg = self.msg
        if not isinstance(msg, types.StringTypes):
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

# inject own (improved) function
logging.LogRecord.getMessage = getMessage
