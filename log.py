"""
Functions and classes to record and print out log messages.

This module provides a own logger class as well as specific functions to improve Pythons logging facility.

wxGlade uses the python logging instance with three log handler attached.

The first handler StringHandler is used to cache messages later displaying calling getBufferAsList() or
getBufferAsString().

The second handler logging.StreamHandler to print error messages to sys.stderr.

The third handler logging.FileHandler writes all messages into a file. This
behaviour is useful to store logged exceptions permanently.

@todo: Integrate Unicode logging fix.

@copyright: 2013-2016 Carsten Grohmann
@copyright: 2017-2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import datetime
import logging, logging.handlers
import os, sys, traceback

try:
    _nameToLevel = logging._levelNames
except:
    _nameToLevel = logging._nameToLevel


import config, compat


stringLoggerInstance = None  # Reference to the active StringHandler instance

exception_orig = logging.exception # Reference to the original implementation of logging.exception


class StringHandler(logging.handlers.MemoryHandler):
    "Stores the log records as a list of strings."

    storeAsUnicode = True  # Store the log records as unicode strings

    # Encoding of all character strings
    # The default encoding is used to convert character strings into unicode strings.
    encoding = sys.stdout and sys.stdout.encoding or sys.getfilesystemencoding()

    def __init__(self, storeAsUnicode=True):
        """Constructor

        storeAsUnicode: Store recorded log records as unicode strings"""
        self.buffer = []  # The message buffer itself
        #logging.handlers.MemoryHandler.__init__(self, sys.maxint, 99)
        logging.handlers.MemoryHandler.__init__(self, 2**31-1, 99)
        self.storeAsUnicode = storeAsUnicode

    def _toUnicode(self, msg):
        "Convert a non unicode string into a unicode string"
        # return msg if is already a unicode string or None
        if msg is None or isinstance(msg, compat.unicode):
            return msg

        # convert character string into a unicode string
        if not isinstance(msg, compat.unicode):
            msg = msg.decode(self.encoding, 'replace')
        return msg

    def getBufferAsList(self, clean=True):
        "Returns all buffered messages"
        self.acquire()
        try:
            messages = self.buffer[:]
            if clean:
                self.flush()
        finally:
            self.release()
        return messages

    def getBufferAsString(self, clean=True):
        "Returns all buffered messages"
        msg_list = self.getBufferAsList(clean)
        if self.storeAsUnicode:
            return u'\n'.join(msg_list)
        return '\n'.join(msg_list)

    def emit(self, record):
        "Emit a record, i.e. add a formatted log record to the buffer."
        msg = self.format(record)
        if self.storeAsUnicode:
            msg = self._toUnicode(msg)
        self.buffer.append(msg)
        if self.shouldFlush(record):
            self.flush()

    def flush(self):
        "Empty the buffer"
        self.buffer = []



class ExceptionFormatter(logging.Formatter):
    "Extended formatter to include more exception details automatically"

    def formatException(self, ei):
        """Returns a detailed exception

        ei: Tuple or list of exc_type, exc_value, exc_tb"""
        exc_tb = ei[2]
        exc_type = ei[0]
        exc_value = ei[1]
        msg = []
        try:
            try:
                # log exception details
                now = datetime.datetime.now().isoformat()
                py_version = getattr(config, 'py_version', 'not found')
                wx_version = getattr(config, 'wx_version', 'not found')
                platform = getattr(config, 'platform', 'not found')
                app_version = getattr(config, 'version', 'not found')

                msg.append('An unexpected error occurred!\n')
                msg.append('\n')
                msg.append('Exception type:      %s\n' % exc_type)
                msg.append('Exception details:   %s\n' % exc_value)
                if exc_tb:
                    msg.append('\nApplication stack traceback:\n')
                    msg += traceback.format_tb(exc_tb)
                msg.append('\n')
                msg.append('Date and time:       %s\n' % now)
                msg.append('Python version:      %s\n' % py_version)
                msg.append('wxPython version:    %s\n' % wx_version)
                msg.append('wxWidgets platform:  %s\n' % platform)
                msg.append('wxGlade version:     %s\n' % app_version)
                msg.append('\n')

            except Exception as e:
                # This code should NEVER be executed!
                if config.debugging: raise
                logging.error('An exception has been raised inside the exception handler: %s', e)
                sys.exit(1)

        # delete local references of trace backs or part of them  to avoid circular references
        finally:
            del ei, exc_tb, exc_type, exc_value

        if msg[-1][-1] == "\n":
            msg[-1] = msg[-1][:-1]
        return "".join(msg)



def init(filename='wxglade.log', encoding='utf-8', level=None):
    """Initialise the logging facility

    Initialise and configure the logging itself as well as the handlers described above.
    Our own exception handler will be installed finally.
    The file logger won't be instantiate if not file name is given.

    filename: Name of the log file
    encoding: Encoding of the log file
    level:    Verbosity of messages written in log file e.g. "INFO"

    see: StringHandler, stringLoggerInstance, installExceptionHandler()"""
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
    if config.debugging:
        console_logger.setLevel(logging.DEBUG)
    else:
        console_logger.setLevel(logging.INFO)
    console_logger.setFormatter(default_formatter)
    logger.addHandler(console_logger)

    # instantiate file handler
    if filename:
        log_directory = os.path.dirname(filename)
        if not os.path.isdir(log_directory):
            logging.warning(_('Logging directory "%s" does not exists. Skip file logger initialisation!'),log_directory)
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

    # Install own exception handler at the end because it can raise a debug messages. This debug messages triggers the
    # creation of a default StreamHandler if no log handler exists.
    # There is a period of time with no log handler during this log initialisation.
    if not config.debugging:
        installExceptionHandler()


def deinit():
    "Reactivate system exception handler; see deinstallExceptionHandler()"
    deinstallExceptionHandler()


def setDebugLevel():
    "Set the log level to DEBUG for all log handlers"
    logger = logging.getLogger()
    logger.setLevel(logging.DEBUG)


def getBufferAsList(clean=True):
    """Returns all messages buffered by stringLoggerInstance as list of strings

    clean: Clean the internal message buffer

    see: StringHandler.getBufferAsList(), stringLoggerInstance"""
    return stringLoggerInstance.getBufferAsList(clean)


def getBufferAsString(clean=True):
    """Returns all messages buffered by stringLoggerInstance as string

    clean: Clean the internal message buffer
    see StringHandler.getBufferAsString(), stringLoggerInstance"""
    return stringLoggerInstance.getBufferAsString(clean)


def flush():
    """Empty the buffer of the stringLoggerInstance.

    see: StringHandler.flush(), stringLoggerInstance"""
    stringLoggerInstance.flush()


def installExceptionHandler():
    """Install own exception handler

    see: deinstallExceptionHandler()"""
    if sys.excepthook == exceptionHandler:
        logging.debug( _('The exception handler has been installed already.') )
        return
    sys.excepthook = exceptionHandler


def deinstallExceptionHandler():
    """Restore the original exception handler from sys.__excepthook__.

   see: installExceptionHandler()"""
    sys.excepthook = sys.__excepthook__


def exceptionHandler(exc_type, exc_value, exc_tb):
    """Log detailed information about uncaught exceptions. The exception information will be cleared after that.

    exc_type:  Type of the exception (normally a class object)
    exc_value: The "value" of the exception
    exc_tb:    Call stack of the exception"""
    logging.error( _("An unhandled exception occurred"), exc_info=(exc_type, exc_value, exc_tb) )
    if compat.PYTHON2: sys.exc_clear()


def getMessage(self):
    """Return the message for this LogRecord.

    Return the message for this LogRecord after merging any user-supplied arguments with the message.
    This specific version tries to handle Unicode user-supplied arguments."""
    msg = self.msg
    if not isinstance(msg, compat.basestring):
        try:
            msg = compat.unicode(msg)
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
