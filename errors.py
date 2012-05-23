"""\
wxGlade internal exceptions

@copyright: 2012 Carsten Grohmann <mail@carstengrohmann.de>
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


class WxgBaseException(Exception):
    """\
    Base class for all individual wxGlade exceptions
    """
    msg = None

    def __str__(self):
        if not self.msg:
            return ""
        if len(self.args) == 1:
            text = self.msg % self.args[0]
        else:
            text = "%s: %s" % (self.msg, self.args)
        return text


class WxgOutputDirectoryNotExist(WxgBaseException):
    """\
    Raised if output path is not an existing directory
    """
    msg = _('Output path "%s" must be an existing directory when generating'
            ' multiple files')


class WxgOutputDirectoryNotWritable(WxgBaseException):
    """\
    Raised if the output path exists but the directory is not writable
    """
    msg = _('Output path "%s" exists but the directory is not writable')


class WxgOutputPathIsDirectory(WxgBaseException):
    """\
    Raised if the output path is a directory when generating a single file
    """
    msg = _('Output path "%s" can not be a directory when generating a '
            'single file')
