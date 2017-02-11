"""\
wxGlade internal exceptions

@note: Please update the documentation accordingly after changing this file.

@copyright: 2012-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


class WxgBaseException(Exception):
    "Base class for all individual wxGlade exceptions"
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
    "Raised if output path is not an existing directory"
    msg = _('Output path "%s" must be an existing directory.\nCheck Properties->Application->Output Path.')

class WxgOutputDirectoryNotWritable(WxgBaseException):
    "Raised if the output path exists but the directory is not writable"
    msg = _('Output path "%s" exists but the directory is not writable.\nCheck Properties->Application->Output Path.')

class WxgOutputPathIsDirectory(WxgBaseException):
    "Raised if the output path is a directory when generating a single file"
    msg = _('Output path "%s" can not be a directory when generating a single file.\nCheck Properties->Application->Output Path.')

class WxgOutputPathIsNotDirectory(WxgBaseException):
    "Raised if the output path is not a directory when generating multiple files"
    msg = _('Output path "%s" should be a directory when generating multiple files.\nCheck Properties->Application->Output Path.')

class WxgOutputUnicodeError(WxgBaseException):
    "Raised if the generated code can't converted to the desired encoding"
    msg = _('''Generated source code couldn't converted to encoding %s.\n'''
            '''The source contains invalid characters "%s" from %s to %s.''')

class WxgLispWx3NotSupported(WxgBaseException):
    "Raised if Lisp code for wx 3.0 or newer should be generated"
    msg = _('Generating Lisp code for wxWidgets version %s is not supported.')

class WxgXRCMultipleFilesNotSupported(WxgBaseException):
    "Raised multi file XRC designs."
    msg = _('XRC code cannot be split into multiple files.')

class WxgTemplateCodegenNotPossible(WxgBaseException):
    "Code generation from a template is not possible"
    msg = _('Code generation from a template is not possible.')

class WxgMissingCodeWriter(WxgBaseException):
    "Code writer for the given language is not available"
    msg = _('Code writer for "%s" is not available.')

class WxgReadSourceFileUnicodeError(WxgBaseException):
    "Unicode decode error during reading an already existing source file"
    msg = _('Conversion of the source file %s to Unicode failed.')
