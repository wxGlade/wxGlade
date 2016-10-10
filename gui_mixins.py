"""\
Different Mixins

@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import copy
import decorators
import logging
import wx

import config
import compat
import misc


class StylesMixin(object):
    "Class mixin to handle formatting and re-combining styles"

    def cn_f(self, flags):
        """\
        Rearrange and format flags.

        Steps to rearrange:
         1. Split given string using delimiter '|' and remove duplicate flags
         2. Process following style attributes, the styles are processed in
            a alphanumeric order:
              - Rename flags using the 'rename_to' entry
              - Add additional flags using the 'include' entry (soft
                requirement)
              - Delete flags using the 'exclude' entry
              - Remove unsupported flags using the 'supported_by' entry
              - Add required flags using the 'require' entry (hard
                requirement)
         3. Combine flags using the 'combination' entry
         4. Format single flags with L{wcodegen.BaseLanguageMixin.cn()} if
            L{wcodegen.BaseLanguageMixin.format_flags} is True
         5. Sort and recombine flags using
            L{wcodegen.BaseLanguageMixin.tmpl_flag_join}

        The style details are described in L{config.widget_config}. The
        access to the details is only available in widget writer instances.

        Sometime the flag is a digit as a string. The function doesn't
        process such kind of flags. It returns this flags unchanged.

        Example C++::
            >>> self.cn_f('wxLC_REPORT|wxSUNKEN_BORDER')
            'wxLC_REPORT|wxSUNKEN_BORDER'

        Example Python::
            >>> self.cn_f('wxLC_REPORT|wxSUNKEN_BORDER')
            'wxLC_REPORT | wxSUNKEN_BORDER'

        @param flags: wxWidget styles joined by '|'
        @type flags:  str

        @rtype: str

        @see: cn()
        @see: format_flags
        @see: tmpl_flag_join
        @see: L{config.widget_config}
        """
        assert isinstance(flags, compat.basestring)
        if flags.isdigit():
            return flags

        # split flags to set first
        oflags = flags
        flags = set(flags.split('|'))

        # check for non-supported, renamed flags and ...
        if self.style_defs:
            flags = self.process_styles(flags)
            flags = self.combine_styles(flags)

        if hasattr(self, 'cn') and getattr(self, 'format_flags', True):
            flags = [self.cn(f) for f in flags]

        tmpl_flag_join = getattr(self, 'tmpl_flag_join', '|')
        flags = tmpl_flag_join.join(sorted(flags))

        if hasattr(self, 'klass'):
            logging.debug('cn_f(%s:%s): %s', getattr(self, 'klass'), oflags, flags)
        else:
            logging.debug('cn_f(%s): %s', oflags, flags)

        return flags

    @decorators.memoize
    def _get_widget_styles_defs(self, widget_name):
        """
        Logic of L{_get_style_defs()} but extracted for cache decorator.

        @note: The styles are copied using a deep-copy to prevent changing
               original data accidentally.

        @param widget_name: Widget name e.g. 'wxCheckBox'
        @type widget_name: str

        @return: A joined copy of the generic styles and widget specific
                 styles.
        @rtype: dict
        """
        styles = {}
        # Use always a deep-copy to prevent changing original data
        try:
            styles = copy.deepcopy(config.widget_config['generic_styles'])
            styles.update(config.widget_config[widget_name]['style_defs'])
        except KeyError:
            pass

        return styles

    def _get_style_defs(self):
        """\
        Return all styles related to this widget. This includes generic
        styles from L{config.widget_config}.

        The implementation has moved to L{_get_widget_styles_defs()} to use a
        simple cache decorator instead of using an own cache implementation.

        @rtype: dict
        @see: L{config.widget_config}
        @see: L{_get_widget_styles_defs()}
        """
        return self._get_widget_styles_defs(getattr(self, 'klass', None))

    style_defs = property(_get_style_defs)

    def process_styles(self, flags):
        """\
        Process the style attributes 'rename_to', 'include', 'exclude',
        'supported_by' and 'require'.

        The documentation of L{cn_f()} contains more details of the flag
        handling process.

        @param flags: Flags to process
        @type flags:  set

        @return: Processed flags
        @rtype: set

        @see: L{config.widget_config}
        """
        assert isinstance(flags, set)

        # processing empty set()s causes later trouble with
        # set([<filled>]) >= set()
        if not flags:
            return flags

        for flag in flags.copy():
            try:
                flags.add(self.style_defs[flag]['rename_to'])
                flags.remove(flag)
            except (AttributeError, KeyError):
                pass

        add = set()
        remove = set()
        flag_list = list(flags)
        flag_list.sort()

        for required_by in flag_list:
            if required_by in remove:
                continue
            try:
                add |= self.style_defs[required_by]['include']
            except (AttributeError, KeyError):
                pass

            try:
                remove |= self.style_defs[required_by]['exclude']
            except (AttributeError, KeyError):
                pass

            try:
                supported_by = self.style_defs[required_by]['supported_by']
                major = 'wx%d' % self.codegen.for_version[0]
                detailed = 'wx%d%d' % self.codegen.for_version
                if not (major in supported_by or detailed in supported_by):
                    remove.add(required_by)
            except (AttributeError, KeyError):
                pass

            try:
                for required in self.style_defs[required_by]['require']:
                    if required in remove:
                        remove.add(required_by)
                    else:
                        add.add(required)
            except (AttributeError, KeyError):
                pass

        # drop flags from add if they should be removed
        add -= remove

        flags |= add
        flags -= remove

        return flags

    def combine_styles(self, flags):
        """\
        Combine flags (attribute 'combination') and remove flags that are
        parts of other flags already.

        @param flags: Flags to combine and reduce
        @type flags:  set

        @return: Processed flags
        @rtype: set

        @see: L{config.widget_config}
        """
        # processing empty set()s causes later trouble with
        # set([<filled>]) >= set()
        if not flags:
            return flags

        # combined flags: replace children by parent flag
        for style in self.style_defs:
            try:
                if self.style_defs[style]['combination'] <= flags:
                    flags -= self.style_defs[style]['combination']
                    flags.add(style)
            except KeyError:
                pass

        # combined flags: remove flags that are part of other flags already
        for flag in flags.copy():
            # ignore already eliminated flags
            if flag not in flags:
                continue
            try:
                flags -= self.style_defs[flag]['combination']
            except (KeyError, TypeError):
                pass

        return flags



class BitmapMixin(object):
    "Class mixin to create wxBitmap instances from the given statement"

    bitmap_tooltip_text = _(
        'Choice a bitmap to show.\n\nYou can either select a file or you '
        'can specify the bitmap using hand-crafted statements with the '
        'prefixes "art:", "code:", "empty:" or "var:".\nThe wxGlade '
        'documentation describes how to write such statements.')
    "Detailed tooltip string to show with each bitmap property."

    def get_preview_obj_bitmap(self, bitmap=None):
        """\
        Create a wxBitmap instance from the given statement.

        If not statement is given, the instance variable named "bitmap" is used.

        @param bitmap: Bitmap definition
        @type bitmap: str | None

        @see: L{get_preview_obj_artprovider()}
        @see: L{get_preview_obj_emptybitmap()}

        @rtype: wx.Bitmap | wx.EmptyBitmap
        """
        if bitmap is None:
            bitmap = getattr(self, 'bitmap', None)

        if not bitmap:
            return wx.EmptyBitmap(1, 1)

        if bitmap.startswith('var:') or bitmap.startswith('code:'):
            return wx.EmptyBitmap(16, 16)
        elif bitmap.startswith('empty:'):
            return self.get_preview_obj_emptybitmap(bitmap)
        elif bitmap.startswith('art:'):
            return self.get_preview_obj_artprovider(bitmap)
        else:
            bitmap = misc.get_relative_path(bitmap)
            return wx.Bitmap(bitmap, wx.BITMAP_TYPE_ANY)

    def get_preview_obj_artprovider(self, bitmap):
        """\
        Create a wxBitmap instance from the given statement using
        wxArtProvider.

        @note: Preview shows only wxART_* resources.

        @param bitmap: Bitmap definition
        @type bitmap: str | None

        @rtype: wx.Bitmap | wx.EmptyBitmap

        @see: L{wcodegen.BaseWidgetWriter.get_inline_stmt_artprovider()}
        """
        # keep in sync with BitmapMixin.get_inline_stmt_artprovider()
        art_id = 'wxART_ERROR'
        art_client = 'wxART_OTHER'
        size = wx.DefaultSize

        # art:ArtID,ArtClient
        # art:ArtID,ArtClient,width,height
        try:
            content = bitmap[4:]
            elements = [item.strip() for item in content.split(',')]
            if len(elements) == 2:
                art_id, art_client = elements
            elif len(elements) == 4:
                art_id, art_client, width, height = elements
                size = wx.Size(int(width), int(height))
            else:
                raise ValueError

        except (ValueError, TypeError):
            self._logger.warn( 'Malformed statement to create a bitmap via wxArtProvider(): %s', bitmap )

        # show wx art resources only
        if not art_id.startswith('wx'):
            art_id = 'wxART_HELP'
        if not art_client.startswith('wx'):
            art_client = 'wxART_OTHER'

        return wx.ArtProvider.GetBitmap(
            self.wxname2attr(self.codegen.cn(art_id)),
            self.wxname2attr(self.codegen.cn(art_client)),
            size
        )

    def get_preview_obj_emptybitmap(self, bitmap):
        """\
        Create an empty wxBitmap instance from the given statement.

        @param bitmap: Bitmap definition
        @type bitmap: str | None

        @rtype: wx.Bitmap | wx.EmptyBitmap

        @see: L{wcodegen.BaseWidgetWriter.get_inline_stmt_emptybitmap()}
        """
        # keep in sync with BaseWidgetWriter.get_inline_stmt_emptybitmap()
        width = 16
        height = 16
        try:
            size = bitmap[6:]
            width, height = [int(item.strip()) for item in size.split(',', 1)]
        except ValueError:
            self._logger.warn( 'Malformed statement to create an empty bitmap: %s', bitmap )
        return wx.EmptyBitmap(max(1, width), max(1, height))
