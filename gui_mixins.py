"""\
Different Mixins

@copyright: 2014-2016 Carsten Grohmann
@copyright: 2017-2018 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import copy, decorators, logging
import wx

import config, compat, misc


class StylesMixin(object):
    "Class mixin to handle formatting and re-combining styles"

    def cn_f(self, flags):
        """Rearrange and format flags into a string.

        Steps to rearrange:
         1. Split given string using delimiter '|' and remove duplicate flags
         2. Process following style attributes, the styles are processed in a alphanumeric order:
              - Rename flags using the 'rename_to' entry
              - Add additional flags using the 'include' entry (soft requirement)
              - Delete flags using the 'exclude' entry
              - Remove unsupported flags using the 'supported_by' entry
              - Add required flags using the 'require' entry (hard requirement)
         3. Combine flags using the 'combination' entry
         4. Format single flags with wcodegen.BaseLanguageMixin.cn() if wcodegen.BaseLanguageMixin.format_flags is True
         5. Sort and recombine flags using wcodegen.BaseLanguageMixin.tmpl_flag_join

        The style details are described in config.widget_config.
        The access to the details is only available in widget writer instances.

        Sometime the flag is a digit as a string. The function doesn't process such kind of flags.
        It returns these flags unchanged.

        Example C++::
            >>> self.cn_f('wxLC_REPORT|wxSUNKEN_BORDER')
            'wxLC_REPORT|wxSUNKEN_BORDER'

        Example Python::
            >>> self.cn_f('wxLC_REPORT|wxSUNKEN_BORDER')
            'wxLC_REPORT | wxSUNKEN_BORDER'

        flags: string with wxWidget styles joined by '|'
        
        see: cn(), format_flags, tmpl_flag_join, config.widget_config"""
        assert isinstance(flags, compat.basestring)

        if flags.isdigit(): return flags

        # split flags to set first
        oflags = flags
        flags = set(flags.split('|'))

        # check for non-supported, renamed flags and ...
        if self.style_defs:
            flags = self.process_styles(flags)
            flags = self.combine_styles(flags)

        if hasattr(self, 'cn') and getattr(self, 'format_flags', True):
            flags = [self.cn(f) for f in flags if f]

        tmpl_flag_join = getattr(self, 'tmpl_flag_join', '|')
        flags = tmpl_flag_join.join(sorted(flags))

        return flags

    @decorators.memoize
    def _get_widget_styles_defs(self, widget_name):
        """Logic of _get_style_defs() but extracted for cache decorator.

        note: The styles are copied using a deep-copy to prevent changing original data accidentally.

        widget_name: Widget name e.g. 'wxCheckBox'
        widget_name: Widget name e.g. 'wxCheckBox'

        returns a joined copy of the generic styles and widget specific styles as dict"""
        styles = {}
        # Use always a deep-copy to prevent changing original data
        try:
            styles = copy.deepcopy(config.widget_config['generic_styles'])
            styles.update(config.widget_config[widget_name]['style_defs'])
        except KeyError:
            pass

        return styles

    def _get_style_defs(self):
        """Return all styles related to this widget as dict. This includes generic styles from config.widget_config.

        The implementation has moved to _get_widget_styles_defs() to use a
        simple cache decorator instead of using an own cache implementation.

        see: config.widget_config, _get_widget_styles_defs()"""
        return self._get_widget_styles_defs(getattr(self, 'klass', None))

    style_defs = property(_get_style_defs)

    def process_styles(self, flags):
        """Process the style attributes 'rename_to', 'include', 'exclude', 'supported_by' and 'require'.
        Returns processed flags as set.

        flags: Flags to process as set

        see: The documentation of cn_f() contains more details of the flag handling process.
             config.widget_config"""
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
        """Combine flags (attribute 'combination') and remove flags that are parts of other flags already.
        Returns processed flags as set.

        flags: Flags to combine and reduce as set

        see: config.widget_config"""
        # processing empty set()s causes later trouble with set([<filled>]) >= set()
        if not flags:
            return flags

        # combined flags: replace children by parent flag
        for style in self.style_defs:
            try:
                if self.style_defs[style]['combination'] <= flags:
                    flags -= self.style_defs[style]['combination']
                    style = self.style_defs[style].get('rename_to', style)
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
    bitmap_tooltip_text = _('Choose a bitmap to show.\n\nYou can either drop or select a file or you can specify '
                            'the bitmap using hand-crafted statements with the prefixes '
                            '"art:", "code:", "empty:" or "var:".\n\n'
                            'Double-click to see the wxGlade documentation how to write such statements.')

    def get_preview_obj_bitmap(self, bitmap=None):
        """Create a wx.Bitmap or wx.EmptyBitmap from the given statement.
        If no statement is given, the instance variable named "bitmap" is used.

        bitmap: Bitmap definition (str or None)

        see: get_preview_obj_artprovider(), get_preview_obj_emptybitmap()"""
        if bitmap is None:
            bitmap = getattr(self, 'bitmap', None)

        if not bitmap:
            return compat.wx_EmptyBitmap(1, 1)

        if bitmap.startswith('var:') or bitmap.startswith('code:'):
            return compat.wx_EmptyBitmap(16, 16)
        elif bitmap.startswith('empty:'):
            return self.get_preview_obj_emptybitmap(bitmap)
        elif bitmap.startswith('art:'):
            return self.get_preview_obj_artprovider(bitmap)
        else:
            bitmap = misc.get_absolute_path(bitmap)
            return wx.Bitmap(bitmap, wx.BITMAP_TYPE_ANY)

    def get_preview_obj_artprovider(self, bitmap):
        """Create a wxBitmap or wx.EmptyBitmap from the given statement using wxArtProvider.
        (note: Preview shows only wxART_* resources.)

        bitmap: Bitmap definition (str or None)

        see: Lwcodegen.BaseWidgetWriter.get_inline_stmt_artprovider()"""
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
        if not art_id.startswith('wx'):     art_id     = 'wxART_HELP'
        if not art_client.startswith('wx'): art_client = 'wxART_OTHER'

        return wx.ArtProvider.GetBitmap( self.wxname2attr(self.codegen.cn(art_id)),
                                         self.wxname2attr(self.codegen.cn(art_client)), size )

    def get_preview_obj_emptybitmap(self, bitmap):
        """Create an empty wx.EmptyBitmap instance from the given statement.

        bitmap: Bitmap definition as str or None

        see: wcodegen.BaseWidgetWriter.get_inline_stmt_emptybitmap()"""
        # keep in sync with BaseWidgetWriter.get_inline_stmt_emptybitmap()
        width = 16
        height = 16
        try:
            size = bitmap[6:]
            width, height = [int(item.strip()) for item in size.split(',', 1)]
        except ValueError:
            self._logger.warn( 'Malformed statement to create an empty bitmap: %s', bitmap )
        return compat.wx_EmptyBitmap( max(1,width), max(1,height) )
