"""\
Different mixins

@copyright: 2014 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import copy
import logging

import common


class StylesMixin(object):
    """\
    Class mixin to handle formatting and re-combining styles
    """

    _cache_style_defs = {}
    """\
    Cache for style definitions

    Structure::
        _cache_style_defs = {
            'generic_styles': {
                '<style>': {<details},
            },
            '<wx klass>': {
                '<style>': {<details},
            },
        }

    @type: dict[str, dict]
    """

    def init_styles(self, klass=None):
        """\
        Copy the style definitions from L{common.widget_config} and
        initialise  it.

        The initialised styles are stored in L{_cache_style_defs}.
        """

        # All copy operation of styles uses always a deep-copy to prevent
        # changing original data

        # merge generic specific styles
        if not 'generic_styles' in self._cache_style_defs:
            self._cache_style_defs['generic_styles'] = copy.deepcopy(
                common.widget_config['generic_styles'])

            # convert 'combination' from string to set
            genstyles = self._cache_style_defs['generic_styles']
            for style in genstyles.keys():
                try:
                    genstyles[style]['combination'] = set(
                        genstyles[style]['combination'].split('|')
                    )
                except KeyError:
                    pass

        if not klass or klass not in common.widget_config or \
                        klass in self._cache_style_defs:
            return

        # merge widget specific settings
        self._cache_style_defs[klass] = copy.deepcopy(
            self._cache_style_defs['generic_styles'])

        if 'style_defs' in common.widget_config[klass]:
            self._cache_style_defs[klass].update(
                common.widget_config[klass]['style_defs'])

        # convert 'combination' from string to set
        for style in self._cache_style_defs[klass].keys():
            style_def = self._cache_style_defs[klass][style]
            try:
                style_def['combination'] = set(
                    style_def['combination'].split('|'))
            except (AttributeError, KeyError):
                pass

    def cn_f(self, flags):
        """\
        Rearrange and format flags.

        Steps to rearrange:
         1. Split given string using delimiter '|'
         2. Remove duplicate flags
         3. Rename flags using the 'rename_to' entry
            (see L{common.widget_config})
         4. Combine flags using the 'combination' entry
         5. Remove unsupported styles checking 'for_version' attribute of
            L{self.codegen}
         6. Format single flags with L{cn_f()} if L{format_flags} is True
         7. Sort and recombine flags using L{tmpl_flag_join}

        The style details are described in L{common.widget_config}. The
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
        @see: L{common.widget_config}
        """
        assert isinstance(flags, str)
        if flags.isdigit():
            return flags

        # split flags to set first
        oflags = flags
        flags = set(flags.split('|'))

        # check for non-supported, renamed flags and ...
        if self.style_defs:
            flags = self.rename_styles(flags)
            flags = self.combine_styles(flags)
            flags = self.remove_unsupported_styles(flags)

        if hasattr(self, 'cn') and getattr(self, 'format_flags', True):
            flags = [self.cn(f) for f in flags]

        tmpl_flag_join = getattr(self, 'tmpl_flag_join', '|')
        flags = tmpl_flag_join.join(sorted(flags))

        if hasattr(self, 'klass'):
            logging.debug('cn_f(%s:%s): %s',
                          getattr(self, 'klass'), oflags, flags)
        else:
            logging.debug('cn_f(%s): %s', oflags, flags)

        return flags

    def _get_style_defs(self):
        """\
        Return all styles related to this widget. That includes generic
        styles from L{common.widget_config}.

        @rtype: dict
        @see: L{common.widget_config}
        """
        try:
            klass = getattr(self, 'klass', None)
            if klass and klass in self._cache_style_defs:
                styles = self._cache_style_defs[self.klass]
            else:
                styles = self._cache_style_defs['generic_styles']
        except (AttributeError, KeyError):
            styles = {}
        return styles

    style_defs = property(_get_style_defs)

    def combine_styles(self, flags):
        """\
        Combine flags (attribute 'combination') and remove flags that are
        parts of other flags already.

        @param flags: Flags to combine and reduce
        @type flags:  set

        @return: Processed flags
        @rtype: set

        @see: L{common.widget_config}
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

    def remove_unsupported_styles(self, flags):
        """\
        Check given flags for the attribute 'supported_by' and remove remove
        the unsupported flags.

        @param flags: Flags to check for removal.
        @type flags:  set

        @return: Processed flags
        @rtype: set

        @see: L{common.widget_config}
        """
        if not flags:
            return flags

        for flag in flags.copy():
            try:
                # nothing to do if 'supported_by' doesn't exists
                supported_by = self.style_defs[flag]['supported_by']
            except (AttributeError, KeyError):
                continue

            for_version_major = 'wx%d' % self.codegen.for_version[0]
            for_version_detailed = 'wx%d%d' % self.codegen.for_version

            if not (for_version_major in supported_by or
                            for_version_detailed in supported_by):
                flags.remove(flag)

        return flags

    def rename_styles(self, flags):
        """\
        Check given flags for alternative names (attribute 'rename_to') and
        rename those flags.

        @param flags: Flags to check for alternative names
        @type flags:  set

        @return: Processed flags
        @rtype: set

        @see: L{common.widget_config}
        """
        if not flags:
            return flags

        for flag in flags.copy():
            try:
                flags.add(self.style_defs[flag]['rename_to'])
                flags.remove(flag)
            except (AttributeError, KeyError):
                pass

        return flags

# end of class StylesMixin
