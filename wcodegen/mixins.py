"""\
Different mixins

@copyright: 2014-2015 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import copy
import decorators
import logging

import common


class StylesMixin(object):
    """\
    Class mixin to handle formatting and re-combining styles
    """

    def cn_f(self, flags):
        """\
        Rearrange and format flags.

        Steps to rearrange:
         1. Split given string using delimiter '|'
         2. Remove duplicate flags
         3. Delete flags using the 'delete' entry
         4. Add additional flags using the 'add' entry
         5. Rename flags using the 'rename_to' entry
            (see L{common.widget_config})
         6. Combine flags using the 'combination' entry
         7. Remove unsupported styles checking 'for_version' attribute of
            L{self.codegen}
         8. Format single flags with L{cn_f()} if L{format_flags} is True
         9. Sort and recombine flags using L{tmpl_flag_join}

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
            flags = self.add_styles(flags)
            flags = self.delete_styles(flags)
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
        # Use always a deep-copy to prevent changing original data

        try:
            styles = copy.deepcopy(common.widget_config['generic_styles'])
            styles.update(common.widget_config[widget_name]['style_defs'])
        except KeyError:
            pass

        return styles

    def _get_style_defs(self):
        """\
        Return all styles related to this widget. This includes generic
        styles from L{common.widget_config}.

        The implementation has moved to L{_get_widget_styles_defs()} to use a
        simple cache decorator instead of using an own cache implementation.

        @rtype: dict
        @see: L{common.widget_config}
        @see: L{_get_widget_styles_defs()}
        """
        return self._get_widget_styles_defs(getattr(self, 'klass', None))

    style_defs = property(_get_style_defs)

    def add_styles(self, flags):
        """\
        Check given flags for the attribute 'add' and add required flags.

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
                flags |= self.style_defs[flag]['add']
            except (AttributeError, KeyError):
                pass

        return flags

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

    def delete_styles(self, flags):
        """\
        Check given flags for the attribute 'delete' and remove the excluded
        flags.

        @param flags: Flags to check for removal.
        @type flags:  set

        @return: Processed flags
        @rtype: set

        @see: L{common.widget_config}
        """
        if not flags:
            return flags

        flag_list = list(flags)
        flag_list.sort()
        for flag in flag_list:
            try:
                to_delete = self.style_defs[flag]['delete']
                if to_delete <= flags and flag in flags:
                    flags -= to_delete
            except (AttributeError, KeyError):
                pass

        return flags

    def remove_unsupported_styles(self, flags):
        """\
        Check given flags for the attribute 'supported_by' and remove the
        unsupported flags.

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
