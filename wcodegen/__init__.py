"""\
Common code used by all widget code generators

@copyright: 2013-2015 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import config
import misc

import copy
import logging
import os.path
import types
from dialogs import *
from mixins import StylesMixin


class BaseCodeWriter(object):
    """\
    Base for all code writer classes.

    @ivar _logger: Instance specific logger
    """

    def __init__(self):
        """\
        Initialise only instance variables using there defaults.
        """
        # initialise instance logger
        self._logger = logging.getLogger(self.__class__.__name__)

    def __getstate__(self):
        state = self.__dict__.copy()
        del state['_logger']
        return state

    def __setstate__(self, state):
        self.__dict__.update(state)

        # re-initialise logger instance deleted from __getstate__
        self._logger = logging.getLogger(self.__class__.__name__)

# end of class BaseCodeWriter


class BaseLanguageMixin(StylesMixin):
    """\
    Common language specific but generic settings and functions
    """
    comment_sign = ''
    """\
    Character(s) to start a comment (e.g. C{#} for Python and Perl or
    C{;;;} for lisp).

    @type: str
    """

    default_extensions = []
    """\
    Default extensions for generated files: a list of file extensions

    @type: list[str]
    """

    format_flags = False
    """\
    Format single flags with L{cn()} before joining flags in L{cn_f()}

    @type: bool
    @see: L{cn_f()}
    @see: L{cn()}
    """

    language = None
    """\
    Language generated by this code generator

    @type: str
    """

    lang_prefix = None
    """\
    Language prefix to use in filenames to specify language specific code.

    @type: str
    """

    tmpl_flag_join = '|'
    """\
    Separator used to concatenate flags

    @type: str
    @see: L{cn_f()}
    """

    def cn(self, name):
        """\
        Return the properly formatted class name.

        @rtype: str
        @see: L{cn_f()}
        @see: L{cn_class()}
        """
        return name

    def cn_class(self, klass):
        """\
        Return the class name

        @rtype: str
        @see: L{cn()}
        """
        return klass

    def _get_style_list(self):
        """\
        Return a list of all styles supported by this widget

        @rtype: list[str]
        """
        try:
            groups = self.config['style_list']
        except (AttributeError, KeyError):
            groups = []
        return groups

    style_list = property(_get_style_list)

    def _get_box_label(self):
        """\
        Return the label of the style box in the widget dialog

        @rtype: str
        """
        try:
            name = self.config['box_label']
        except (AttributeError, KeyError):
            name = ''
        return name

    box_label = property(_get_box_label)

# end of class BaseLanguageMixin


class CppMixin(BaseLanguageMixin):
    """\
    C++ specific but generic settings and functions
    """
    comment_sign = '//'
    default_extensions = ['cpp', 'cc', 'C', 'cxx', 'c++',
                          'h', 'hh', 'hpp', 'H', 'hxx', ]
    language = 'C++'
    lang_prefix = 'cpp'

# end of class CppMixin


class LispMixin(BaseLanguageMixin):
    """\
    Lisp specific but generic settings and functions
    """
    comment_sign = ';;;'
    default_extensions = ['lisp']
    format_flags = True
    language = 'lisp'
    lang_prefix = 'lisp'

    def cn(self, name):
        if name[:2] == 'wx':
            return 'wx' + name[2:]
        elif name[:4] == 'EVT_':
            return 'wx' + name
        return name

    def cn_f(self, flags):
        flags = BaseLanguageMixin.cn_f(self, flags)
        # split again
        flags = flags.split('|')
        if len(flags) == 1:
            flags = flags[0]
        else:
            flags = '(logior %s)' % ' '.join(flags)
        return flags

# end of class LispMixin


class PerlMixin(BaseLanguageMixin):
    """\
    Perl specific but generic settings and functions

    @cvar _perl_constant_list: Incomplete list of wx constants used in wxPerl
                               Constants don't follow the Wx::ObjectName name
                               schema. There is a need to handle constants
                               separately. See also L{cn} and
                               wxPerl/trunk/Constant.xs.
    @type _perl_constant_list: list[str]
    """
    comment_sign = '#'
    default_extensions = ['pl', 'pm']
    language = 'perl'
    lang_prefix = 'perl'


    _perl_constant_list = [
        "wxALL", "wxTOP", "wxBOTTOM", "wxLEFT", "wxRIGHT", "wxDOWN",

        "wxNORTH", "wxSOUTH", "wxWEST", "wxEAST",

        "wxEXPAND", "wxGROW", "wxSHAPED", "wxFIXED_MINSIZE",

        "wxCAPTION", "wxMINIMIZE_BOX", "wxMAXIMIZE_BOX", "wxRESIZE_BORDER",

        "wxYES_NO", "wxYES", "wxNO", 'wxYES_DEFAULT', 'wxNO_DEFAULT',
        "wxCANCEL", "wxOK",

        # Colours
        "wxBLACK", "wxWHITE", "wxRED", "wxBLUE", "wxGREEN", "wxCYAN",
        "wxLIGHT_GREY",

        'wxDEFAULT', 'wxDECORATIVE', 'wxROMAN', 'wxSWISS', 'wxSCRIPT',
        'wxMODERN', 'wxTELETYPE',
        'wxNORMAL', 'wxSLANT', 'wxITALIC', 'wxNORMAL', 'wxLIGHT', 'wxBOLD',
        'wxNORMAL_FONT', 'wxSMALL_FONT', 'wxITALIC_FONT', 'wxSWISS_FONT',

        'wxHORIZONTAL', 'wxVERTICAL',
        'wxALIGN_CENTER', 'wxALIGN_CENTRE', 'wxALIGN_LEFT', 'wxALIGN_RIGHT',
        'wxALIGN_TOP', 'wxALIGN_BOTTOM', 'wxALIGN_CENTER_VERTICAL',
        'wxALIGN_CENTRE_VERTICAL', 'wxALIGN_CENTER_HORIZONTAL',
        'wxALIGN_CENTRE_HORIZONTAL',
        'wxSTANDARD_CURSOR', 'wxHOURGLASS_CURSOR', 'wxCROSS_CURSOR',

        'wxTheClipboard', 'wxFormatInvalid', 'wxThePrintPaperDatabase',
        'wxNullAnimation', 'wxNullBitmap', 'wxNullIcon', 'wxNullColour',
        'wxNullColour', 'wxNullCursor', 'wxNullFont', 'wxNullPen',
        'wxNullBrush', 'wxNullPalette', 'wxNullAcceleratorTable',

        # wxStaticLine
        'wxLI_HORIZONTAL', 'wxLI_VERTICAL',

        # wxHyperlink
        'wxHL_CONTEXTMENU', 'wxHL_ALIGN_LEFT', 'wxHL_ALIGN_RIGHT',
        'wxHL_ALIGN_CENTRE', 'wxHL_DEFAULT_STYLE',

        'wxMAJOR_VERSION', 'wxMINOR_VERSION',

        # wxSplitterWindow
        'wxSPLIT_HORIZONTAL', 'wxSPLIT_VERTICAL',

    ]

    def cn(self, name):
        """\
        Return the name properly formatted.

        @see: L{self._perl_constant_list}
        """
        # handles constants like event or language identifiers
        if name.startswith('wxBITMAP_TYPE_') or \
           name.startswith('wxBORDER_') or \
           name.startswith('wxBRUSHSTYLE_') or \
           name.startswith('wxBU_') or \
           name.startswith('wxCB_') or \
           name.startswith('wxCC_') or \
           name.startswith('wxCHB_') or \
           name.startswith('wxCHK_') or \
           name.startswith('wxCURSOR_') or \
           name.startswith('wxDD_') or \
           name.startswith('wxEVT_') or \
           name.startswith('wxFONTENCODING_') or \
           name.startswith('wxFONTFAMILY_') or \
           name.startswith('wxFONTSTYLE_') or \
           name.startswith('wxFONTWEIGHT_') or \
           name.startswith('wxFONTFLAG_') or \
           name.startswith('wxFRAME_') or \
           name.startswith('wxGA_') or \
           name.startswith('wxICON_') or \
           name.startswith('wxID_') or \
           name.startswith('wxK_') or \
           name.startswith('wxLANGUAGE_') or \
           name.startswith('wxLB_') or \
           name.startswith('wxMOD_') or \
           name.startswith('wxNB_') or \
           name.startswith('wxALIGN_') or \
           name.startswith('wxDefault') or \
           name.startswith('wxPD_') or \
           name.startswith('wxPROPSHEET_') or \
           name.startswith('wxRA_') or \
           name.startswith('wxRB_') or \
           name.startswith('wxSL_') or \
           name.startswith('wxSP_') or \
           name.startswith('wxSPLASH_') or \
           name.startswith('wxST_') or \
           name.startswith('wxSys_') or \
           name.startswith('wxSW_') or \
           name.startswith('wxSASH_') or \
           name.startswith('wxTB_') or \
           name.startswith('wxTE_') or \
           name.startswith('wxWIZARD_') or \
           name in self._perl_constant_list:
            return name

        # don't process already formatted items again
        if name.startswith('Wx::'):
            return name

        # use default for remaining names
        if name[:2] == 'wx':
            return 'Wx::' + name[2:]
        elif name[:4] == 'EVT_':
            return 'Wx::Event::' + name
        return name

# end of class PerlMixin


class PythonMixin(BaseLanguageMixin):
    """\
    Python specific but generic settings and functions
    """
    comment_sign = '#'
    default_extensions = ['py', 'pyw']
    format_flags = True
    language = 'python'
    lang_prefix = 'py'
    tmpl_flag_join = ' | '

    def cn(self, name):
        # don't process already formatted items again
        if name.startswith('wx.'):
            return name
        if name.startswith('wx'):
            return 'wx.' + name[2:]
        elif name.startswith('EVT_'):
            return 'wx.' + name
        return name

# end of class PythonMixin


class XRCMixin(BaseLanguageMixin):
    """\
    XRC specific but generic settings and functions
    """
    default_extensions = ['xrc']
    language = 'XRC'
    lang_prefix = 'xrc'

# end of class XRCMixin


class BaseWidgetWriter(StylesMixin, BaseCodeWriter):
    """\
    Base class for all widget code writer classes.

    @ivar codegen: Language specific code generator
    @type codegen: Instance of L{codegen.BaseLangCodeWriter}

    @ivar config: Widgets specific configuration (see L{common.widget_config})
    @type config: dict

    @ivar klass: wxWidgets class name
    @type klass: str | None
    """

    extra_headers = []
    """\
    List of extra header file, in the form <header.h> or "header.h" or an
    empty list.

    Example::
        extra_headers = ['<wx/treectrl.h>']

    @type: list[str]
    """

    supported_by = ()
    """\
    This widget is only available at the listed wx versions. An empty list
    means the widgets is always available.

    Example for a widgets introduced with wx 2.8::
        supported_by = ((2, 8), (3, 0))

    @type: List of tuples with major and minor wx version number
    @see: L{wxglade.codegen.BaseLangCodeWriter.for_version}
    """

    tmpl_after = []
    """\
    Template for instructions to execute after the widget is initialised

    @type: list[str]
    @see: L{tmpl_before}
    """

    tmpl_before = []
    """\
    Template for instructions to execute before the widget is initialised

    @type: list[str]
    @see: L{tmpl_after}
    """

    tmpl_layout = []
    """\
    Template to set widget layout

    @type: list[str]
    """

    tmpl_props = []
    """\
    Template to set widget properties

    @type: list[str]
    """

    tmpl = ''
    """\
    Template to create a new instance of a new wxWidget object.

    @type: str
    @see: L{get_code()}
    @see: L{tmpl_dict}
    """

    tmpl_bitmap = ''
    """\
    Template to create a C{wxBitmap(...)} call.

    @type: str
    @see: L{generate_code_bitmap()}
    @see: L{_prepare_bitmap()}
    """

    tmpl_concatenate_choices = ', '
    """\
    Template to concatenate choices

    @type: str
    @see: L{_prepare_choice()}
    """

    tmpl_dict = {}
    """\
    Content to replace in the templates

    @type: dict
    @see: L{tmpl}
    @see: L{tmpl_before}
    @see: L{tmpl_props}
    """

    tmpl_flags = '%s'
    """\
    Template to format the styles parameter.

    @type: str
    @see: L{_prepare_style()}
    """

    has_selection = False
    """\
    Flag to create a C{SetSelection(...)} call.

    @type: bool
    @see: L{tmpl_selection}
    """

    tmpl_selection = ''
    """\
    Template to create a C{SetSelection(...)} call.

    @type: str
    @see: L{has_selection}
    """

    has_setdefault = False
    """\
    Flag to create a C{SetDefault()} call.

    @type: bool
    """

    tmpl_setdefault = ''
    """\
    Template to create a C{SetDefault()} call.

    @type: str
    """

    has_setvalue = False
    """\
    Flag to create a C{SetValue(...)} call.

    @type: bool
    @see: L{tmpl_setvalue}
    @see: L{has_setvalue1}
    """

    has_setvalue1 = False
    """\
    Flag to create a C{SetValue(1)} call.

    @type: bool
    @see: L{tmpl_setvalue}
    @see: L{has_setvalue}
    """

    tmpl_setvalue = ''
    """\
    Template to create a C{SetValue(...)} call.

    @type: str
    @see: L{has_setvalue}
    @see: L{has_setvalue1}
    """

    prefix_style = False
    """\
    Prepend wxDefaultPosition and wxDefaultSize to the widget style if the
    style will be set.

    @type: bool
    @see: L{default_style}
    @see: L{set_default_style}
    """

    set_default_style = False
    """\
    Flag to to add the default style always. The default style won't added
    generally.

    @type: bool
    @see: L{default_style}
    @see: L{prefix_style}
    """

    use_names_for_binding_events = True
    """\
    Use formatted names for widget ID in event binding if widget is is
    C{-1} or C{wxID_ANY}.

    @see: L{codegen.BaseLangCodeWriter.add_object_format_name()}
    @see: L{wcodegen.BaseWidgetWriter.get_event_handlers()}
    """

    def __init__(self, klass=None):
        # call inherited constructor
        BaseCodeWriter.__init__(self)
        self.config = {}
        self.klass = klass

        # Copy non-style settings (Style settings will be handled in
        # StylesMixin fully)
        if klass and klass in common.widget_config:
            self.klass = klass
            for item in common.widget_config[self.klass]:
                if item == 'style_defs':
                    continue
                self.config[item] = \
                    copy.deepcopy(common.widget_config[self.klass][item]
                    )

        self.codegen = common.code_writers[self.language]
        self._reset_vars()

    def stmt2list(self, stmt):
        """\
        Split a code statement into a list by conserving tailing newlines

        Example::
            >>> tmpl2list('line 1\\nline 2\\nline 3\\n')
            ['line 1\\n', 'line 2\\n', 'line 3\\n', '\\n']

        @param stmt: Code statement
        @type stmt:  str

        @rtype: list[str]
        """
        temp = ['%s\n' % line for line in stmt.split('\n')]
        return temp

    def _reset_vars(self):
        """\
        Reset instance variables back to defaults
        """
        self.has_selection = False
        self.has_setdefault = False
        self.has_setvalue = False
        self.has_setvalue1 = False
        self.tmpl_before = []
        self.tmpl_after = []
        self.tmpl_layout = []
        self.tmpl_props = []
        self.tmpl_dict = {}

    def _prepare_style(self, style):
        """\
        Process and format styles.

        @param style: Styles to process / format with L{cn_f()}
        @type style:  str

        @rtype: str
        @see: L{_prepare_tmpl_content()}
        @see: L{tmpl_flags}
        """
        fmt_style = self.cn_f(style)
        fmt_default_style = self.cn_f(self.default_style)

        if fmt_style and fmt_style != fmt_default_style:
            style = self.tmpl_flags % fmt_style
        else:
            if self.set_default_style:
                if style and not fmt_style:
                    self._logger.debug(
                        _('Unsupported attribute %s use default %s instead'),
                        style, self.default_style)
                style = self.tmpl_flags % fmt_default_style
            else:
                style = ''
        if style and self.prefix_style:
            style = ', %s, %s, %s' % (
                self.cn('wxDefaultPosition'),
                self.cn('wxDefaultSize'),
                fmt_style,
            )
        return style

    def _prepare_tmpl_content(self, obj):
        """\
        Prepare and set template variables.

        @param obj: Instance of L{xml_parse.CodeObject}

        @rtype: dict
        @see: L{get_code()}
        """
        self.tmpl_dict['comment'] = self.codegen.comment_sign
        self.tmpl_dict['tab'] = self.codegen.tabs(1)
        self.tmpl_dict['store_as_attr'] = self.codegen.test_attribute(obj)
        self.tmpl_dict['id_name'], self.tmpl_dict['id_number'] = \
            self.codegen.generate_code_id(obj)
        self.tmpl_dict['id'] = self.tmpl_dict['id_number']
        self.tmpl_dict['obj_name'] = obj.name

        klass = obj.klass
        if klass == obj.base:
            klass = self.cn(klass)
        self.tmpl_dict['klass'] = klass

        self.tmpl_dict['store_as_attr'] = self.codegen.test_attribute(obj)

        prop = obj.properties

        self.tmpl_dict['style'] = self._prepare_style(prop.get('style', ''))
        self.tmpl_dict['label'] = self.codegen.quote_str(prop.get('label', ''))
        self.tmpl_dict['value'] = self.codegen.quote_str(prop.get('value', ''))
        self.tmpl_dict['value_unquoted'] = prop.get('value', '')

        return

    def _get_default_style(self):
        """\
        Default widget style in wxWidget notation.

        @rtype: str
        @see: L{set_default_style}
        @see: L{prefix_style}
        """
        try:
            name = self.config['default_style']
        except (AttributeError, KeyError):
            name = ''
        return name

    default_style = property(_get_default_style)

    def _prepare_bitmap(self, obj):
        """\
        Prepare content for widgets with bitmaps.

        The wxBitmap code will be generated automatically if the template
        in L{self.tmpl} contains '%(bitmap)s'.

        @param obj: Instance of L{xml_parse.CodeObject}
        @type obj: xml_parse.CodeObject

        @see: L{generate_code_bitmap()}
        """
        bmp_file = obj.properties.get('bitmap', '')
        self.tmpl_dict['bitmap'] = self.generate_code_bitmap(
            bmp_file, obj.preview)

        disabled_bmp = obj.properties.get('disabled_bitmap')
        if disabled_bmp:
            self.tmpl_dict['disabled_bitmap'] = self.generate_code_bitmap(
                disabled_bmp, obj.preview)
            self.tmpl_props.append(self.tmpl_bitmap_disabled)

        if not obj.properties.has_key('size') and self.tmpl_SetBestSize:
            self.tmpl_props.append(self.tmpl_SetBestSize)

        self.has_setdefault = obj.properties.get('default', False)

    def _prepare_choice(self, obj):
        """\
        Prepare content for widgets with choices.

        The content of choices will be generated automatically if the
        template in L{self.tmpl} contains '%(choices)s' or '%(choices_len)s'

        @param obj: Instance of L{xml_parse.CodeObject}

        @see: L{get_code()}
        @see: L{tmpl_concatenate_choices}
        """
        choices = obj.properties.get('choices')

        # empty choices are not allowed
        if not choices:
            choices = ['<set by wxGlade>']

        choices_str = self.tmpl_concatenate_choices.join(
            [self.codegen.quote_str(c) for c in choices])
        self.tmpl_dict['choices'] = choices_str
        self.tmpl_dict['choices_len'] = len(choices)

        selection = obj.properties.get('selection', None)
        if selection is not None and choices:
            self.tmpl_dict['selection'] = selection
            self.has_selection = True
        return

    def generate_code_bitmap(self, bitmap, preview=False):
        """\
        Returns a code fragement that generates an wxBitmap object

        @param bitmap: Bitmap definition
        @type bitmap: str

        @param preview: True to generate code for the preview
        @type preview:  bool

        @rtype: str

        @see: L{tmpl_bitmap}
        """
        assert self.tmpl_bitmap

        if not bitmap:
            return self.cn('wxNullBitmap')

        if preview and (
                    bitmap.startswith('var:') or bitmap.startswith('code:')):
            preview_icon = os.path.join(config.icons_path, "icon.xpm")
            return self.tmpl_bitmap % {
                'name': self.cn('wxBitmap'),
                'bitmap': self.codegen.quote_path(preview_icon),
                'bitmap_type': self.cn('wxBITMAP_TYPE_XPM'), }

        if bitmap.startswith('var:'):
            return self.tmpl_bitmap % {
                'name': self.cn('wxBitmap'),
                'bitmap': bitmap[4:].strip(),
                'bitmap_type': self.cn('wxBITMAP_TYPE_ANY'), }

        if bitmap.startswith('code:'):
            return '%s' % self.cn(bitmap[5:].strip())

        if preview:
            bitmap = misc.get_relative_path(bitmap, True)

        return self.tmpl_bitmap % {
            'name': self.cn('wxBitmap'),
            'bitmap': self.codegen.quote_path(bitmap),
            'bitmap_type': self.cn('wxBITMAP_TYPE_ANY'), }

    def get_code(self, obj):
        """\
        Generates language specific code for the wxWidget object from a
        template by filling variables generated by L{_prepare_tmpl_content()}.

        @see: L{_prepare_tmpl_content()}
        """
        assert self.tmpl or obj.klass == 'spacer'
        init_lines = []
        self._reset_vars()

        # spacers are generally handled by a hack:
        # The the implementations of add_sizeritem() contains more details.
        if obj.klass == 'spacer':
            width = obj.properties.get('width', '0')
            height = obj.properties.get('height', '0')
            obj.name = '%s, %s' % (width, height)
            return [], [], []

        self._prepare_tmpl_content(obj)

        # generate choices automatically if the template contains
        # '%(choices)s' or '%(choices_len)s'
        if '%(choices)s' in self.tmpl or '%(choices_len)s' in self.tmpl:
            self._prepare_choice(obj)

        # generate wxBitmap code automatically if the template contains
        # '%(bitmap)s'.
        if '%(bitmap)s' in self.tmpl:
            self._prepare_bitmap(obj)

        if self.tmpl_dict['id_name']:
            init_lines.append(self.tmpl_dict['id_name'])

        if self.tmpl_before:
            for line in self.tmpl_before:
                init_lines.append(line % self.tmpl_dict)

        init_lines.append(self.tmpl % self.tmpl_dict)

        if self.tmpl_after:
            for line in self.tmpl_after:
                init_lines.append(line % self.tmpl_dict)

        prop_lines = self.codegen.generate_common_properties(obj)

        if self.tmpl_props:
            for line in self.tmpl_props:
                prop_lines.append(line % self.tmpl_dict)

        if self.has_setvalue1:
            assert self.tmpl_setvalue
            assert not self.has_setvalue
            self.tmpl_dict['value_unquoted'] = '1'
            prop_lines.append(self.tmpl_setvalue % self.tmpl_dict)

        if self.has_setvalue and self.tmpl_dict['value_unquoted']:
            assert self.tmpl_setvalue
            assert not self.has_setvalue1
            prop_lines.append(self.tmpl_setvalue % self.tmpl_dict)

        if self.has_setdefault:
            assert self.tmpl_setdefault
            prop_lines.append(self.tmpl_setdefault % self.tmpl_dict)

        if self.has_selection:
            assert self.tmpl_selection
            prop_lines.append(self.tmpl_selection % self.tmpl_dict)

        if not self.tmpl_dict['store_as_attr']:
            # the object doesn't have to be stored as an attribute of the
            # custom class, but it is just considered part of the layout
            return [], [], init_lines + prop_lines
        return init_lines, prop_lines, []

    def get_event_handlers(self, obj):
        """\
        Returns a list of event handlers defined for the given object.

        Each list entry has following items:
         - ID,
         - Event
         - Handler
         - Event prototype

        Example::
            >>> self.get_event_handlerss(obj)
            [('wxID_OPEN', 'EVT_MENU', 'OnOpen', 'wxCommandEvent'),
             ('wxID_EXIT', 'EVT_MENU', 'OnClose', 'wxCommandEvent')]

        @param obj: Object to generate code for
        @type obj:  CodeObject

        @rtype: list[(str, str, str, str)]
        """
        ret = []
        if 'events' not in obj.properties:
            return ret

        if 'events' not in self.config:
            self._logger.warn(_('Object %(name)s(%(klass)s contains unknown '
                                'events: %(events)s)'),
                              {'name': obj.name, 'klass': obj.klass,
                               'events': obj.properties ['events']})
            return ret

        win_id = self.codegen.generate_code_id(obj)[1]
        if self.use_names_for_binding_events and \
                (win_id == '-1' or win_id == self.codegen.cn('wxID_ANY')):
            win_id = self.codegen.add_object_format_name(obj.name)

        try:
            default_event = self.config['events']['default']['type']
        except KeyError:
            default_event = 'wxCommandEvent'

        for event, handler in obj.properties['events'].iteritems():
            if event not in self.config['events']:
                self._logger.warn(
                    _('Ignore unknown event %s for %s'), (event, obj.klass)
                )
                continue

            major = 'wx%d' % self.codegen.for_version[0]
            detailed = 'wx%d%d' % self.codegen.for_version
            try:
                supported_by = self.config['events'][event]['supported_by']
                if not (major in supported_by or detailed in supported_by):
                    continue
            except (AttributeError, KeyError):
                pass

            # check for specific event type
            type_generic = 'type_%s' % major
            try:
                evt_type = self.config['events'][event][type_generic]
                ret.append((win_id, event, handler, evt_type))
                continue
            except KeyError:
                pass

            # check for generic event type
            try:
                evt_type = self.config['events'][event]['type']
            except KeyError:
                evt_type = default_event
            ret.append((win_id, event, handler, evt_type))
        return ret

    def get_properties_code(self, obj):
        """\
        Generates language specific code to set properties for the wxWidget
        object from a template by filling variables generated by
        L{_prepare_tmpl_content()}.

        @see: L{_prepare_tmpl_content()}
        @see: L{tmpl_props}
        @rtype: list[str]
        """
        prop_lines = []
        self._reset_vars()

        self._prepare_tmpl_content(obj)

        for line in self.tmpl_props:
            prop_lines.append(line % self.tmpl_dict)

        return prop_lines

    def get_layout_code(self, obj):
        """\
        Generates language specific code to create the layout for the
        wxWidget object from a template by filling variables generated by
        L{_prepare_tmpl_content()}.

        @see: L{_prepare_tmpl_content()}
        @see: L{tmpl_props}
        @rtype: list[str]
        """
        layout_lines = []
        self._reset_vars()

        self._prepare_tmpl_content(obj)

        for line in self.tmpl_layout:
            layout_lines.append(line % self.tmpl_dict)

        return layout_lines

    def is_widget_supported(self, major, minor=None):
        """\
        Check if the widget is supported for the given version

        @param major: Major version number
        @type major:  int
        @param minor: Minor version number
        @type minor:  int

        @return: True if the widget is supported by the specified wx version
        @rtype:  bool

        @see: L{common.widget_config}
        """
        assert isinstance(major, types.IntType)
        assert isinstance(minor, (types.IntType, types.NoneType))

        # no restrictions exists
        if 'supported_by' not in self.config:
            return True

        if not isinstance(minor, types.NoneType):
            version_specific = 'wx%s%s' % (major, minor)
            if version_specific in self.config['supported_by']:
                return True

        version_generic = 'wx%s' % major
        if version_generic in self.config['supported_by']:
            return True

        return False

# end of class BaseWidgetWriter


class CppWidgetCodeWriter(CppMixin, BaseWidgetWriter):
    """\
    Base class for all C++ widget code writer classes.
    """
    prefix_style = True

    tmpl_bitmap = '%(name)s(%(bitmap)s, %(bitmap_type)s)'
    tmpl_bitmap_disabled = '%(name)s->SetBitmapDisabled(%(disabled_bitmap)s);\n'
    tmpl_SetBestSize = '%(name)s->SetSize(%(name)s->GetBestSize());\n'

    tmpl_setvalue = '%(name)s->SetValue(%(value_unquoted)s);\n'
    tmpl_setdefault = '%(name)s->SetDefault();\n'
    tmpl_selection = '%(name)s->SetSelection(%(selection)s);\n'
    use_names_for_binding_events = False

    def _prepare_choice(self, obj):
        # generic part
        super(CppWidgetCodeWriter, self)._prepare_choice(obj)

        # C++ part - extend generic settings
        choices = obj.properties.get('choices')
        # empty choices are not allowed
        if not choices:
            choices = ['<set by wxGlade>']

        self.tmpl_before.append('const wxString %(name)s_choices[] = {\n')
        for choice in choices:
            self.tmpl_before.append(
                '%s%s,\n' % (
                    self.codegen.tabs(1),
                    self.codegen.quote_str(choice),
                )
            )
        self.tmpl_before.append('};\n')

        return

    def _prepare_tmpl_content(self, obj):
        BaseWidgetWriter._prepare_tmpl_content(self, obj)

        if not obj.parent.is_toplevel:
            self.tmpl_dict['parent'] = '%s' % obj.parent.name
        else:
            self.tmpl_dict['parent'] = 'this'

        if self.tmpl_dict['store_as_attr']:
            self.tmpl_dict['name'] = self.codegen._format_classattr(obj)
        else:
            self.tmpl_dict['name'] = '%s* %s' % (obj.klass, obj.name)

        return

    def get_code(self, obj):
        init, props_buf, layout = BaseWidgetWriter.get_code(self, obj)

        # default get_code() returns a tuple of three lists (init, properties
        # and layout).
        # But CPP get_code() returns a tuple of four lists (init, ids,
        # properties and layout).
        id_name = self.codegen.generate_code_id(obj)[0]
        if id_name:
            ids = [id_name]
        else:
            ids = []

        return init, ids, props_buf, layout

# end of class CppWidgetCodeWriter


class LispWidgetCodeWriter(LispMixin, BaseWidgetWriter):
    """\
    Base class for all Lisp widget code writer classes.
    """
    tmpl_bitmap = '(%(name)s_CreateLoad %(bitmap)s %(bitmap_type)s)'
    tmpl_bitmap_disabled = '(wxBitmapButton_SetBitmapDisabled ' \
                           '(slot-%(name)s obj) %(disabled_bitmap)s)\n'
    tmpl_SetBestSize = '%(name)s.wxWindow_SetSize(' \
                       '%(name)s.wxWindow_GetBestSize())\n'

    tmpl_concatenate_choices = ' '
    tmpl_setvalue = '(%(klass)s_SetValue %(name)s %(value_unquoted)s)\n'
    tmpl_setdefault = '(%(klass)s_SetDefault %(name)s)\n'
    tmpl_selection = '(%(klass)s_SetSelection %(name)s %(selection)s)\n'

    def _prepare_tmpl_content(self, obj):
        BaseWidgetWriter._prepare_tmpl_content(self, obj)

        if not obj.parent.is_toplevel:
            self.tmpl_dict['parent'] = '(slot-%s obj)' % obj.parent.name
        else:
            self.tmpl_dict['parent'] = '(slot-top-window obj)'

        if not self.tmpl_dict['style']:
            if self.default_style:
                self.tmpl_dict['style'] = self.default_style
            else:
                self.tmpl_dict['style'] = '0'

        # Lisp stores all widgets as class attributes
        self.tmpl_dict['name'] = '(%s obj)' % \
                                 self.codegen._format_classattr(obj)

        return

# end of class LispWidgetCodeWriter


class PerlWidgetCodeWriter(PerlMixin, BaseWidgetWriter):
    """\
    Base class for all Perl widget code writer classes.
    """
    prefix_style = True

    tmpl_bitmap = '%(name)s->new(%(bitmap)s, %(bitmap_type)s)'
    tmpl_bitmap_disabled = '%(name)s->SetBitmapDisabled(%(disabled_bitmap)s);\n'
    tmpl_SetBestSize = '%(name)s->SetSize(%(name)s->GetBestSize());\n'

    tmpl_setvalue = '%(name)s->SetValue(%(value_unquoted)s);\n'
    tmpl_setdefault = '%(name)s->SetDefault();\n'
    tmpl_selection = '%(name)s->SetSelection(%(selection)s);\n'

    def _prepare_tmpl_content(self, obj):
        BaseWidgetWriter._prepare_tmpl_content(self, obj)

        if not obj.parent.is_toplevel:
            parent = '$self->{%s}' % obj.parent.name
        else:
            parent = '$self'

        self.tmpl_dict['parent'] = parent
        if self.tmpl_dict['store_as_attr']:
            name = '$self->{%s}' % obj.name
        else:
            name = 'my $%s' % obj.name
        self.tmpl_dict['name'] = name

        return

# end of class PerlWidgetCodeWriter


class PythonWidgetCodeWriter(PythonMixin, BaseWidgetWriter):
    """\
    Base class for all Python widget code writer classes.
    """
    tmpl_bitmap = '%(name)s(%(bitmap)s, %(bitmap_type)s)'
    tmpl_bitmap_disabled = '%(name)s.SetBitmapDisabled(%(disabled_bitmap)s)\n'
    tmpl_SetBestSize = '%(name)s.SetSize(%(name)s.GetBestSize())\n'

    tmpl_flags = ', style=%s'
    tmpl_setvalue = '%(name)s.SetValue(%(value_unquoted)s)\n'
    tmpl_setdefault = '%(name)s.SetDefault()\n'
    tmpl_selection = '%(name)s.SetSelection(%(selection)s)\n'

    def _prepare_tmpl_content(self, obj):
        BaseWidgetWriter._prepare_tmpl_content(self, obj)

        # toplevel widgets like wxFrame or wxDialog don't have a parent
        # object
        if obj.parent and not obj.parent.is_toplevel:
            self.tmpl_dict['parent'] = 'self.%s' % obj.parent.name
        else:
            self.tmpl_dict['parent'] = 'self'

        if self.tmpl_dict['store_as_attr']:
            self.tmpl_dict['name'] = self.codegen._format_classattr(obj)
        else:
            self.tmpl_dict['name'] = obj.name

        return

# end of class PythonWidgetCodeWriter


class XrcWidgetCodeWriter(XRCMixin, BaseWidgetWriter):
    """\
    Base class for all XRC widget code writer classes.
    """
    pass

# end of class XrcWidgetCodeWriter
