"""\
Common code used by all widget code generators

@copyright: 2013-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

from __future__ import absolute_import

import common, config, misc, compat

import copy, logging, os.path
from .dialogs import *
from gui_mixins import StylesMixin


class BaseCodeWriter(object):
    "Base for all code writer classes"
    def __init__(self):
        "Initialise only instance variables using there defaults."
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



class BaseLanguageMixin(StylesMixin):
    "Common language specific but generic settings and functions"

    comment_sign = ''        # Character(s) to start a comment (e.g. '#' for Python and Perl or ';;;' for lisp).
    default_extensions = []  # Default extensions for generated files: a list of file extensions
    format_flags = False     # Format single flags with cn() before joining flags in cn_f()?
    language = None          # Language generated by this code generator
    lang_prefix = None       # Language prefix to use in filenames to specify language specific code.
    scope_sep = ''           # Separator between the hierarchical elements of a scope
    tmpl_flag_join = '|'     # Separator used to concatenate flags


    def cn(self, name):
        "Return the properly formatted name;  see: cn_f(), cn_class()"
        return name

    def cn_class(self, klass):
        "Return the properly formatted class name;  see cn()"
        return klass

    def get_class(self, scope):
        """Return the last element of the given scope;  see: get_scope(), scope_sep

        Example::
            >>> self.get_class('ui.AboutDialog')
            'ui.AboutDialog'
        """
        if self.scope_sep:
            scope_list = scope.rsplit(self.scope_sep, 1)
            if len(scope_list) == 2:
                return scope_list[1]
            else:
                return scope
        return scope

    def get_scope(self, scope):
        """Return the scope without the last element;  see: get_class(), scope_sep

        Example::
            >>> self.get_scope('ui.AboutDialog')
            'ui'
            >>> self.get_scope('uiAboutDialog')
            ''
        """
        if self.scope_sep:
            scope_list = scope.rsplit(self.scope_sep, 1)
            if len(scope_list) == 2:
                return scope_list[0]
            else:
                return ''
        return ''

    def _get_style_list(self):
        "Return a list of all styles supported by this widget"
        try:
            groups = self.config['style_list']
        except (AttributeError, KeyError):
            groups = []
        return groups

    style_list = property(_get_style_list)



class CppMixin(BaseLanguageMixin):
    "C++ specific but generic settings and functions"
    comment_sign = '//'
    default_extensions = ['cpp', 'cc', 'C', 'cxx', 'c++',  'h', 'hh', 'hpp', 'H', 'hxx', ]
    language = 'C++'
    lang_prefix = 'cpp'
    scope_sep = '::'

    def cn_class(self, klass):
        if not klass:
            return klass
        klass = klass.replace('::', '_')
        return klass



class LispMixin(BaseLanguageMixin):
    "Lisp specific but generic settings and functions"
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



class PerlMixin(BaseLanguageMixin):
    """Perl specific but generic settings and functions

    @cvar _perl_constant_list: Incomplete list of wx constants used in wxPerl
                               Constants don't follow the Wx::ObjectName name schema.
                               There is a need to handle constants separately.
                               See also L{cn} and wxPerl/trunk/Constant.xs.
    @type _perl_constant_list: list[str]"""
    comment_sign = '#'
    default_extensions = ['pl', 'pm']
    language = 'perl'
    lang_prefix = 'perl'
    scope_sep = '::'

    _perl_constant_list = [
        "wxALL", "wxTOP", "wxBOTTOM", "wxLEFT", "wxRIGHT", "wxDOWN",

        "wxNORTH", "wxSOUTH", "wxWEST", "wxEAST",

        "wxEXPAND", "wxGROW", "wxSHAPED", "wxFIXED_MINSIZE",

        "wxCAPTION", "wxMINIMIZE_BOX", "wxMAXIMIZE_BOX", "wxRESIZE_BORDER",

        "wxYES_NO", "wxYES", "wxNO", 'wxYES_DEFAULT', 'wxNO_DEFAULT', "wxCANCEL", "wxOK",

        # Colours
        "wxBLACK", "wxWHITE", "wxRED", "wxBLUE", "wxGREEN", "wxCYAN", "wxLIGHT_GREY",
        # Fonts
        'wxDEFAULT', 'wxDECORATIVE', 'wxROMAN', 'wxSWISS', 'wxSCRIPT', 'wxMODERN', 'wxTELETYPE',
        'wxNORMAL', 'wxSLANT', 'wxITALIC', 'wxNORMAL', 'wxLIGHT', 'wxBOLD',
        'wxNORMAL_FONT', 'wxSMALL_FONT', 'wxITALIC_FONT', 'wxSWISS_FONT',

        'wxHORIZONTAL', 'wxVERTICAL',
        'wxALIGN_CENTER', 'wxALIGN_CENTRE', 'wxALIGN_LEFT', 'wxALIGN_RIGHT',
        'wxALIGN_TOP', 'wxALIGN_BOTTOM', 'wxALIGN_CENTER_VERTICAL',
        'wxALIGN_CENTRE_VERTICAL', 'wxALIGN_CENTER_HORIZONTAL', 'wxALIGN_CENTRE_HORIZONTAL',
        'wxSTANDARD_CURSOR', 'wxHOURGLASS_CURSOR', 'wxCROSS_CURSOR',

        'wxTheClipboard', 'wxFormatInvalid', 'wxThePrintPaperDatabase',
        'wxNullAnimation', 'wxNullBitmap', 'wxNullIcon', 'wxNullColour',
        'wxNullColour', 'wxNullCursor', 'wxNullFont', 'wxNullPen',
        'wxNullBrush', 'wxNullPalette', 'wxNullAcceleratorTable',

        # wxStaticLine
        'wxLI_HORIZONTAL', 'wxLI_VERTICAL',

        # wxHyperlink
        'wxHL_CONTEXTMENU', 'wxHL_ALIGN_LEFT', 'wxHL_ALIGN_RIGHT', 'wxHL_ALIGN_CENTRE', 'wxHL_DEFAULT_STYLE',

        'wxMAJOR_VERSION', 'wxMINOR_VERSION',

        # wxSplitterWindow
        'wxSPLIT_HORIZONTAL', 'wxSPLIT_VERTICAL',
    ]
    def cn(self, name):
        "Return the name properly formatted; see: self._perl_constant_list"
        # handles constants like event or language identifiers
        if name.startswith('wxBITMAP_TYPE_') or name.startswith("wxDefault") or name.startswith('wxSYS_COLOUR_'):
            return name
        if "_" in name:
            # check whether name starts with any of these plus underscore:
            start = name.split("_",1)[0]
            if start in {'wxART', 'wxBORDER', 'wxBRUSHSTYLE', 'wxBU', 'wxCB', 'wxCC', 'wxCHB', 'wxCHK',
                        'wxCURSOR', 'wxDD', 'wxEVT', 'wxFONTENCODING', 'wxFONTFAMILY', 'wxFONTSTYLE',
                        'wxFONTWEIGHT', 'wxFONTFLAG', 'wxFRAME', 'wxGA', 'wxICON', 'wxID', 'wxK', 'wxLANGUAGE',
                        'wxLB', 'wxMOD', 'wxNB', 'wxALIGN', 'wxDefault', 'wxPD', 'wxPROPSHEET', 'wxRA', 'wxRB',
                        'wxSL', 'wxSP', 'wxSPLASH', 'wxST', 'wxSys', 'wxSW', 'wxSASH',
                        'wxTB', 'wxTE', 'wxWIZARD'}:
                return name
        if name in self._perl_constant_list: return name

        # don't process already formatted items again
        if name.startswith('Wx::'): return name

        # use default for remaining names
        if name[:2] == 'wx':   return 'Wx::' + name[2:]
        if name[:4] == 'EVT_': return 'Wx::Event::' + name

        return name


class PythonMixin(BaseLanguageMixin):
    "Python specific but generic settings and functions"
    comment_sign = '#'
    default_extensions = ['py', 'pyw']
    format_flags = True
    language = 'python'
    lang_prefix = 'py'
    scope_sep = '.'
    tmpl_flag_join = ' | '

    def cn(self, name):
        # don't process already formatted items again
        if name.startswith('wx.'):  return name
        if name.startswith('wx'):   return 'wx.' + name[2:]
        if name.startswith('EVT_'): return 'wx.' + name
        return name

    def cn_class(self, klass):
        if not klass:
            return klass
        if not klass.startswith('wx.'):
            klass = self.get_class(klass)
        klass = klass.replace('::', '_')
        return klass



class XRCMixin(BaseLanguageMixin):
    "XRC specific but generic settings and functions"
    default_extensions = ['xrc']
    language = 'XRC'
    lang_prefix = 'xrc'



class BaseWidgetWriter(StylesMixin, BaseCodeWriter):
    """Base class for all widget code writer classes.

    codegen: Language specific code generator, instance of codegen.BaseLangCodeWriter
    config: Widgets specific configuration dict (see config.widget_config)
    klass: wxWidgets class name or None"""

    # List of extra modules to import; this list can be changed on demand.
    # It'll be reset to the initial value stored in L{__import_modules} within L{_reset_vars()}.
    # example: import_modules = ['use Wx::Grid;\\n']
    import_modules = [] 
    # Copy of the initial state of import_modules. This copy is used to restore the initial state within _reset_vars().
    __import_modules = []

    # This widget is only available at the listed wx versions. An empty list means the widgets is always available.
    # List of tuples with major and minor wx version number; see wxglade.codegen.BaseLangCodeWriter.for_version
    # Example for a widgets introduced with wx 2.8:: supported_by = ((2, 8), (3, 0))
    supported_by = ()

    ####################################################################################################################
    # template strings and lists of template strings:
    tmpl_after  = []  # for instructions to execute after the widget is initialised
    tmpl_before = []  # for instructions to execute before the widget is initialised
    tmpl_layout = []  # to set widget layout
    tmpl_props  = []  # to set widget properties
    tmpl = ''         # to create a new instance of a new wxWidget object; see get_code(), tmp_dict
    tmpl_concatenate_choices = ', ' # to concatenate choices; see _prepare_choice()
    tmpl_dict  = {}   # dict of content to replace in the templates; see tmpl, tmpl_before, tmpl_props
    tmpl_flags = '%s' # to format the styles parameter; see _prepare_style()

    # see: generate_code_bitmap(), _prepare_bitmap()
    tmpl_inline_artprovider = '' # to inline a bitmap from wxArtProvider; doesn't end with a newline
    tmpl_inline_bitmap      = '' # to inline a C{wxBitmap(...)} call; doesn't end with a newline
    tmpl_inline_emptybitmap = '' # to create an empty wxBitmap; doesn't end with a newline

    tmpl_import_artprovider = '' # to import / include the art provider; see _prepare_bitmap()

    tmpl_inline_wxSize = '' # to inline a widget size with wxSize(); doesn't end with a newline; get_inline_stmt_wxSize()

    has_selection  = False  # Flag to create a SetSelection(...) call; see tmpl_selection
    tmpl_selection = ''    # Template to create a SetSelection(...) call; see has_selection

    has_setdefault  = False # Flag to create a C{SetDefault()} call.
    tmpl_setdefault = ''    # Template to create a C{SetDefault()} call.

    has_setvalue  = False  # Flag to create a C{SetValue(...)} call;     see tmpl_setvalue, has_setvalue1
    has_setvalue1 = False  # Flag to create a C{SetValue(1)} call;       see tmpl_setvalue, has_setvalue
    tmpl_setvalue = ''     # Template to create a C{SetValue(...)} call; see has_setvalue, has_setvalue1

    # see default_style:
    prefix_style      = False # Prepend wxDefaultPosition and wxDefaultSize to the widget style if the style will be set
    set_default_style = False # Flag to add the default style always. The default style won't added generally.

    # Use formatted names for widget ID in event binding if widget is is -1 or wxID_ANY.
    # see codegen.BaseLangCodeWriter.add_object_format_name(), wcodegen.BaseWidgetWriter.get_event_handlers()
    use_names_for_binding_events = True

    def __init__(self, klass=None):
        # call inherited constructor
        BaseCodeWriter.__init__(self)
        self.config = {}
        self.klass = klass

        # store initial content
        if hasattr(self, 'import_modules'):
            self.__import_modules = self.import_modules[:]
        else:
            self.__import_modules = []

        # Copy non-style settings (Style settings will be handled in StylesMixin fully)
        if klass and klass in config.widget_config:
            self.klass = klass
            for item in config.widget_config[self.klass]:
                if item == 'style_defs':
                    continue
                self.config[item] = copy.deepcopy(config.widget_config[self.klass][item])

        self.codegen = common.code_writers[self.language]
        self._reset_vars()

    def format_widget_access(self, obj):
        return self.codegen.format_generic_access(obj)

    def stmt2list(self, stmt):
        """Split a code statement into a list by conserving tailing newlines
        e.g. tmpl2list('line 1\\nline 2\\nline 3\\n') -> ['line 1\\n', 'line 2\\n', 'line 3\\n', '\\n']"""
        temp = ['%s\n' % line for line in stmt.split('\n')]
        return temp

    def _reset_vars(self):
        "Reset instance variables back to defaults"
        self.import_modules = self.__import_modules[:]
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
        "Process and format style string with cn_f(); returns string; see _prepare_tmpl_content(), tmpl_flags"
        fmt_style = self.cn_f(style.get_string_value())
        fmt_default_style = self.cn_f(self.default_style)

        if fmt_style and fmt_style != fmt_default_style:
            style = self.tmpl_flags % fmt_style
        else:
            if self.set_default_style:
                if style and not fmt_style:
                    self._logger.debug( _('Unsupported attribute %s use default %s instead'), style, self.default_style)
                style = self.tmpl_flags % fmt_default_style
            else:
                style = ''
        if style and self.prefix_style:
            style = ', %s, %s, %s' % ( self.cn('wxDefaultPosition'), self.cn('wxDefaultSize'), fmt_style )
        return style

    def _prepare_tmpl_content(self, obj):
        "Prepare and set template variables; obj is instance of L{xml_parse.CodeObject}; returns dict"
        self.tmpl_dict['comment'] = self.codegen.comment_sign
        self.tmpl_dict['tab'] = self.codegen.tabs(1)
        self.tmpl_dict['store_as_attr'] = self.codegen.store_as_attr(obj)
        self.tmpl_dict['id_name'], self.tmpl_dict['id_number'] = self.codegen.generate_code_id(obj)
        self.tmpl_dict['id'] = self.tmpl_dict['id_number']
        self.tmpl_dict['obj_name'] = obj.name

        klass = obj.klass
        if klass == obj.base:
            klass = self.cn(klass)
        else:
            klass = self.cn_class(klass)
        self.tmpl_dict['klass'] = klass

        self.tmpl_dict['store_as_attr'] = self.codegen.store_as_attr(obj)

        if obj.check_prop('style'): self.tmpl_dict['style'] = self._prepare_style(obj.properties["style"])
        if obj.check_prop('label'):
            self.tmpl_dict['label'] = self.codegen.quote_str( obj.label )
        if obj.check_prop('value'): self.tmpl_dict['value'] = self.codegen.quote_str( str(obj.value) )
        if obj.check_prop('value_unquoted'): self.tmpl_dict['value_unquoted'] = obj.value

        return

    def _get_default_style(self):
        "Default widget style in wxWidget notation; see set_default_style, prefix_style"
        try:
            name = self.config['default_style']
        except (AttributeError, KeyError):
            name = ''
        return name

    default_style = property(_get_default_style)

    def _prepare_bitmap(self, obj, first='bitmap', second='disabled_bitmap'):
        "Prepare content for widgets with bitmaps; obj is xml_parse.CodeObject; see generate_code_bitmap(), get_code()"
        bmp_first = obj.properties[first].get_value()
        self.tmpl_dict[first] = self.generate_code_bitmap(bmp_first, self.codegen.preview)

        if second in obj.properties:
            bmp_second = obj.properties[second].get_value()
            if bmp_second:
                self.tmpl_dict[second] = self.generate_code_bitmap(bmp_second, self.codegen.preview)
                self.tmpl_props.append(self.tmpl_bitmap_disabled)
        else:
            bmp_second = ""

        if self.tmpl_import_artprovider and (bmp_first.startswith('art:') or bmp_second.startswith('art:')):
            self.import_modules.append(self.tmpl_import_artprovider)

        if not obj.check_prop('size') and self.tmpl_SetBestSize:
            self.tmpl_props.append(self.tmpl_SetBestSize)

        self.has_setdefault = "default" in obj.properties and obj.default or False

    def _prepare_choice(self, obj):
        """Prepare content for widgets with choices; see: get_code(), tmpl_concatenate_choices

        The content of choices will be generated automatically if the
        template in self.tmpl contains '%(choices)s' or '%(choices_len)s'

        obj: Instance of xml_parse.CodeObject"""
        choices = [c[0] for c in obj.choices]

        choices_str = self.tmpl_concatenate_choices.join( [self.codegen.quote_str(c) for c in choices] )
        self.tmpl_dict['choices'] = choices_str
        self.tmpl_dict['choices_len'] = len(choices)

        if choices:
            selection_p = obj.properties.get("selection", None)
            if selection_p and selection_p.is_active():
                self.tmpl_dict['selection'] = selection_p.get()
                self.has_selection = True

    def generate_code_bitmap(self, bitmap, preview=False):
        """Returns a code fragment that generates an wxBitmap object

        bitmap: Bitmap definition string
        preview: True to generate code for the preview

        see: tmpl_inline_bitmap, get_inline_stmt_emptybitmap(), get_inline_stmt_artprovider()"""
        assert self.tmpl_inline_bitmap

        if not bitmap:
            return self.codegen.cn('wxNullBitmap')

        if preview and ( bitmap.startswith('var:') or bitmap.startswith('code:') ):
            preview_icon = os.path.join(config.icons_path, "icon.xpm")
            return self.tmpl_inline_bitmap % { 'name': self.codegen.cn('wxBitmap'),
                                               'bitmap': self.codegen.quote_path(preview_icon),
                                               'bitmap_type': self.codegen.cn('wxBITMAP_TYPE_XPM') }

        if bitmap.startswith('var:'):
            return self.tmpl_inline_bitmap % { 'name': self.codegen.cn('wxBitmap'),
                                               'bitmap': bitmap[4:].strip(),
                                               'bitmap_type': self.codegen.cn('wxBITMAP_TYPE_ANY') }

        if bitmap.startswith('empty:'): return self.get_inline_stmt_emptybitmap(bitmap)
        if bitmap.startswith('art:'):   return self.get_inline_stmt_artprovider(bitmap) 
        if bitmap.startswith('code:'):  return '%s' % self.codegen.cn(bitmap[5:].strip())

        if preview:
            bitmap = misc.get_relative_path(bitmap, True)

        return self.tmpl_inline_bitmap % { 'name': self.codegen.cn('wxBitmap'),
                                           'bitmap': self.codegen.quote_path(bitmap),
                                           'bitmap_type': self.codegen.cn('wxBITMAP_TYPE_ANY') }

    def get_code(self, obj):
        """Generates language specific code for the wxWidget object from a template by filling variables
        generated by L{_prepare_tmpl_content()}."""
        assert self.tmpl or obj.klass in ('spacer','sizerslot')#,'sizeritem')
        init_lines = []
        self._reset_vars()

        self._prepare_tmpl_content(obj)

        # generate choices automatically if the template contains '%(choices)s' or '%(choices_len)s'
        if '%(choices)s' in self.tmpl or '%(choices_len)s' in self.tmpl:
            self._prepare_choice(obj)

        # generate wxBitmap code automatically if the template contains '%(bitmap)s'.
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
        """Returns a list of event handlers defined for the given object (CodeObject instance).

        Each list entry has following items: (ID, Event, Handler, Event prototype)

        B{Example}::
            >>> self.get_event_handlers(obj)
            [('wxID_OPEN', 'EVT_MENU', 'OnOpen', 'wxCommandEvent'),
             ('wxID_EXIT', 'EVT_MENU', 'OnClose', 'wxCommandEvent')]"""

        ret = []
        if 'events' not in obj.properties:
            return ret
        events = [(event,handler) for (event,handler) in obj.events if handler]
        if not events: return ret

        if 'events' not in self.config:
            self._logger.warn( _('Object %(name)s(%(klass)s contains unknown events: %(events)s)'),
                               {'name':obj.name,'klass': obj.klass, 'events':obj.properties ['events']})
            return ret

        win_id = self.codegen.generate_code_id(obj)[1]
        if self.use_names_for_binding_events and (win_id == '-1' or win_id == self.codegen.cn('wxID_ANY')):
            win_id = self.codegen.add_object_format_name(obj.name)

        try:
            default_event = self.config['events']['default']['type']
        except KeyError:
            default_event = 'wxCommandEvent'

        for event, handler in sorted( events ):
            if event not in self.config['events']:
                self._logger.warn( _('Ignore unknown event %s for %s'), (event, obj.klass) )
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
        """Generates language specific code to set properties for the wxWidget object from a template
        by filling variables generated by _prepare_tmpl_content(); returns list of strings; see tmpl_props"""
        prop_lines = []
        self._reset_vars()

        self._prepare_tmpl_content(obj)
        for line in self.tmpl_props:
            prop_lines.append(line % self.tmpl_dict)
        return prop_lines

    def get_layout_code(self, obj):
        """Generates language specific code to create the layout for the wxWidget object from a template
        by filling variables generated by _prepare_tmpl_content(); returns list of strings; see tmpl_props"""
        layout_lines = []
        self._reset_vars()

        self._prepare_tmpl_content(obj)
        for line in self.tmpl_layout:
            layout_lines.append(line % self.tmpl_dict)
        return layout_lines

    def get_inline_stmt_artprovider(self, bitmap):
        """Return a inline statement of a bitmap from the given statement using wxArtProvider.
        See generate_code_bitmap().

        bitmap: Bitmap definition (string)

        B{Syntax}::
            art:<ArtID>,<ArtClient>
            art:<ArtID>,<ArtClient>,<width>,<height>

        B{Example}::
            >>> get_inline_stmt_artprovider('art:wxART_HELP,wxART_OTHER,32,32')
            'wx.ArtProvider.GetBitmap(wx.ART_HELP, wx.ART_OTHER, (32, 32))'"""
        # keep in sync with BitmapMixin.get_preview_obj_bitmap()
        art_id = 'wxART_ERROR'
        art_client = 'wxART_OTHER'
        size = 'wxDefaultSize'

        try:
            content = bitmap[4:]
            elements = [item.strip() for item in content.split(',')]
            if len(elements) == 2:
                art_id, art_client = elements
            elif len(elements) == 4:
                art_id, art_client, width, height = elements
                size = self.get_inline_stmt_wxSize(width, height)
            else:
                raise ValueError

        except ValueError:
            self._logger.warn('Malformed statement to create a bitmap via wxArtProvider(): %s', bitmap)

        stmt = self.tmpl_inline_artprovider % {'art_id': self.codegen.cn(art_id),
                                               'art_client': self.codegen.cn(art_client),
                                               'size': self.codegen.cn(size) }
        return stmt

    def get_inline_stmt_emptybitmap(self, bitmap):
        """Return a inline statement to create an empty wxBitmap. See generate_code_bitmap().

        bitmap: Bitmap definition (string)

        B{Syntax}::
            empty:<width>,<height>

        B{Example}::
            >>> get_inline_stmt_emptybitmap('empty:32,32')
            'wx.EmptyBitmap(32, 32)'"""
        # keep in sync with BitmapMixin.get_preview_obj_bitmap()
        width = 16
        height = 16
        try:
            size = bitmap[6:]
            width, height = [int(item.strip()) for item in size.split(',', 1)]
        except ValueError:
            self._logger.warn( 'Malformed statement to create an empty bitmap: %s', bitmap )
        stmt = self.tmpl_inline_emptybitmap % { 'width': width, 'height': height }
        return stmt

    def get_inline_stmt_wxSize(self, width, heigh):
        """Returns a inline statement to specific the widget size with wxSize()

        B{Example}::
            >>> get_inline_stmt_wxSize(16, 16)
            '(16, 16)'                  # Python

            >>> get_inline_stmt_wxSize(16, 16)
            'wxSize(16, 16)'            # C++"""
        stmt = self.tmpl_inline_wxSize % {'width': width, 'height': heigh }
        return stmt

    def is_widget_supported(self, major, minor=None):
        """Check if the widget is supported for the given version; see config.widget_config
        major, minor: Major and minor version number (int)"""
        assert isinstance(major, int)
        assert isinstance(minor, int) or minor is None

        # no restrictions exists
        if 'supported_by' not in self.config:
            return True

        if minor is not None:
            version_specific = 'wx%s%s' % (major, minor)
            if version_specific in self.config['supported_by']:
                return True

        version_generic = 'wx%s' % major
        if version_generic in self.config['supported_by']:
            return True

        return False



class CppWidgetCodeWriter(CppMixin, BaseWidgetWriter):
    """Base class for all C++ widget code writer classes.

    @cvar constructor: List of tuples to describe the constructor parameter set, ech tuple contains type, name and
                       optional the default value,
                       The constructor parameters will be used for toplevel windows only.
    @type constructor:  list[(str, str, str)] | list[(str, str)]"""
    prefix_style = True

    tmpl_import_artprovider = '<wx/artprov.h>'
    tmpl_inline_artprovider = 'wxArtProvider::GetBitmap(%(art_id)s, %(art_client)s, %(size)s)'
    tmpl_inline_bitmap      = '%(name)s(%(bitmap)s, %(bitmap_type)s)'
    tmpl_inline_emptybitmap = 'wxBitmap(%(width)s, %(height)s)'
    tmpl_bitmap_disabled    = '%(name)s->SetBitmapDisabled(%(disabled_bitmap)s);\n'

    tmpl_selection   = '%(name)s->SetSelection(%(selection)s);\n'
    tmpl_setvalue    = '%(name)s->SetValue(%(value_unquoted)s);\n'
    tmpl_SetBestSize = '%(name)s->SetSize(%(name)s->GetBestSize());\n'
    tmpl_setdefault  = '%(name)s->SetDefault();\n'
    tmpl_inline_wxSize = 'wxSize(%(width)s, %(height)s)'

    use_names_for_binding_events = False

    def _prepare_choice(self, obj):
        # generic part
        super(CppWidgetCodeWriter, self)._prepare_choice(obj)

        # C++ part - extend generic settings
        choices = obj.choices
        # empty choices are not allowed
        if choices:
            self.tmpl_before.append('const wxString %(name)s_choices[] = {\n')
            for choice in choices:
                self.tmpl_before.append( '%s%s,\n' % (self.codegen.tabs(1), self.codegen.quote_str(choice[0])) )
            self.tmpl_before.append('};\n')
        else:
            self.tmpl_before.append('const wxString %(name)s_choices[] = {};\n')

        return

    def _prepare_tmpl_content(self, obj):
        BaseWidgetWriter._prepare_tmpl_content(self, obj)

        # Toplevel widgets like wxFrame or wxDialog don't have a parent object.
        # The parent object is optional for MenuBar and ToolBar widgets.
        if not obj.parent:
            # this breaks the generated code
            self.tmpl_dict['parent'] = 'Do not use the "parent" substitution in code templates for toplevel windows'
        elif not obj.parent.is_toplevel:
            self.tmpl_dict['parent'] = '%s' % obj.parent.name
        else:
            self.tmpl_dict['parent'] = 'this'

        if self.tmpl_dict['store_as_attr']:
            self.tmpl_dict['name'] = self.codegen._format_classattr(obj)
        else:
            self.tmpl_dict['name'] = '%s* %s' % (obj.klass, obj.name)

        if 'id_name' in self.tmpl_dict:
            # An enum with the IDs has been generated in codegen.cpp_codegen.CPPCodeWriter.add_class() already
            self.tmpl_dict['id_name'] = []

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

    def format_widget_access(self, obj):
        if obj.is_toplevel:
            return 'this'
        else:
            return '%s' % obj.name



class LispWidgetCodeWriter(LispMixin, BaseWidgetWriter):
    "Base class for all Lisp widget code writer classes"
    tmpl_inline_artprovider = 'wxArtProvider_GetBitmap(%(art_id)s %(art_client)s %(size)s)'
    tmpl_inline_bitmap      = '(%(name)s_CreateLoad %(bitmap)s %(bitmap_type)s)'
    tmpl_inline_emptybitmap = 'wxBitmap_Create(%(width)s %(height)s)'
    tmpl_bitmap_disabled    = '(wxBitmapButton_SetBitmapDisabled (slot-%(name)s obj) %(disabled_bitmap)s)\n'

    tmpl_concatenate_choices = ' '
    tmpl_selection   = '(%(klass)s_SetSelection %(name)s %(selection)s)\n'
    tmpl_setvalue    = '(%(klass)s_SetValue %(name)s %(value_unquoted)s)\n'
    tmpl_SetBestSize = '%(name)s.wxWindow_SetSize(%(name)s.wxWindow_GetBestSize())\n'
    tmpl_setdefault  = '(%(klass)s_SetDefault %(name)s)\n'
    tmpl_inline_wxSize = 'wxSize_Create(%(width)s %(height)s)'

    def _prepare_tmpl_content(self, obj):
        BaseWidgetWriter._prepare_tmpl_content(self, obj)

        # Toplevel widgets like wxFrame or wxDialog don't have a parent object.
        # The parent object is optional for MenuBar and ToolBar widgets.
        if not obj.parent:
            # this breaks the generated code
            self.tmpl_dict['parent'] = 'Do not use the "parent" substitution in code templates for toplevel windows'
        elif not obj.parent.is_toplevel:
            self.tmpl_dict['parent'] = '(slot-%s obj)' % obj.parent.name
        else:
            self.tmpl_dict['parent'] = '(slot-top-window obj)'

        if 'style' in obj.properties and not self.tmpl_dict['style']:
            if self.default_style:
                self.tmpl_dict['style'] = self.default_style
            else:
                self.tmpl_dict['style'] = '0'

        # Lisp stores all widgets as class attributes
        self.tmpl_dict['name'] = '(%s obj)' % self.codegen._format_classattr(obj)

        return



class PerlWidgetCodeWriter(PerlMixin, BaseWidgetWriter):
    "Base class for all Perl widget code writer classes"
    prefix_style = True

    tmpl_import_artprovider = 'use Wx::ArtProvider qw/:artid :clientid/;\n'
    tmpl_inline_artprovider = 'Wx::ArtProvider::GetBitmap(%(art_id)s, %(art_client)s, %(size)s)'
    tmpl_inline_bitmap      = '%(name)s->new(%(bitmap)s, %(bitmap_type)s)'
    tmpl_inline_emptybitmap = 'Wx::Bitmap->new(%(width)s, %(height)s)'
    tmpl_bitmap_disabled    = '%(name)s->SetBitmapDisabled(%(disabled_bitmap)s);\n'

    tmpl_selection   = '%(name)s->SetSelection(%(selection)s);\n'
    tmpl_setvalue    = '%(name)s->SetValue(%(value_unquoted)s);\n'
    tmpl_SetBestSize = '%(name)s->SetSize(%(name)s->GetBestSize());\n'
    tmpl_setdefault  = '%(name)s->SetDefault();\n'
    tmpl_inline_wxSize = 'Wx::Size->new(%(width)s, %(height)s)'


    def _prepare_tmpl_content(self, obj):
        BaseWidgetWriter._prepare_tmpl_content(self, obj)

        # Toplevel widgets like wxFrame or wxDialog don't have a parent object.
        # The parent object is optional for MenuBar and ToolBar widgets.
        if not obj.parent:
            # this breaks the generated code
            self.tmpl_dict['parent'] = 'Do not use the "parent" substitution in code templates for toplevel windows'
        elif not obj.parent.is_toplevel:
            self.tmpl_dict['parent'] = '$self->{%s}' % obj.parent.name
        else:
            self.tmpl_dict['parent'] = '$self'

        if self.tmpl_dict['store_as_attr']:
            name = '$self->{%s}' % obj.name
        else:
            name = 'my $%s' % obj.name
        self.tmpl_dict['name'] = name

        return



class PythonWidgetCodeWriter(PythonMixin, BaseWidgetWriter):
    "Base class for all Python widget code writer classes"
    tmpl_inline_artprovider = 'wx.ArtProvider.GetBitmap(%(art_id)s, %(art_client)s, %(size)s)'
    tmpl_inline_bitmap = '%(name)s(%(bitmap)s, %(bitmap_type)s)'
    if compat.IS_CLASSIC:
        tmpl_inline_emptybitmap = 'wx.EmptyBitmap(%(width)s, %(height)s)'
    else:
        tmpl_inline_emptybitmap = 'wx.Bitmap(%(width)s, %(height)s)'
    tmpl_bitmap_disabled = '%(name)s.SetBitmapDisabled(%(disabled_bitmap)s)\n'

    tmpl_flags       = ', style=%s'
    tmpl_selection   = '%(name)s.SetSelection(%(selection)s)\n'
    tmpl_setvalue    = '%(name)s.SetValue(%(value_unquoted)s)\n'
    tmpl_SetBestSize = '%(name)s.SetSize(%(name)s.GetBestSize())\n'
    tmpl_setdefault  = '%(name)s.SetDefault()\n'
    tmpl_inline_wxSize = '(%(width)s, %(height)s)'

    def _prepare_tmpl_content(self, obj):
        BaseWidgetWriter._prepare_tmpl_content(self, obj)

        # Toplevel widgets like wxFrame or wxDialog don't have a parent object.
        # The parent object is optional for MenuBar and ToolBar widgets.
        if not obj.parent:
            # this breaks the generated code
            self.tmpl_dict['parent'] = 'Do not use the "parent" substitution in code templates for toplevel windows'
        elif not obj.parent.is_toplevel:
            self.tmpl_dict['parent'] = 'self.%s' % obj.parent.name
        else:
            self.tmpl_dict['parent'] = 'self'

        if self.tmpl_dict['store_as_attr']:
            self.tmpl_dict['name'] = self.codegen._format_classattr(obj)
        else:
            self.tmpl_dict['name'] = obj.name

        return



class XrcWidgetCodeWriter(XRCMixin, BaseWidgetWriter):
    "Base class for all XRC widget code writer classes"
    pass
