"""\
Functions to import modules

@copyright: 2016 Carsten Grohmann
@copyright: 2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import os, re, sys, zipfile, logging
from collections import OrderedDict

import common, config, misc

# Regex tp match section headers; optionally with a hotkey character
rec_section = re.compile(r'\[(?P<section>[^]]+)\](\:(?P<hotkey>[A-Z]))?')

# Regex to match modules
rec_module = re.compile(r'^(?P<module>\w+)')


def load_widgets_from_dir(widget_dir, submodule='', default_section='not_set'):
    """Load and initialise the all widgets listed in widgets.txt in the given directory.

    If you need to import a submodule instead, just specify the name of the submodule and "<module name>.<submodule>"
    will be imported. This is useful to import language specific code generators.

    widget_dir:      Directory to search for widgets
    submodule:       Submodule to import
    default_section: Section name to group all widgets, if no section has been found

    returns: In GUI-Mode: OrderedDict with module sections as key and assigned list of wxBitmapButtons
             In batch mode: empty OrderedDict

    see: import_module(); config.use_gui - for "GUI" or "batch" mode"""
    buttons = OrderedDict()

    # language code generators e.g. perl_codegen
    widgets_filename = os.path.join(widget_dir, 'widgets.txt')
    module_info = _modulenames_from_file(widgets_filename, default_section)

    if module_info and config.use_gui and not submodule.endswith('_codegen'):
        if submodule:
            logging.info(_('Loading "%s" modules from %s:'), submodule, widgets_filename)
        else:
            logging.info(_('Loading widgets from %s:'), widgets_filename)

    for section, module_names in module_info.items():
        buttons[section] = []

        for module_name in module_names:
            if submodule:
                fqmn = "%s.%s" % (module_name, submodule)
            else:
                fqmn = "%s" % module_name
    
            # step 1: import widget module
            module = import_module(widget_dir, fqmn)
            if not module: continue  # error already logged
    
            # step 2: use individual initialisation if available
            if hasattr(module, 'initialize'):
                button = module.initialize()
                if config.use_gui and button: buttons[section].append(button)
    
            # step 3: import and initialise Python codegen as well as widget GUI elements
            elif not submodule:
                result, button = _init_codegen_gui(widget_dir, module_name)
                if not result: continue
                if config.use_gui and button: buttons[section].append(button)
    
            # step 4: do special initialisation for wconfig submodules
            elif submodule and submodule == 'wconfig':
                _process_widget_config(module)
                # don't log this action
                continue
    
            else:
                logging.warning(_('Missing function "initialize()" in imported module %s. Skip initialisation.'), fqmn)
                continue
    
            if config.use_gui and not submodule.endswith('codegen'):
                logging.info('\t%s', module_name)
    return buttons


def _modulenames_from_file(filename, default_section):
    """Return OrderedDict with module sections as key and assigned list of module names read from given file.

    @param filename: Absolute filename of the widgets.txt file
    @param default_section: Section name to group all widgets, if no section has been found"""
    content = OrderedDict()

    # test if the "widgets.txt" file exists
    if not os.path.isfile(filename):
        logging.debug(_('File %s not found.'), filename)
        return content
    try:
        widgets_file = open(filename)
        module_lines = widgets_file.readlines()
        widgets_file.close()
    except EnvironmentError as details:
        logging.warning( _("Can't read file %s file: %s"), filename, details )
        return content

    cursect = default_section

    for line in module_lines:
        # remove empty lines, comment lines and tailing comments
        line = line.rstrip()
        if not line or line.startswith('#'): continue

        if cursect not in content: content[cursect] = []

        # section heading
        mo = rec_section.match(line)
        if mo:
            cursect = mo.group('section')
            hotkey = mo.group('hotkey')
            if hotkey: misc.palette_hotkeys[hotkey] = cursect
            continue

        mo = rec_module.match(line)
        if mo:
            content[cursect].append(mo.group('module'))

    # remove empty sections
    for section in list(content.keys()):
        if not content[section]:
            del content[section]

    return content


def _process_widget_config(module):
    """Process widget configuration stored in modules 'config' dictionary.
    The processed configuration will be stored in config.widget_config.

    module: Already imported module
    
    returns bool"""
    name = getattr(module, '__name__', str(module))
    if not hasattr(module, 'config'):
        logging.debug(_('Missing configuration in module %s'), name)
        return False

    config_dict = getattr(module, 'config')

    # check mandatory attributes
    if 'wxklass' not in config_dict:
        logging.warning(
            _('Missing mandatory configuration item "wxklass" in %s. Ignoring whole configuration settings.'), name)
        return False

    try:
        # process widget related style attributes
        common.style_attrs_to_sets(config_dict['style_defs'])
        config.widget_config[config_dict['wxklass']] = config_dict
    except KeyError:
        pass

    return True


def _init_codegen_gui(widget_dir, widget_name):
    """Initialise Python code generator for the widget as well as widget GUI parts.
    returns (bool, wx.BitmapButton)"""
    widget_button = None

    # initialise Python code generator
    codegen_name = '%s.codegen' % widget_name
    codegen_module = import_module(widget_dir, codegen_name)
    if not codegen_module: return False, None  # error already logged

    if not hasattr(codegen_module, 'initialize'):
        logging.warning(_('Missing function "initialize()" in imported  module %s. Skip initialisation.'), codegen_name)
        return False, None
    codegen_module.initialize()

    gui_name = '%s.%s' % (widget_name, widget_name)
    gui_module = import_module(widget_dir, gui_name)
    if not gui_module: return False, None  # error already logged

    if not hasattr(gui_module, 'initialize'):
        logging.warning(_('Missing function "initialize()" in imported module %s. Skip initialisation.'), gui_name)
        return False, None
    widget_button = gui_module.initialize()

    return True, widget_button


def import_module(widget_dir, module_name):
    """Import a single module from a ZIP file or from the directory structure.

    The consistency of ZIP files will be checked by calling is_valid_zip().

    If widget ZIP files are found, they will be process first and the default Python imports will be the second.

    widget_dir will be added to the Python search path temporarily if it's not path of sys.path already.

    Example::
        >>> import_module('./mywidgets', 'static_text')
       <module 'static_text' from 'mywidgets/static_text.zip/static_text/__init__.pyc'>

    widget_dir:  Directory to search for widgets
    module_name: Name of the module to import

    returns Imported module or None in case of errors"""
    # split module name into module name and sub module name
    basemodule = module_name.split('.', 1)[0]

    if widget_dir not in sys.path: sys.path.append(widget_dir)

    zip_filename = os.path.join(widget_dir, '%s.zip' % basemodule)
    if os.path.exists(zip_filename):
        # check ZIP file formally
        if not is_valid_zip(zip_filename, basemodule):
            logging.warning( _('ZIP file %s is not a valid ZIP file. Ignoring it.'), zip_filename )
            zip_filename = None
        else:
            # add module temporarily to search path
            sys.path.insert(0, zip_filename)
    else:
        zip_filename = None

    # import module
    try:
        try:
            imported_module = __import__(module_name, {}, {}, ['just_not_empty'])
            return imported_module
        except ImportError:
            if config.debugging and not module_name.endswith(".wconfig") and not "lisp" in module_name and not "generic_calendar_ctrl" in module_name and not "property_grid" in module_name: raise
            return None
        except (AttributeError, NameError, SyntaxError, ValueError):
            if zip_filename:
                logging.exception( _('Importing widget "%s" from ZIP file %s failed'), module_name, zip_filename )
            else:
                logging.exception( _('Importing widget "%s" failed'), module_name )
            return None
        except:
            logging.exception( _('Unexpected error during import of widget module %s'), module_name )
            return None

    finally:
        # remove zip file from search path
        if zip_filename and zip_filename in sys.path:
            sys.path.remove(zip_filename)


def is_valid_zip(filename, module_name):
    """Check the consistency of the given ZIP files. It's a formal check as well as a check of the content.

    filename:    Name of the ZIP file to check
    module_name: Name of the module to import

    returns: True, if the ZIP is a valid widget zip file

    see: _get_zipfile_filelist()"""
    if not os.path.exists(filename):
        logging.debug(_('File %s does not exists.'), filename)
        return False

    if not zipfile.is_zipfile(filename):
        logging.warning(_('ZIP file %s is not a ZIP file.'), filename)
        return False

    #  check content of ZIP file
    content = _get_zipfile_filelist(filename)

    # check for codegen.py[co]
    found_file = False
    for ext in ['.py', '.pyc', '.pyo']:
        name = '%s/codegen%s' % (module_name, ext)
        if name in content:
            found_file = True
            break
    if not found_file:
        logging.warning(_('Missing file %s/codegen.py[co] in ZIP file %s. Ignoring ZIP file.'), module_name, filename)
        return False

    # check for GUI module
    found_file = False
    for ext in ['.py', '.pyc', '.pyo']:
        name = '%s/%s%s' % (module_name, module_name, ext)
        if name in content:
            found_file = True
            break
    if not found_file:
        logging.warning(
                _('Missing file %s/%s.py[co] in ZIP file %s. Ignoring ZIP file.'), module_name, module_name, filename)
        return False
    return True


def _get_zipfile_filelist(filename):
    "Return the file list of a zip file. The list will be empty if an error occurred"
    zfile = None
    namelist = []
    try:
        try:
            zfile = zipfile.ZipFile(filename)
            namelist = zfile.namelist()
            zfile.close()
        except zipfile.BadZipfile as inst:
            logging.warning( _('ZIP file %s is corrupt (%s). Ignoring ZIP file.'), filename, inst )
            return []
        except zipfile.LargeZipFile as inst:
            logging.warning( _('ZIP file %s is bigger than 4GB (%s). Ignoring ZIP file.'), filename, inst )
            return []
    finally:
        if zfile: zfile.close()

    return namelist
