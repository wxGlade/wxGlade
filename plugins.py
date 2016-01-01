"""\
Functions to import modules

@copyright: 2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import logging
import os
import sys
import zipfile

import common
import config


def load_widgets_from_dir(widget_dir, submodule=''):
    """\
    Load and initialise the all widgets listed in widgets.txt in the given
    directory.

    The names of the modules to import, are read from the file widgets.txt.

    If you need to import a submodule instead, just specify the name of the
    submodule and "<module name>.<submodule>" will be imported. This is
    useful to import language specific code generators.

    If wxGlade run in the GUI mode, the imported module returns a
    wxBitmapButton object. A list of such objects will be returned. In batch
    mode or if submodules are imported, an empty list will be returned.

    @param widget_dir: Directory to search for widgets
    @type widget_dir:  str

    @param submodule: Submodule to import
    @type submodule:  str

    @rtype: list

    @see: L{import_module()}
    @see: L{config.use_gui} - for "GUI" or "batch" mode
    """
    buttons = []

    # language code generators e.g. perl_codegen
    widgets_filename = os.path.join(widget_dir, 'widgets.txt')
    module_list = _get_modulenames_from_file(widgets_filename)

    if module_list and config.use_gui and not submodule.endswith('_codegen'):
        if submodule:
            logging.info(_('Loading "%s" modules from %s:'),
                         submodule, widgets_filename)
        else:
            logging.info(_('Loading widgets from %s:'), widgets_filename)

    for module_name in module_list:
        if submodule:
            fqmn = "%s.%s" % (module_name, submodule)
        else:
            fqmn = "%s" % module_name

        # step 1: import widget module
        module = import_module(widget_dir, fqmn)
        if not module:
            # error already logged
            continue

        # step 2: use individual initialisation if available
        if hasattr(module, 'initialize'):
            button = module.initialize()
            if config.use_gui and button:
                buttons.append(button)

        # step 3: import and initialise Python codegen as well as widget
        #         GUI elements
        elif not submodule:
            result, button = _init_codegen_gui(widget_dir, module_name)
            if not result:
                continue
            if config.use_gui and button:
                buttons.append(button)

        # step 4: do special initialisation for wconfig submodules
        elif submodule and submodule == 'wconfig':
            _process_widget_config(module)
            # don't log this action
            continue

        else:
            logging.warning(
                _('Missing function "initialize()" in imported module %s. '
                  'Skip initialisation.'), fqmn)
            continue

        if config.use_gui and not submodule.endswith('codegen'):
            logging.info('\t%s', module_name)
        else:
            logging.debug(_('Widget %s imported'), module_name)
    return buttons


def _get_modulenames_from_file(filename):
    """\
    Return a list of module names read from given file.

    @param filename: Absolute filename of the widgets.txt file
    @type filename:  str

    @rtype: list[str] | None
    """
    # test if the "widgets.txt" file exists
    if not os.path.isfile(filename):
        logging.debug(_('File %s not found.'), filename)
        return []
    try:
        widgets_file = open(filename)
        module_list = widgets_file.readlines()
        widgets_file.close()
    except (IOError, OSError), details:
        logging.warning(
            _("Can't read file %s file: %s"), filename, details)
        return []

    # remove empty lines, comment lines and tailing comments
    cleaned = []
    for module_name in module_list:
        if not module_name or module_name.startswith('#'):
            continue
        module_name = module_name.split('#', 1)[0].strip()
        module_name = module_name.strip()
        if module_name:
            cleaned.append(module_name)

    return cleaned


def _process_widget_config(module):
    """\
    Process widget configuration stored in modules 'config' dictionary. The
    processed configuration will be stored in L{config.widget_config}.

    @param module: Already imported module
    @type module: module

    @see: L{config.widget_config}

    @rtype: bool
    """
    name = getattr(module, '__name__', str(module))
    if not hasattr(module, 'config'):
        logging.debug(_('Missing configuration in module %s'), name)
        return False

    config_dict = getattr(module, 'config')

    # check mandatory attributes
    if 'wxklass' not in config_dict:
        logging.warning(
            _('Missing mandatory configuration item "wxklass" in %s. '
              'Ignoring whole configuration settings.'), name)
        return False

    try:
        # process widget related style attributes
        common.style_attrs_to_sets(config_dict['style_defs'])
        config.widget_config[config_dict['wxklass']] = \
            config_dict
    except KeyError:
        pass

    return True


def _init_codegen_gui(widget_dir, widget_name):
    """\
    Initialise Python code generator for the widget as well as widget
    GUI parts.

    @param widget_dir: Directory to search for widgets
    @type widget_dir:  str

    @param widget_name: Widget
    @type widget_name:  str

    @rtype: (bool, wx.BitmapButton)
    """
    widget_button = None

    # initialise Python code generator
    codegen_name = '%s.codegen' % widget_name
    codegen_module = import_module(widget_dir, codegen_name)
    if not codegen_module:
        # error already logged
        return False, None
    if not hasattr(codegen_module, 'initialize'):
        logging.warning(
            _('Missing function "initialize()" in imported '
              'module %s. Skip initialisation.'), codegen_name)
        return False, None
    codegen_module.initialize()

    # initialise GUI part
    if config.use_gui:
        gui_name = '%s.%s' % (widget_name, widget_name)
        gui_module = import_module(widget_dir, gui_name)
        if not gui_module:
            # error already logged
            return False, None
        if not hasattr(gui_module, 'initialize'):
            logging.warning(
                _('Missing function "initialize()" in imported '
                  'module %s. Skip initialisation.'), gui_name)
            return False, None
        widget_button = gui_module.initialize()

    return True, widget_button


def import_module(widget_dir, module_name):
    """\
    Import a single module from a ZIP file or from the directory structure.

    The consistency of ZIP files will be checked by calling L{is_valid_zip()}.

    If widget ZIP files are found, they will be process first and the default
    Python imports will be the second.

    widget_dir will be added to the Python search path temporarily if it's
    not path of sys.path already.

    Example::
        >>> import_module('./mywidgets', 'static_text')
       <module 'static_text' from 'mywidgets/static_text.zip/static_text/__init__.pyc'>

    @param widget_dir: Directory to search for widgets
    @type widget_dir:  str

    @param module_name: Name of the module to import
    @type module_name:  str

    @return: Imported module or None in case of errors
    @rtype:  Module | None

    @see: L{is_valid_zip()}
    """
    # split module name into module name and sub module name
    basemodule = module_name.split('.', 1)[0]

    if widget_dir not in sys.path:
        sys.path.append(widget_dir)

    zip_filename = os.path.join(widget_dir, '%s.zip' % basemodule)
    if os.path.exists(zip_filename):
        # check ZIP file formally
        if not is_valid_zip(zip_filename, basemodule):
            logging.warning(
                _('ZIP file %s is not a valid ZIP file. Ignoring it.'),
                zip_filename)
            zip_filename = None
        else:
            # add module temporarily to search path
            sys.path.insert(0, zip_filename)
    else:
        zip_filename = None

    # import module
    try:
        try:
            imported_module = __import__(module_name, {}, {},
                                         ['just_not_empty'])
            return imported_module
        except ImportError:
            logging.info(_('Module %s not found.'), module_name)
            return None
        except (AttributeError, NameError, SyntaxError, ValueError):
            if zip_filename:
                logging.exception(
                    _('Importing widget "%s" from ZIP file %s failed'),
                    module_name, zip_filename)
            else:
                logging.exception(
                    _('Importing widget "%s" failed'), module_name)
            return None
        except:
            logging.exception(
                _('Unexpected error during import of widget module %s'),
                module_name)
            return None

    finally:
        # remove zip file from search path
        if zip_filename and zip_filename in sys.path:
            sys.path.remove(zip_filename)


def is_valid_zip(filename, module_name):
    """\
    Check the consistency of the given ZIP files. It's a formal check as well
    as a check of the content.

    @param filename: Name of the ZIP file to check
    @type filename:  str

    @param module_name: Name of the module to import
    @type module_name:  str

    @return: True, if the ZIP is a valid widget zip file
    @rtype:  bool

    @see: L{_get_zipfile_filelist()}
    """
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
        logging.warning(
            _('Missing file %s/codegen.py[co] in ZIP file %s. Ignoring '
              'ZIP file.'), module_name, filename)
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
            _('Missing file %s/%s.py[co] in ZIP file %s. Ignoring '
              'ZIP file.'), module_name, module_name, filename)
        return False
    return True


def _get_zipfile_filelist(filename):
    """\
    Return the file list of a zip file. The list will be empty if an error
    occurred.

    @param filename: ZIP file name
    @type filename:  str

    @rtype: list
    """
    zfile = None
    namelist = []
    try:
        try:
            zfile = zipfile.ZipFile(filename)
            namelist = zfile.namelist()
            zfile.close()
        except zipfile.BadZipfile, inst:
            logging.warning(
                _('ZIP file %s is corrupt (%s). Ignoring ZIP file.'),
                filename,
                inst
            )
            return []
        except zipfile.LargeZipFile, inst:
            logging.warning(
                _('ZIP file %s is bigger than 4GB (%s). Ignoring ZIP file.'),
                filename,
                inst
            )
            return []
    finally:
        if zfile:
            zfile.close()

    return namelist
