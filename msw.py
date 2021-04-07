"""\
Microsoft Windows specific helpers

@copyright: 2016-2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import sys, os
import win32api, win32con, pywintypes


class RegistryCurrentUser:
    branch = win32con.HKEY_CURRENT_USER
    def __init__(self, keyname, handle=None, mode="w"): #access=win32con.KEY_ALL_ACCESS):
        """Easy access to win32 registry.
        mode: 'r' or 'w'"""
        if mode=="w":
            access = win32con.KEY_ALL_ACCESS
        elif mode=="r":
            access = win32con.KEY_READ
        else:
            raise ValueError( "mode must be 'r' or 'w'" )
        self.mode = mode
        self.handle = None
        if handle is None:
            handle=self.branch
        # open key
        try:
            self.handle = win32api.RegOpenKeyEx(handle, keyname, 0, access)
        except:
            #except (pywintypes.error, pywintypes.api_error):
            self.handle = win32api.RegCreateKey(handle, keyname)
    def __getattr__(self, name):
        if name[0]=='_':
            raise AttributeError( "'%s' object has no attribute '%s'"%(self.__class__.__name__, name) )
        # create subkey
        return self.__class__(name, self.handle, self.mode)
    def __setattr__(self, name, value):
        if name in ('handle','mode') or name in self.__dict__:
            self.__dict__[name]=value
            return
        # create new key
        self.__getattr__(name)[''] = value
    def __getitem__(self, name):
        value, type = win32api.RegQueryValueEx( self.handle, name)
        if type==win32con.REG_MULTI_SZ:
            return tuple(value)
        return value
    def __setitem__(self, name, value):
        if isinstance(value, (tuple, list)):
            value = list(map(str, value))
            win32api.RegSetValueEx( self.handle, name, None, win32con.REG_MULTI_SZ, value)
        elif isinstance(value, bytes):
            win32api.RegSetValueEx( self.handle, name, None, win32con.REG_SZ, value.decode("UTF8"))
        else:
            win32api.RegSetValueEx( self.handle, name, None, win32con.REG_SZ, str(value))
    def __del__(self):
        # close key
        if self.handle is not None:
            win32api.RegCloseKey(self.handle)
            self.handle = None
        # XXX todo: delete ourselves if key has been created newly but no value has been written
    def DeleteTree(self, name):
        win32api.RegDeleteTree( self.handle, name)



def register_extensions(fileextensions, appname):
    """register program with extensions for automatic loading of files"""

    prefix = "Software\\Classes\\"
    try:
        key1 = RegistryCurrentUser('.%s'%fileextensions[0])
    except:
        print("Could not register file extension(s) - no registry access")
        return

    for fileextension in fileextensions:
        key1 = RegistryCurrentUser(prefix + '.%s'%fileextension)
        name = '%s.File'%appname
        try:
            current = key1['']
        except:
            current = None
        try:
            if not current or current!=name:
                key1[''] = name
        except pywintypes.error:
            print("Could not register file extension %s"%fileextension)
        # register command
        key2 = RegistryCurrentUser(prefix+name)
        value2 = '%s "%s" "%%1"'%(sys.executable, os.path.abspath( sys.argv[0] ))
        try:
            current = key2.shell.open.command['']
        except:
            current = None
        if not current or current!=value2 or True:
            try:
                key2.shell.open.command[''] = value2
            except:
                print("Could not register file extension %s"%fileextension)


def check_for_key(keyname, branch=None):
    if branch is None:
        branch = win32con.HKEY_CURRENT_USER
    try:
        handle = win32api.RegOpenKeyEx(branch, keyname, 0, win32con.KEY_READ)
        win32api.RegCloseKey(handle)
        return True
    except pywintypes.error:
        return False


def check_for_screen_reader():
    if check_for_key("SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\App Paths\\nvda.exe", win32con.HKEY_LOCAL_MACHINE):
        return "NVDA"
    if check_for_key("Software\\Freedom Scientific\\JAWS"):
        return "JAWS"
    return None
