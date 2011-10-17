#! /usr/bin/env python
# -*- coding: iso-8859-1 -*-
# 
#   PYTHON MODULE:     MKI18N.PY
#                      =========
# 
#   Abstract:         Make Internationalization (i18n) files for an application.
# 
#   Copyright Pierre Rouleau. 2003. Released to public domain.
# 
#   Last update: Saturday, November 8, 2003. @ 15:55:18.
# 
#   File: ROUP2003N01::C:/dev/python/mki18n.py
# 
#   RCS $Header: //software/official/MKS/MKS_SI/TV_NT/dev/Python/rcs/mki18n.py 1.5 2003/11/05 19:40:04 PRouleau Exp $
# 
#   Update history:
# 
#   - File created: Saturday, June 7, 2003. by Pierre Rouleau
#   - 10/06/03 rcs : RCS Revision 1.1  2003/06/10 10:06:12  PRouleau
#   - 10/06/03 rcs : RCS Initial revision
#   - 23/08/03 rcs : RCS Revision 1.2  2003/06/10 10:54:27  PRouleau
#   - 23/08/03 P.R.: [code:fix] : The strings encoded in this file are encode in iso-8859-1 format.  Added the encoding
#                    notification to Python to comply with Python's 2.3 PEP 263.
#   - 23/08/03 P.R.: [feature:new] : Added the '-e' switch which is used to force the creation of the empty English .mo file.
#   - 22/10/03 P.R.: [code] : incorporated utility functions in here to make script self sufficient.
#   - 05/11/03 rcs : RCS Revision 1.4  2003/10/22 06:39:31  PRouleau
#   - 05/11/03 P.R.: [code:fix] : included the unixpath() in this file.
#   - 08/11/03 rcs : RCS Revision 1.5  2003/11/05 19:40:04  PRouleau
# 
#   RCS $Log: $
# 
# 
# -----------------------------------------------------------------------------
"""                                
mki18n allows you to internationalize your software.  You can use it to 
create the GNU .po files (Portable Object) and the compiled .mo files
(Machine Object).

mki18n module can be used from the command line or from within a script (see 
the Usage at the end of this page).

    Table of Contents
    -----------------
    
    makePO()             -- Build the Portable Object file for the application --
    catPO()              -- Concatenate one or several PO files with the application domain files. --
    makeMO()             -- Compile the Portable Object files into the Machine Object stored in the right location. --
    printUsage           -- Displays how to use this script from the command line --

    Scriptexecution      -- Runs when invoked from the command line --


NOTE:  this module uses GNU gettext utilities.

You can get the gettext tools from the following sites:

   - `GNU FTP site for gettetx`_ where several versions (0.10.40, 0.11.2, 0.11.5 and 0.12.1) are available.
     Note  that you need to use `GNU libiconv`_ to use this. Get it from the `GNU
     libiconv  ftp site`_ and get version 1.9.1 or later. Get the Windows .ZIP
     files and install the packages inside c:/gnu. All binaries will be stored
     inside  c:/gnu/bin.  Just  put c:/gnu/bin inside your PATH. You will need
     the following files: 

      - `gettext-runtime-0.12.1.bin.woe32.zip`_ 
      - `gettext-tools-0.12.1.bin.woe32.zip`_
      - `libiconv-1.9.1.bin.woe32.zip`_ 


.. _GNU libiconv:                            http://www.gnu.org/software/libiconv/
.. _GNU libiconv ftp site:                   http://www.ibiblio.org/pub/gnu/libiconv/
.. _gettext-runtime-0.12.1.bin.woe32.zip:    ftp://ftp.gnu.org/gnu/gettext/gettext-runtime-0.12.1.bin.woe32.zip           
.. _gettext-tools-0.12.1.bin.woe32.zip:      ftp://ftp.gnu.org/gnu/gettext/gettext-tools-0.12.1.bin.woe32.zip 
.. _libiconv-1.9.1.bin.woe32.zip:            http://www.ibiblio.org/pub/gnu/libiconv/libiconv-1.9.1.bin.woe32.zip

"""
# -----------------------------------------------------------------------------
# Module Import
# -------------
# 
import os
import sys
import wx
# -----------------------------------------------------------------------------
# Global variables
# ----------------
#

__author__ = "Pierre Rouleau"
__version__= "$Revision: 1.5 $"

# -----------------------------------------------------------------------------

def getlanguageDict():
    languageDict = {}
    
    for lang in [x for x in dir(wx) if x.startswith("LANGUAGE")]:
        i = wx.Locale(wx.LANGUAGE_DEFAULT).GetLanguageInfo(getattr(wx, lang))
        if i:
            languageDict[i.CanonicalName] = i.Description

    return languageDict

def makePO(applicationDirectoryPath,  applicationDomain=None, verbose=0) :
    """Build the Portable Object Template file for the application.

    makePO builds the .pot file for the application stored inside 
    a specified directory by running xgettext for all application source 
    files.  It finds the name of all files by looking for a file called 'app.fil'. 
    If this file does not exists, makePo raises an IOError exception.
    By default the application domain (the application
    name) is the same as the directory name but it can be overridden by the
    'applicationDomain' argument.

    makePO always creates a new file called messages.pot.  If it finds files 
    of the form app_xx.po where 'app' is the application name and 'xx' is one 
    of the ISO 639 two-letter language codes, makePO resynchronizes those 
    files with the latest extracted strings (now contained in messages.pot). 
    This process updates all line location number in the language-specific
    .po files and may also create new entries for translation (or comment out 
    some).  The .po file is not changed, instead a new file is created with 
    the .new extension appended to the name of the .po file.

    By default the function does not display what it is doing.  Set the 
    verbose argument to 1 to force it to print its commands.
    """

    if applicationDomain is None:
        applicationName = os.path.basename(os.path.splitext(applicationDirectoryPath)[0])
    else:
        applicationName = applicationDomain
    currentDir = os.getcwd()
    os.chdir(applicationDirectoryPath)                    
    if not os.path.exists('app.fil'):
        raise IOError(2,'No module file: app.fil')

    cmd = 'xgettext -s --no-wrap --files-from=app.fil --output=messages.pot'
    if verbose: print cmd
    os.system(cmd)                                                

    languageDict = getlanguageDict()

    for langCode in languageDict.keys():
        if langCode == 'en':
            pass
        else:
            langPOfileName = "%s_%s.po" % (applicationName , langCode)
            if os.path.exists(langPOfileName):
                cmd = 'msgmerge -s --no-wrap "%s" messages.pot > "%s.new"' % (langPOfileName, langPOfileName)
                if verbose: print cmd
                os.system(cmd)
    os.chdir(currentDir)

def catPO(applicationDirectoryPath, listOf_extraPo, applicationDomain=None, targetDir=None, verbose=0) :
    """Concatenate one or several PO files with the application domain files.
    """

    if applicationDomain is None:
        applicationName = os.path.basename(os.path.splitext(applicationDirectoryPath)[0])
    else:
        applicationName = applicationDomain
    currentDir = os.getcwd()
    os.chdir(applicationDirectoryPath)

    languageDict = getlanguageDict()

    for langCode in languageDict.keys():
        if langCode == 'en':
            pass
        else:
            langPOfileName = "%s_%s.po" % (applicationName , langCode)
            if os.path.exists(langPOfileName):
                fileList = ''
                for fileName in listOf_extraPo:
                    fileList += ("%s_%s.po " % (fileName,langCode))
                cmd = "msgcat -s --no-wrap %s %s > %s.cat" % (langPOfileName, fileList, langPOfileName)
                if verbose: print cmd
                os.system(cmd)
                if targetDir is None:
                    pass
                else:
                    mo_targetDir = "%s/%s/LC_MESSAGES" % (targetDir,langCode)
                    cmd = "msgfmt --output-file=%s/%s.mo %s_%s.po.cat" % (mo_targetDir,applicationName,applicationName,langCode)
                    if verbose: print cmd
                    os.system(cmd)
    os.chdir(currentDir)

def makeMO(applicationDirectoryPath,targetDir='./locale',applicationDomain=None, verbose=0, forceEnglish=0) :
    """Compile the Portable Object files into the Machine Object stored in the right location.

    makeMO converts all translated language-specific PO files located inside 
    the  application directory into the binary .MO files stored inside the 
    LC_MESSAGES sub-directory for the found locale files.

    makeMO searches for all files that have a name of the form 'app_xx.po' 
    inside the application directory specified by the first argument.  The 
    'app' is the application domain name (that can be specified by the 
    applicationDomain argument or is taken from the directory name). The 'xx' 
    corresponds to one of the ISO 639 two-letter language codes.

    makeMo stores the resulting files inside a sub-directory of `targetDir`
    called xx/LC_MESSAGES where 'xx' corresponds to the 2-letter language
    code.
    """
    if targetDir is None:
        targetDir = './locale'
    if verbose:
        print "Target directory for .mo files is: %s" % targetDir

    if applicationDomain is None:
        applicationName = os.path.basename(os.path.splitext(applicationDirectoryPath)[0])
    else:
        applicationName = applicationDomain
    currentDir = os.getcwd()
    os.chdir(applicationDirectoryPath)

    languageDict = getlanguageDict()

    for langCode in languageDict.keys():
        if (langCode == 'en') and (forceEnglish==0):
            pass
        else:
            langPOfileName = "%s_%s.po" % (applicationName , langCode)
            if os.path.exists(langPOfileName):
                mo_targetDir = "%s/%s/LC_MESSAGES" % (targetDir,langCode) 
                if not os.path.exists(mo_targetDir):
                    os.makedirs(mo_targetDir)
                cmd = 'msgfmt --output-file="%s/%s.mo" "%s_%s.po"' % (mo_targetDir,applicationName,applicationName,langCode)
                if verbose: print cmd
                os.system(cmd)
    os.chdir(currentDir)
   
# ----------------------------------------------------------------------------- 

# S c r i p t   e x e c u t i o n               -- Runs when invoked from the command line --
# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# 
if __name__ == "__not_used__":#"__main__":
    import getopt     # command line parsing
    argc = len(sys.argv)
    if argc == 1:
        printUsage('Missing argument: specify at least one of -m or -p (or both).')
        sys.exit(1)
    # If there is some arguments, parse the command line
    validOptions     = "ehmpv"
    validLongOptions = ['domain=', 'moTarget=']             
    option = {}
    option['forceEnglish'] = 0
    option['mo'] = 0
    option['po'] = 0        
    option['verbose'] = 0
    option['domain'] = None
    option['moTarget'] = None
    try:
	    optionList,pargs = getopt.getopt(sys.argv[1:],validOptions,validLongOptions):
    except getopt.GetoptError, e:
        printUsage(e[0])
        sys.exit(1)       
    for (opt,val) in optionList:
        if  (opt == '-h'):    
            printUsage()
            sys.exit(0) 
        elif (opt == '-e'):         option['forceEnglish'] = 1
        elif (opt == '-m'):         option['mo'] = 1
        elif (opt == '-p'):         option['po'] = 1
        elif (opt == '-v'):         option['verbose'] = 1
        elif (opt == '--domain'):   option['domain'] = val
        elif (opt == '--moTarget'): option['moTarget'] = val
    if len(pargs) == 0:
        appDirPath = os.getcwd()
        if option['verbose']:
            print "No project directory given. Using current one:  %s" % appDirPath
    elif len(pargs) == 1:
        appDirPath = pargs[0]
    else:
        printUsage('Too many arguments (%u).  Use double quotes if you have space in directory name' % len(pargs))
        sys.exit(1)
    if option['domain'] is None:
        # If no domain specified, use the name of the target directory
        option['domain'] = os.path.basename(os.path.splitext(appDirPath)[0])
    if option['verbose']:
        print "Application domain used is: '%s'" % option['domain']
    if option['po']:
        try:
            makePO(appDirPath,option['domain'],option['verbose'])
        except IOError, e:
            printUsage(e[1] + '\n   You must write a file app.fil that contains the list of all files to parse.')
    if option['mo']:
        makeMO(appDirPath,option['moTarget'],option['domain'],option['verbose'],option['forceEnglish'])
    sys.exit(1)            
            

# -----------------------------------------------------------------------------

if __name__ == '__main__':
    for p, ds, fs in os.walk(''):
        for f in fs:
            if os.path.splitext(f)[1] == '.py':
                print 'P',os.path.join(p, f)

