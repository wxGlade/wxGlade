; Inno Setup script file - Full Edition
;
; Copyright: 2007 Alberto Griggio
; Copyright: 2011-2016 Carsten Grohmann
; License: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY

; define own variables
#define PRODUCT_VERSION "HG"

; EDITION_SHORT will be defined in wxglade-SAE-installer.iss only

#ifdef EDITION_SHORT
  #define DIST_DIR        "..\..\dist\bdist"
  #define EXECUTABLE_NAME "{app}\wxglade.exe"
  #define EDITION_LONG    " (Standalone Edition)"
#else
  #define DIST_DIR        "..\.."
  #define EXECUTABLE_NAME "{app}\wxglade.pyw"
  #define EDITION_LONG    ""
  #undef  EDITION_SHORT
#endif

; Exclude from standalone edition as well as from full edition
#define EXCLUDE_ALWAYS "\kdefiledialog.py,\README.txt,\docs\src,\docs\man"


[Setup]
AppName=wxGlade
AppVerName=wxGlade {#EDITION_LONG} Version {#PRODUCT_VERSION}
AppPublisher=the wxGlade team
AppPublisherURL=http://wxglade.sourceforge.net
AppSupportURL=http://wxglade.sourceforge.net
AppUpdatesURL=http://wxglade.sourceforge.net
DefaultDirName={pf}\wxGlade
DefaultGroupName=wxGlade
AllowNoIcons=yes
ChangesAssociations=yes
LicenseFile="LICENSE.txt"
SourceDir="{#DIST_DIR}"
#ifdef EDITION_SHORT
  OutputDir="..\..\dist"
  OutputBaseFilename=wxGlade-SAE-{#PRODUCT_VERSION}-setup
#else
  OutputDir="dist"
  OutputBaseFilename=wxGlade-{#PRODUCT_VERSION}-setup
#endif

[Tasks]
Name: "desktopicon"; Description: "Create a &desktop icon"; GroupDescription: "Additional icons:"
Name: "quicklaunchicon"; Description: "Create a &Quick Launch icon"; GroupDescription: "Additional icons:"; Flags: unchecked

[Files]
; NOTE: Don't use "Flags: ignoreversion" on any shared system files
#ifdef EDITION_SHORT
  Source: "*"; \
    Excludes: "{#EXCLUDE_ALWAYS}"; \
    DestDir: "{app}"; \
    Flags: ignoreversion recursesubdirs
#else
  Source: "*"; \
    Excludes: "{#EXCLUDE_ALWAYS},\bdist\*,\build\*,dist\*,\install\*,.hg,\.hgignore,\.hgtags,\logdict*.log,\tests,\test.py,\warnwxglade.txt,*.pyc,*.pyo,Makefile,epydoc.conf,setup.py";  \
    DestDir: "{app}"; \
    Flags: ignoreversion recursesubdirs
#endif
Source: "README.txt"; \
  DestDir: "{app}"; \
  Flags: isreadme;

[INI]
Filename: "{app}\wxglade.url"; Section: "InternetShortcut"; Key: "URL"; String: "http://wxglade.sourceforge.net"

[Icons]
Name: "{group}\wxGlade{#EDITION_LONG}"; Filename: "{#EXECUTABLE_NAME}"; IconFilename: "{app}\icons\wxglade.ico"; WorkingDir: "{app}"
Name: "{group}\Home page"; Filename: "{app}\wxglade.url"
Name: "{group}\Documentation\Tutorial";            Filename: "{app}\docs\tutorial.html";   WorkingDir: "{app}\docs";       Comment: "wxGlade Tutorial"
Name: "{group}\Documentation\User Manual (PDF)";   Filename: "{app}\docs\pdf\manual.pdf";  WorkingDir: "{app}\docs\pdf";   Comment: "wxGlade User Manual (PDF)"
Name: "{group}\Documentation\User Manual (HTML)";  Filename: "{app}\docs\html\index.html";  WorkingDir: "{app}\docs\html"; Comment: "wxGlade User Manual (HTML)"
Name: "{group}\Documentation\TODO";    Filename: "{app}\TODO.txt";    AfterInstall: Unix2Dos('{app}\TODO.txt');
Name: "{group}\Documentation\README";  Filename: "{app}\README.txt";  AfterInstall: Unix2Dos('{app}\README.txt');
Name: "{group}\Documentation\License"; Filename: "{app}\LICENSE.txt"; AfterInstall: Unix2Dos('{app}\LICENSE.txt');
Name: "{group}\Documentation\Credits"; Filename: "{app}\CREDITS.txt"; AfterInstall: Unix2Dos('{app}\CREDITS.txt');
Name: "{group}\Documentation\Changes"; Filename: "{app}\CHANGES.txt"; AfterInstall: Unix2Dos('{app}\CHANGES.txt');
Name: "{group}\Documentation\Contributing"; Filename: "{app}\CONTRIBUTING.txt"; AfterInstall: Unix2Dos('{app}\CONTRIBUTING.txt');
Name: "{group}\Documentation\News";    Filename: "{app}\NEWS.txt";    AfterInstall: Unix2Dos('{app}\NEWS.txt');
Name: "{group}\Uninstall wxGlade{#EDITION_LONG}";  Filename: "{uninstallexe}"
Name: "{userdesktop}\wxGlade{#EDITION_LONG}";  Filename: "{#EXECUTABLE_NAME}"; Tasks: desktopicon; IconFilename: "{app}\icons\wxglade.ico"; WorkingDir: "{app}"
Name: "{userappdata}\Microsoft\Internet Explorer\Quick Launch\wxGlade{#EDITION_LONG}"; Filename: "{#EXECUTABLE_NAME}"; Tasks: quicklaunchicon; IconFilename: "{app}\icons\wxglade.ico"; WorkingDir: "{app}"

[Run]
Filename: "{#EXECUTABLE_NAME}"; \
  Description: "Launch wxGlade{#EDITION_LONG}"; \
  Flags: nowait postinstall skipifsilent shellexec; \
  WorkingDir: "{app}"

[UninstallDelete]
Type: files; Name: "{app}\wxglade.url"
Name: "{app}\*.pyc"; Type: files
Name: "{app}\*.pyo"; Type: files
Name: "{app}\codegen\*.pyc"; Type: files
Name: "{app}\codegen\*.pyo"; Type: files
Name: "{app}\edit_sizers\*.pyc"; Type: files
Name: "{app}\edit_sizers\*.pyo"; Type: files
Name: "{app}\wcodegen\*.pyc"; Type: files
Name: "{app}\wcodegen\*.pyo"; Type: files
Name: "{app}\widgets\bitmap_button\*.pyc"; Type: files
Name: "{app}\widgets\bitmap_button\*.pyo"; Type: files
Name: "{app}\widgets\button\*.pyc"; Type: files
Name: "{app}\widgets\button\*.pyo"; Type: files
Name: "{app}\widgets\calendar_ctrl\*.pyc"; Type: files
Name: "{app}\widgets\calendar_ctrl\*.pyo"; Type: files
Name: "{app}\widgets\check_list_box\*.pyc"; Type: files
Name: "{app}\widgets\check_list_box\*.pyo"; Type: files
Name: "{app}\widgets\checkbox\*.pyc"; Type: files
Name: "{app}\widgets\checkbox\*.pyo"; Type: files
Name: "{app}\widgets\choice\*.pyc"; Type: files
Name: "{app}\widgets\choice\*.pyo"; Type: files
Name: "{app}\widgets\combo_box\*.pyc"; Type: files
Name: "{app}\widgets\combo_box\*.pyo"; Type: files
Name: "{app}\widgets\custom_widget\*.pyc"; Type: files
Name: "{app}\widgets\custom_widget\*.pyo"; Type: files
Name: "{app}\widgets\datepicker_ctrl\*.pyc"; Type: files
Name: "{app}\widgets\datepicker_ctrl\*.pyo"; Type: files
Name: "{app}\widgets\dialog\*.pyc"; Type: files
Name: "{app}\widgets\dialog\*.pyo"; Type: files
Name: "{app}\widgets\frame\*.pyc"; Type: files
Name: "{app}\widgets\frame\*.pyo"; Type: files
Name: "{app}\widgets\gauge\*.pyc"; Type: files
Name: "{app}\widgets\gauge\*.pyo"; Type: files
Name: "{app}\widgets\generic_calendar_ctrl\*.pyc"; Type: files
Name: "{app}\widgets\generic_calendar_ctrl\*.pyo"; Type: files
Name: "{app}\widgets\grid\*.pyc"; Type: files
Name: "{app}\widgets\grid\*.pyo"; Type: files
Name: "{app}\widgets\hyperlink_ctrl\*.pyc"; Type: files
Name: "{app}\widgets\hyperlink_ctrl\*.pyo"; Type: files
Name: "{app}\widgets\list_box\*.pyc"; Type: files
Name: "{app}\widgets\list_box\*.pyo"; Type: files
Name: "{app}\widgets\list_ctrl\*.pyc"; Type: files
Name: "{app}\widgets\list_ctrl\*.pyo"; Type: files
Name: "{app}\widgets\menubar\*.pyc"; Type: files
Name: "{app}\widgets\menubar\*.pyo"; Type: files
Name: "{app}\widgets\notebook\*.pyc"; Type: files
Name: "{app}\widgets\notebook\*.pyo"; Type: files
Name: "{app}\widgets\panel\*.pyc"; Type: files
Name: "{app}\widgets\panel\*.pyo"; Type: files
Name: "{app}\widgets\property_grid_manager\*.pyc"; Type: files
Name: "{app}\widgets\property_grid_manager\*.pyo"; Type: files
Name: "{app}\widgets\radio_box\*.pyc"; Type: files
Name: "{app}\widgets\radio_box\*.pyo"; Type: files
Name: "{app}\widgets\radio_button\*.pyc"; Type: files
Name: "{app}\widgets\radio_button\*.pyo"; Type: files
Name: "{app}\widgets\slider\*.pyc"; Type: files
Name: "{app}\widgets\slider\*.pyo"; Type: files
Name: "{app}\widgets\spacer\*.pyc"; Type: files
Name: "{app}\widgets\spacer\*.pyo"; Type: files
Name: "{app}\widgets\spin_button\*.pyc"; Type: files
Name: "{app}\widgets\spin_button\*.pyo"; Type: files
Name: "{app}\widgets\spin_ctrl\*.pyc"; Type: files
Name: "{app}\widgets\spin_ctrl\*.pyo"; Type: files
Name: "{app}\widgets\splitter_window\*.pyc"; Type: files
Name: "{app}\widgets\splitter_window\*.pyo"; Type: files
Name: "{app}\widgets\static_bitmap\*.pyc"; Type: files
Name: "{app}\widgets\static_bitmap\*.pyo"; Type: files
Name: "{app}\widgets\statusbar\*.pyc"; Type: files
Name: "{app}\widgets\statusbar\*.pyo"; Type: files
Name: "{app}\widgets\static_line\*.pyc"; Type: files
Name: "{app}\widgets\static_line\*.pyo"; Type: files
Name: "{app}\widgets\static_text\*.pyc"; Type: files
Name: "{app}\widgets\static_text\*.pyo"; Type: files
Name: "{app}\widgets\text_ctrl\*.pyc"; Type: files
Name: "{app}\widgets\text_ctrl\*.pyo"; Type: files
Name: "{app}\widgets\toggle_button\*.pyc"; Type: files
Name: "{app}\widgets\toggle_button\*.pyo"; Type: files
Name: "{app}\widgets\toolbar\*.pyc"; Type: files
Name: "{app}\widgets\toolbar\*.pyo"; Type: files
Name: "{app}\widgets\tree_ctrl\*.pyc"; Type: files
Name: "{app}\widgets\tree_ctrl\*.pyo"; Type: files
Name: "{app}\widgets\*.pyc"; Type: files
Name: "{app}\widgets\*.pyo"; Type: files
Name: "{app}\widgets"; Type: dirifempty
Name: "{app}\install\pyinstaller\*.pyc"; Type: files
Name: "{app}\install\pyinstaller\*.pyo"; Type: files
Name: "{app}\install\srcpkgs\*.pyc"; Type: files
Name: "{app}\install\srcpkgs\*.pyo"; Type: files
Name: "{app}\install\*.pyc"; Type: files
Name: "{app}\install\*.pyo"; Type: files

; remove application directory too
Name: "{app}"; Type: dirifempty

[Registry]
Root: HKCR; Subkey: ".wxg"; ValueType: string; ValueName: ""; ValueData: "wxGladeResourceFile"; Flags: uninsdeletevalue
Root: HKCR; Subkey: "wxGladeResourceFile"; ValueType: string; ValueName: ""; ValueData: "wxGlade Resource File"; Flags: uninsdeletekey
Root: HKCR; Subkey: "wxGladeResourceFile\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{#EXECUTABLE_NAME}"" ""%1"""
Root: HKCR; Subkey: "wxGladeResourceFile\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\icons\wxg_file.ico"
Root: HKCU; Subkey: "Software\Microsoft\Windows\ShellNoRoam\MUICache"; ValueType: none; ValueName: "{uninstallexe}"; Flags: dontcreatekey uninsdeletevalue
Root: HKCU; Subkey: "Software\Microsoft\Windows\ShellNoRoam\MUICache"; ValueType: none; ValueName: "{app}\wxglade.pyw"; Flags: dontcreatekey uninsdeletevalue
Root: HKCU; Subkey: "Software\Microsoft\Windows\ShellNoRoam\MUICache"; ValueType: none; ValueName: "{app}\wxglade.py";  Flags: dontcreatekey uninsdeletevalue
Root: HKCU; Subkey: "Software\Microsoft\Windows\ShellNoRoam\MUICache"; ValueType: none; ValueName: "{app}\wxglade.exe"; Flags: dontcreatekey uninsdeletevalue

[Code]
// Convert a file from Unix EOL style (LF) to Windows EOL style (CRLF)
procedure Unix2Dos(SrcFile: String);
var
    FileContent: String;
begin
    // Expand constants first
    SrcFile := ExpandConstant(SrcFile);
    Log('Convert Unix newline to Windows newline for ' + SrcFile);
    //Load srcfile to a string
    LoadStringFromFile(SrcFile, FileContent);
    //Replace Fraomstring by toString in file string content
    StringChangeEx(FileContent, #13, '', True);
    StringChangeEx(FileContent, #10, #13#10, True);
    //Replace old content srcfile by the new content
    DeleteFile(SrcFile);
    SaveStringToFile(SrcFile,FileContent, True);
end;
