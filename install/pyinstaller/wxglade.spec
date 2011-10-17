# -*- python -*-

PYTHON_DIR = r'C:\Python23'
WXGLADE_EXE_SCRIPTS_DIR = r'C:\Documents and settings\alb\Desktop\wxglade-bin'
WXGLADE_DIR = r'C:\Documents and settings\alb\Desktop\wxGlade-0.5'

a = Analysis(
    [ PYTHON_DIR + r'\Lib\site-packages\Installer\support\_mountzlib.py',
      PYTHON_DIR + r'\Lib\site-packages\Installer\support\useUnicode.py',
      WXGLADE_EXE_SCRIPTS_DIR + r'\support.py',
      WXGLADE_DIR + r'\wxglade.py',
      ])
pyz = PYZ(a.pure)
exe = EXE(pyz,
          a.scripts,
          exclude_binaries=1,
          name='buildwxglade/wxglade.exe',
          debug=0,
          strip=0,
          console=0,
          icon='mondrian.ico',
          upx=1)
coll = COLLECT( exe,
                a.binaries,
                strip=0,
                name='distwxglade')#,
##                use_support_dir=True)
