import os, sys

# adjust this to your environment
WXGLADE_DIST_DIR = r'c:\documents and settings\alb\desktop\wxglade-bin\distwxglade'

TEMPLATE = 'wxglade-installer.iss.template'
OUTPUT = 'wxglade-installer.iss'

def main():
    out = open(OUTPUT, 'w')
    pad_string = "<PAD>"
    for line in open(TEMPLATE):
        out.write(line.replace(pad_string, WXGLADE_DIST_DIR))
    out.close()


if __name__ == '__main__':
    main()
