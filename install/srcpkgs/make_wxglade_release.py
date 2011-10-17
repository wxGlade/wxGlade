#!/usr/bin/env python

"""
Script to generate a tarball for a wxGlade release, from a mercurial repository
"""

import os, sys
import cStringIO
import optparse
import glob
import mercurial.commands as hg

DEFAULT_REPO = 'http://wxglade.sourceforge.net/hg/wxGlade'

class BadCommand(Exception):
    def __init__(self, args):
        super(Exception, self).__init__("Failed command: hg %s" % \
                                        " ".join(args))

class ReleaseError(Exception):
    pass


def parse_args():
    parser = optparse.OptionParser("%prog [options] [wxGlade REPOSITORY]")
    parser.add_option('-r', '--rev', help='Revision to use. If not given, '
                      'use the latest revision tagged "release X"')
    parser.add_option('-o', '--outdir', default=os.getcwd(),
                      help='output directory. Default: current dir. '
                      'Must be writable')
    parser.add_option('-t', '--type', help='Type of archive to generate: '
                      'zip, gz or bz2. Can be specified multiple times. If none'
                      ' is specified, a gz (.tar.gz) will be generated',
                      action='append')
    def usage(msg=None):
        if msg: print msg, "\n"
        parser.print_help()
        sys.exit(1)

    opts, args = parser.parse_args()
    return opts, args, usage


def hg_cmd(*args):
    """\
    Execute a mercurial command and return its output.
    Raise BadCommand on error.
    """
    print "Executing: hg %s" % " ".join(args)
    out = cStringIO.StringIO()
    sys.stdout = out
    try:
        r = hg.dispatch(list(args))
        if r:
            raise BadCommand(args)
        return out.getvalue()
    finally:
        sys.stdout = sys.__stdout__
        out.close()
        

def main():
    opts, args, usage = parse_args()
    if len(args) > 1:
        usage("Too many repositories given")
    if not args:
        repo = DEFAULT_REPO
    else:
        repo = args[0]
        
    # 1. clone the repository
    new_repo = os.path.join(opts.outdir, 'wxglade_dist')
    if os.path.exists(new_repo):
        raise ReleaseError('Temporary output dir "%s" exists' % new_repo)
    hg_cmd('clone', repo, new_repo)
    os.chdir(new_repo)
    # 2. update to the given rev, or to the latest "release tag"
    release_str = None
    if opts.rev:
        hg_cmd('update', '-r', opts.rev)
        release_str = \
                    hg_cmd('log', '-r', opts.rev).splitlines()[0].split(':')[-1]
    else:
        # look for the latest "release tag"
        for line in hg_cmd('tags').splitlines():
            tag, rev = [t.strip() for t in line.split()]
            if tag.startswith('rel_'):
                # found, update and set release_str
                release_str = tag[4:]
                hg_cmd('update', '-r', tag)
                break
        if release_str is None:
            # no release found, abort
            raise ReleaseError('No "release tag" found')
            
    # now, remove the .hg dir and .hgignore file
    os.system('rm -rf .hg*')
    # compile the docs
    if os.system('cd docs && make html') != 0:
        print "WARNING: impossible to generate documentation"
    # generate the translations from .po files
    for catalog in glob.glob('po/*.po'):
        lang = os.path.splitext(os.path.basename(catalog))[0]
        os.system('mkdir -p locale/%s/LC_MESSAGES' % lang)
        if os.system('msgfmt %s -o locale/%s/LC_MESSAGES/wxglade.mo' %
                     (catalog, lang)) != 0:
            print 'WARNING: impossible to generate "%s" translation' % lang
    # rename the directory and create the tarballs
    os.chdir('..')
    os.rename('wxglade_dist', 'wxGlade-%s' % release_str)
    if not opts.type: opts.type = 'gz'
    # .tar.gz
    if 'gz' in opts.type:
        if os.system('tar czf wxGlade-%s.tar.gz wxGlade-%s' % 
                     (release_str, release_str)) != 0:
            raise ReleaseError('Impossible to create tarball '
                               '"%s/wxGlade-%s.tar.gz"' %
                               (opts.outdir, release_str))
        print 'Archive "%s/wxGlade-%s.tar.gz" created successfully' % \
              (opts.outdir, release_str)
    # .tar.bz2
    if 'bz2' in opts.type:
        if os.system('tar cjf wxGlade-%s.tar.bz2 wxGlade-%s' % 
                     (release_str, release_str)) != 0:
            raise ReleaseError('Impossible to create tarball '
                               '"%s/wxGlade-%s.tar.bz2"' %
                               (opts.outdir, release_str))
        print 'Archive "%s/wxGlade-%s.tar.bz2" created successfully' % \
              (opts.outdir, release_str)
    # .zip
    if 'zip' in opts.type:
        if os.system('zip -r wxGlade-%s.zip wxGlade-%s' % 
                     (release_str, release_str)) != 0:
            raise ReleaseError('Impossible to create tarball '
                               '"%s/wxGlade-%s.zip"' %
                               (opts.outdir, release_str))
        print 'Archive "%s/wxGlade-%s.zip" created successfully' % \
              (opts.outdir, release_str)
    os.system('rm -rf wxGlade-%s' % release_str)


if __name__ == '__main__':
    main()
