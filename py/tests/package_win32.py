import os
import re
import sys
import argparse

def existing_empty_dir(s):
    if s == "":
        msg = "{} is not a non-empty directory path".format(s)
        raise argparse.ArgumentTypeError(msg)
    v = os.path.abspath(s)
    if not os.path.isdir(v):
        msg = ("Path {} is not an existing directory "
               "(path absolutized from {})").format(v, s)
        raise argparse.ArgumentTypeError(msg)
    ls = os.listdir(v)
    if ls:
        msg = ("Path {} is not an empty directory "
               "because it contains {} entries i.e. {}"
               "(path absolutized from {})").format(v, len(ls), str(ls), s)
        raise argparse.ArgumentTypeError(msg)
    return v

def read_argv(argv):
    parser = argparse.ArgumentParser(description='Integration test the packaging for Windows')
    parser.add_argument('--workdir', type=existing_empty_dir, required=True,
                        help='Working directory for testing. It must exist and be empty.')
    parser.add_argument('--packagespecfile', type=argparse.FileType('r'), required=True,
                        help='package specification file. It must exist.')

    args = parser.parse_args()
    return (args.workdir, args.packagespecfile)

def finish_test(test_failed):
    if test_failed:
        sys.exit('FAILED')

def is_launcher_defined(launcher, spec):
    def_block = re.search('(\[{0}\])(.*?)(\n\n|\n\Z)'.format(launcher), spec, re.DOTALL)

    if def_block:
        if def_block.group(2):
            return True

    return False

def test_package_launchers(spec):
    launchers = []

    launchers_block = re.search('(launchers\ \=)(.*?)\n\n', spec, re.DOTALL)
    if launchers_block and launchers_block.group(2):
        launchers = filter(None, re.split('[ \\n]+', launchers_block.group(2)))

    if len(launchers) == 0:
        print('List of launchers must not be empty')
        return True

    missing_launcher_defs = filter(lambda l: not is_launcher_defined(l, spec), launchers)
    if len(missing_launcher_defs) > 0:
        print('Launchers are missing a definition block: {0}'.format(missing_launcher_defs))
        return True

    return False

def main(argv):
    test_failed = False
    root_dir, package_spec_file = read_argv(argv)
    curr_dir = os.getcwd()

    test_failed = test_package_launchers(package_spec_file.read())

    finish_test(test_failed)

if __name__ == "__main__":
    main(sys.argv)
