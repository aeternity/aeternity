#!/usr/bin/env python3
import sys, typing, argparse, glob, os

def process_log(node: str, input_file: typing.TextIO, output: typing.TextIO):
    for raw in input_file:
        ldate, ltime, llevel, lsender, lmsg = raw.split(' ', 4)
        output.write(f"{node} {ltime} {llevel[1:-1]} {lsender} {lmsg}")

def start(dir: str):
    # Find the "latest" run directory and go in, find the node dirs
    ct_run_dirs = glob.glob(os.path.join(dir, 'latest.*'))[0] # Assume one and only one exists
    dev_dirs: list[str] = glob.glob(os.path.join(ct_run_dirs, 'dev*'))
    parent_dev_dirs: list[str] = glob.glob(os.path.join(ct_run_dirs, 'parent_chain_dev*'))
    nodes = map(os.path.basename, dev_dirs + parent_dev_dirs)
    print("Found {} child chain (dev) dirs and {} parent (parent_chain_dev) dirs: {}"
          .format(len(dev_dirs), len(parent_dev_dirs), nodes))

    output_file = open('shiviz_out.log', 'wt')

    def make_input(node_path: str) -> typing.Tuple[str, str]:
        node_basename = os.path.basename(node_path)
        return os.path.join(node_path, 'log/aeternity_mining.log'), node_basename
    input_paths = list(map(make_input, dev_dirs + parent_dev_dirs))
    input_paths.sort()

    print("Reading logs from: {}".format(input_paths))
    for log_path, node in input_paths:
        try:
            with open(log_path, 'rt') as input_file:
                process_log(node, input_file, output_file)
        except FileNotFoundError:
            pass

def main():
    parser = argparse.ArgumentParser(
                        prog='AeLogViewer',
                        description='Given a test run results root, opens a web page with all node logs visualised',
                        epilog='Thank you for using AeLogViewer',)
    parser.add_argument('dir', help='Root directory of test run results')
    args = parser.parse_args()
    if args.dir is None:
        parser.print_help()
        sys.exit(1)

    start(args.dir)

if __name__ == "__main__":
    main()
