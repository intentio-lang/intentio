import argparse
import datetime
import sys
from time import time

import runcookiecutter


def run():
    parser = argparse.ArgumentParser(
        description='Build Intentio', epilog='Happy hacking!')

    subparsers = parser.add_subparsers(dest='cmd', title='subcommands')
    subparsers.required = True

    runcookiecutter.install_args(
        subparsers.add_parser('cookiecutter', help='run cookiecutter'))

    args = parser.parse_args()
    args.func(args)


def main():
    start_time = time()
    try:
        run()
        if not help_triggered():
            print('Task completed successfully in',
                  format_build_time(time() - start_time))
    except (SystemExit, KeyboardInterrupt) as error:
        if hasattr(error, 'code') and isinstance(error.code, int):
            exit_code = error.code
        else:
            exit_code = 1
            print(error)
        if not help_triggered():
            print('Task completed unsuccessfully in',
                  format_build_time(time() - start_time))

        sys.exit(exit_code)


def help_triggered():
    return ('-h' in sys.argv) or ('--help' in sys.argv) or (len(sys.argv) == 1)


def format_build_time(duration):
    return str(datetime.timedelta(seconds=int(duration)))


if __name__ == '__main__':
    main()
