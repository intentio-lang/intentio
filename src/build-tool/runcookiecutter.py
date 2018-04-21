from argparse import ArgumentParser
import os

from cookiecutter.main import cookiecutter

from utils import SRC_DIR

TEMPLATE_ROOT = os.path.join(
    os.path.dirname(os.path.abspath(__file__)), 'cookiecutters')


def install_args(parser: ArgumentParser):
    parser.add_argument(
        'template_name', choices=list_available_cookiecutters())
    parser.set_defaults(func=runcookiecutter)


def runcookiecutter(args):
    cookiecutter(
        os.path.join(TEMPLATE_ROOT, args.template_name), output_dir=SRC_DIR)


def list_available_cookiecutters():
    return [
        d for d in os.listdir(TEMPLATE_ROOT)
        if os.path.isdir(os.path.join(TEMPLATE_ROOT, d))
    ]
