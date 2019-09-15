import argparse
from aetherling.modules.permutation import get_latency
from aetherling.space_time.space_time_types import *

parser = argparse.ArgumentParser()
parser.add_argument(
    'file',
    type=str,
    help='File to put results.'
)
subparsers = parser.add_subparsers(dest='subcommand')

#  subparser for latency compute
parser_latency = subparsers.add_parser('reshape_latency')
# add a required argument
parser_latency.add_argument(
    'in_type',
    type=str,
    help='Input type for reshape.')
parser_latency.add_argument(
    'out_type',
    type=str,
    help='Output type for reshape.')

#  subparser for upload
#parser_upload = subparsers.add_parser('upload')
## add a required argument
#parser_upload.add_argument(
#    'server',
#    choices=['amazon', 'imgur'],
#    help='Upload the file to this service.')

args = parser.parse_args()

if __name__ == "__main__":
    if args.subcommand == 'reshape_latency':
        latency = get_latency(eval(args.in_type), eval(args.out_type))
        with open(args.file, 'w+') as f:
            f.write(str(latency))

