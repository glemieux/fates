import argparse

from landusedata.luh2 import luh2print
from landusedata.luh2 import main as luh2main
from landusedata.landusepft import lupftprint

def main(argv=None):

    # Define top level parser
    parser = argparse.ArgumentParser(description="FATES landuse data tool")

    # Target regrid file - is there a nice way to share this between subparsers?
    # parser.add_argument('regrid_target_file', help='target surface data file with desired grid resolution')

    # Define subparser option for luh2 or landuse x pft data tool subcommands
    subparsers = parser.add_subparsers(required=True, title='subcommands',
                                       help='landuse data tool subcommand options')
    luh2_parser = subparsers.add_parser('luh2', prog='luh2',
                                        help='generate landuse harmonization timeseries data output')
    lupft_parser = subparsers.add_parser('lupft', prog='lupft',
                                         help='generate landuse x pft static data map output')

    # Set the default called function for the subparser command
    luh2_parser.set_defaults(func=luh2main)
    lupft_parser.set_defaults(func=lupftprint)

    # LUH2 subparser arguments
    luh2_parser.add_argument('regridder_target_file',
                             help='target surface data file with desired grid resolution')
    luh2_parser.add_argument('luh2_file',
                             help = "full path of luh2 raw states file")
    luh2_parser.add_argument("luh2_static_file",
                             help = "luh2 static data file")
    luh2_parser.add_argument("-w", "--regridder_weights",
                             default = 'regridder.nc',
                             help = "filename of regridder weights to write to or reuse (if -m option used)")
    luh2_parser.add_argument("-b","--begin",
                             type = int,
                             default = None,
                             help = "beginning of date range of interest")
    luh2_parser.add_argument("-e","--end",
                             type = int,
                             default = None,
                             help = "ending of date range to slice")
    luh2_parser.add_argument("-o","--output",
                             default = 'LUH2_timeseries.nc',
                             help = "output filename")
    luh2_parser.add_argument("-m", "--luh2_merge_file",
                             default = None,
                             help = "previous luh2 output filename to merge into current run output")

    # Landuse x pft subparser arguments
    lupft_parser.add_argument('regrid_target_file',
                             help='target surface data file with desired grid resolution')
    lupft_parser.add_argument('lupft_surf_file',
                         help = "full path of the CLM landuse x pft surface data file")

    # Parse the arguments
    args = parser.parse_args(argv)

    # Call the default function for the given command
    args.func(args)

    # Return successful completion
    return 0

# Gaurd against import time side effects
if __name__ == '__main__':
    raise SystemExit(main())
