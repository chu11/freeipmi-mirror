#!/usr/bin/perl

# This script should be used to process the raw text copied out of
# JEDEC 106 pdf.  This script is *very* simple.

use strict;

use Getopt::Std;

my @lines = ();
my $line;

sub usage
{
    my $prog = `basename $0`;

    chomp($prog);
    print "Usage: $prog <filename>\n";
    exit 2;
}

# Perl trim function to remove whitespace from the start and end of the string
sub trim
{
    my $string = shift;
    $string =~ s/^\s+//;
    $string =~ s/\s+$//;
    return $string;
}

if (!defined($ARGV[0]))
{
    usage();
}

if (!open(FH, "< $ARGV[0]")) {
    print STDERR ("Couldn't open $ARGV[0]: $!\n");
    exit 1;
}

# Format is:
#
# <num> <company name> <bit> <bit> <bit> <bit> <bit> <bit> <bit> <bit> <idhex>
#
# We only care about the company name and idhex.

while (($line = <FH>))
{
    # print "$line\n";

    if ($line =~ /\d+ (.+) [01] [01] [01] [01] [01] [01] [01] [01] (.+)/) {
        print "{ 0x$2, \"$1\" },\n";
    }
    else {
        print "\n";
    }
}
