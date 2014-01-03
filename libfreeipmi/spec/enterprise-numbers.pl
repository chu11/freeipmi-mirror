#!/usr/bin/perl

# This script should be used to process the IANA Private Enterprise
# list from http://www.iana.org/assignments/enterprise-numbers.  This
# script is *very* simple.  Strip out the beginning and ending
# text/cruft in the textfile before inputting it into this script.
# You will need to fix some line wrapping issues in the main text file
# too.  I think there was one or two "empty lines" that are accidently
# in the file too.

use strict;

use Getopt::Std;

my @lines = ();
my $line_number;
my $line_organization;
my $line_contact;
my $line_email;
my $count = 0;

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

print STDOUT ("
/*
 * Copyright (C) 2003-2014 FreeIPMI Core Team
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>

#include \"freeipmi/spec/ipmi-iana-enterprise-numbers-spec.h\"

#include \"freeipmi-portability.h\"

const char *const ipmi_iana_enterprise_numbers[] = 
{
\n");

# Format is:
#
# <num>
#   <organization>
#     <contact>
#       <email>
#
# We only care about the number and organization.

#@lines = <FH>;

while (<FH>)
{
    $line_number = $_;
    $line_organization = <FH>;
    $line_contact = <FH>;
    $line_email = <FH>;
    $line_number = trim($line_number);
    $line_organization = trim($line_organization);

    while ($count < $line_number)
    {
        print("    NULL, /* $count */\n");
        $count++;
    }

    $count++;

    # escape slashes

    $line_organization =~ s/\\/\\\\/g;

    # escape quotes - do after slashes for obvious reasons

    $line_organization =~ s/\"/\\\"/g;

    # Some fields are empty, I have no idea why.
    if ($line_organization)
    {
        print("    \"$line_organization\", /* $line_number */\n");
    }
    else
    {
        print("    NULL, /* $line_number */\n");
    }
}

printf("};\n");
