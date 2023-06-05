#!/usr/bin/perl

# This script should be used to process the IANA Private Enterprise
# list from https://www.iana.org/assignments/enterprise-numbers.txt.
#
# It handles a number of corner case formatting errors in the list.
# Hopefully new ones don't crop up when you update.

use strict;

use Getopt::Std;

my @lines = ();
my $line_number;
my $line_organization;
my $line_contact;
my $line_email;
my $count = 0;
my $found_beginning = 0;
my $tmp;

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
 * Copyright (C) 2003-2015 FreeIPMI Core Team
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
    $line_number = trim($line_number);

    # There are some empty lines between some records.  Why?  No idea.
    if ($line_number eq "") {
        next;
    }

    if ($line_number eq "End of Document") {
        last;
    }

    # Iterate until you hit '0', there is comment text at the top
    if ($found_beginning == 0) {
        if ($line_number ne "0") {
            next;
        }
        else {
            $found_beginning++;
        }
    }

    $line_organization = <FH>;

    # Some random empty lines in the records, why?
    # Note, do not check for whitespace, b/c whitespace can mean empty string for field.
    # We are specifically checking for empty line w/o anything.
    while (chomp($line_organization) eq "") {
        $line_organization = <FH>;
     }

    $line_contact = <FH>;

    # more empty strings between lines in record
    while (chomp($line_contact) eq "") {
        $line_contact = <FH>;
     }

    # Some organization names linger onto the next lines, even
    # multiple lines.  I have no idea why some lines are formatted
    # this way.  It doesn't appear to be based on length of the
    # organization name or anything.
    $tmp = substr($line_contact, 0, 1);
    while (!($tmp =~ /\s/)) {
        $line_organization = trim($line_organization);
        $line_organization = "$line_organization $line_contact";
        $line_contact = <FH>;
        $tmp = substr($line_contact, 0, 1);
    }

    $line_organization = trim($line_organization);

    # Some companies have some indication of former names, we'll
    # remove em b/c we don't care about it.
    if ($line_organization =~ /formerly/) {
        $line_organization =~ s/\(formerly .*\)//;
        $line_organization = trim($line_organization);
    }

    if ($line_organization =~ /previous/) {
        $line_organization =~ s/\(previous was .*\)//;
        $line_organization = trim($line_organization);
    }

    if ($line_organization =~ /http/) {
        $line_organization =~ s/\<http:\/\/.*\>//;
        $line_organization = trim($line_organization);
    }

    $line_email = <FH>;

    # Likewise contact names can linger onto the next line.  No idea
    # why.  It seems random
    $tmp = substr($line_email, 0, 1);
    while (!($tmp =~ /\s/)) {
        $line_contact = $line_contact + " " + $line_email;
        $line_email = <FH>;
        $tmp = substr($line_email, 0, 1);
    }

    # Fill in any missing numbers
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
