#!/usr/bin/perl
#############################################################################
# Copyright (C) 2003-2008 FreeIPMI Core Team
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software Foundation,
# Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.
#############################################################################
#
# nagios_ipmimonitoring.sh
#
# Author: 
#
# Albert Chu <chu11 at llnl dot gov>
#
# Description:
#
# This script can be used to monitor IPMI sensors in nagios via
# FreeIPMI's ipmimonitoring utility.  The Nominal, Warning, and
# Critical states of each sensor will be collected and counted.  The
# overall IPMI sensor status will be mapped into a Nagios status of
# OK, Warning, or Critical.  Details will then be output for Nagios to
# read.
#
# Options:
#
# -h - specify hostname(s) to remotely access (don't specify for inband)
# -M - specify an alternate ipmimonitoring location
# -m - specify additional ipmimonitoring arguments
# -d - print debug info
# -H - output help
#
# Environment Variables:
#
# IPMI_HOSTS - specify hostname(s) to remotely access (don't specify for inband)
# IPMIMONITORING_PATH - specify an alternate ipmimonitoring location
# IPMIMONITORING_ARGS - specify additional ipmimonitoring arguments
#
# Setup Notes:
#
# Specify the remote hosts you wish to access IPMI information from
# via the -h option or IPMI_HOSTS environment variable.  If you wish
# only to monitor the local node, do not specify an ipmi host.  The
# input to the -h option is passed directly to ipmimonitoring.  So you
# may specify anything the ipmimonitoring tool accepts including
# hostranged (i.e. foo[0-127]) or comma separated
# (i.e. foo0,foo1,foo2,foo3) inputs.  If you wish to monitor both
# remote and local system, remember to specify one of the hosts as
# "localhost".  Most will probably want to monitor just one host (get
# the IPMI status for each individual machine being monitored),
# however more than one host can be analyzed for a collective result.
#
# If stored in a non-default location the -M option or
# IPMIMONITORING_PATH environment variable must be specified to
# determine the ipmimonitoring location.
#
# In order to specify non-defaults for ipmimonitoring use the -m
# argument or IPMIMONITORING_ARGS environment variable.  Typically,
# this option is necessary for non-default communication information
# or authentication information (i.e. driver path, driver type,
# username, password, etc.).  Non-default communication information
# can also be stored in the FreeIPMI configuration file.  This is the
# suggested method because passwords and other sensitive information
# could show up in ps(1).  If you wish to limit the sensors being
# monitored, you can also specify which record-ids are to be monitored
# (-s option).
#
# The setup for this can vary depending on your environment and nagios
# setup, but most will need to set this up in nagios by defining a
# command and then a service.
#
# define command  {
#    command_name nagios_ipmimonitoring
#    command_line /path/nagios_ipmimonitoring.pl -h $ARG1$
# }
#
# define service {
#    host_name           foohost
#    service_description ipmi
#    check_command       nagios_ipmimonitoring!foohost
# }
#
# The default session timeout length in ipmimonitoring is 20 seconds.
# We would recommend that IPMI not be monitored more frequently than
# that.
#
# Help:
# 
# Report bugs to freeipmi-users@gnu.org or freeipmi-devel@gnu.org.
#
#############################################################################

use strict;

use Getopt::Std;

my $debug = 0;

my $IPMI_HOSTS = undef;
my $IPMIMONITORING_PATH = "/usr/sbin/ipmimonitoring";
my $IPMIMONITORING_ARGS = "";

my $IPMIMONITORING_OUTPUT;
my @IPMIMONITORING_OUTPUT_LINES;
my $line;

my $cmd;

my $num_output = 0;
my $warning_num = 0;
my $critical_num = 0;
my $fatal_error = 0;

sub usage
{
    my $prog = $0;
    print "Usage: $prog -h <hostname(s)> -M <path> -m <sensors arguments> -d -H\n";
    print "  -h specify hostname(s) to remotely access\n";
    print "  -M specify an alternate ipmimonitoring path\n";
    print "  -m specify additional ipmimonitoring arguments\n";
    print "  -d print debug info\n";
    print "  -H output help\n";
    exit 0;
}

if (!getopts("h:M:m:dH"))
{
    usage();
}

if (defined($main::opt_H))
{
    usage();
}

if (defined($main::opt_h))
{
    $IPMI_HOSTS = $main::opt_h;
}

if (defined($main::opt_M))
{
    $IPMIMONITORING_PATH = $main::opt_M;
}

if (defined($main::opt_m))
{
    $IPMIMONITORING_ARGS = $main::opt_m;
}

if (defined($main::opt_d))
{
    $debug = 1;
}

if ($ENV{"IPMI_HOSTS"})
{
    $IPMI_HOSTS = $ENV{"IPMI_HOSTS"};
}

if ($ENV{"IPMIMONITORING_PATH"})
{
    $IPMIMONITORING_PATH = $ENV{"IPMIMONITORING_PATH"};
}

if ($ENV{"IPMIMONITORING_ARGS"})
{
    $IPMIMONITORING_ARGS = $ENV{"IPMIMONITORING_ARGS"};
}

if ($debug)
{
    print "IPMI_HOSTS=$IPMI_HOSTS\n";
    print "IPMIMONITORING_PATH=$IPMIMONITORING_PATH\n";
    print "IPMIMONITORING_ARGS=$IPMIMONITORING_ARGS\n";
}

if (!(-x $IPMIMONITORING_PATH))
{
    print "$IPMIMONITORING_PATH cannot be executed\n";
    exit(1);
}

if ($IPMI_HOSTS)
{
    $cmd = "$IPMIMONITORING_PATH $IPMIMONITORING_ARGS -h $IPMI_HOSTS --quiet-cache --sdr-cache-recreate --always-prefix";
}
else
{
    $cmd = "$IPMIMONITORING_PATH $IPMIMONITORING_ARGS --quiet-cache --sdr-cache-recreate --always-prefix"
}

if ($debug)
{
    print "ipmimonitoring command: $cmd\n";
}

$IPMIMONITORING_OUTPUT = `$cmd`;
if ($? != 0)
{
    print "$IPMIMONITORING_PATH: failed\n";
    exit(1);
}

@IPMIMONITORING_OUTPUT_LINES = split(/\n/, $IPMIMONITORING_OUTPUT);

foreach $line (@IPMIMONITORING_OUTPUT_LINES)
{
    my $hostname;
    my $record_id;
    my $id_string;
    my $group;
    my $state;
    my $reading;
    my $unit;
    my $id_string_state;

    my $output_str;

    # skip header line
    if ($line =~ "Record_ID")
    {
        next;
    }

    if ($debug)
    {
        print "Parsing: $line\n";
    }

    if ($line =~ /(.+)\: (\d+) \| (.+) \| (.+) \| (.+) \| (.+) \| (.+)/)
    {
        $hostname = $1;
        $record_id = $2;
        $id_string = $3;
        $group = $4;
        $state = $5;
        $unit = $6;
        $reading = $7;
    }
    else
    {
        print "Line not parsable\n";
        $fatal_error++;
        next;
    }

    $id_string =~ s/ /_/g;
    $id_string =~ s/\//_/g;

    if ($state eq "Nominal") 
    {
        next;
    }

    if ($state eq "Warning")
    {
        $warning_num++;
        $output_str = "WARNING";
    }
    elsif ($state eq "Critical")
    {
        $critical_num++;
        $output_str = "CRITICAL";
    }
    else
    {
        print "State not parsable\n";
        $fatal_error++;
        next;
    } 

    if ($num_output)
    {
        print "; ";
    }
    print "$id_string - $output_str";
    $num_output++;
}

# Nagios Exit Codes
# 0 = OK
# 1 = WARNING
# 2 = CRITICAL
# 3 = UNKNOWN

if ($fatal_error)
{
    exit 3;                     
}

if ($critical_num)
{
    exit 2;
}

if ($warning_num)
{
    exit 1;
}

exit 0;
