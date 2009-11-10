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
# ganglia_ipmimonitoring.sh
#
# Author: 
#
# Albert Chu <chu11 at llnl dot gov>
#
# Description:
#
# This script can be used to monitor IPMI sensors in ganglia via
# FreeIPMI's ipmimonitoring utility.  Data will be passed into ganglia
# via ganglia's gmetric utility.
#
# All sensors currently supported by libipmimonitoring(3) (at last
# check this is: temperature, voltage, current, fan, physical
# security, platform security violation attempt, processor, power
# supply, power unit, memory, drive slot, system firmware progress,
# event logging disabled, system event, critical interrupt, module
# board, slot connector, watchdog2, entity presence, management
# subsystem health, battery, fru state, cable interconnect, boot
# error) will have their sensor state (Nominal, Warning, or Critical)
# monitored.
#
# Only the actual sensor reading values for temperature, fan, and voltage
# sensors will be monitored.
#
# Options:
#
# -h - specify hostname(s) to remotely access (don't specify for inband)
# -M - specify an alternate ipmimonitoring location
# -m - specify additional ipmimonitoring arguments
# -G - specify an alternate gmetric location
# -g - specify additional gmetric arguments
# -d - print debug info
# -D - do not send sensor data to ganglia (useful during debugging)
# -H - output help
#
# Environment Variables:
#
# IPMI_HOSTS - specify hostname(s) to remotely access (don't specify for inband)
# IPMIMONITORING_PATH - specify an alternate ipmimonitoring location
# IPMIMONITORING_ARGS - specify additional ipmimonitoring arguments
# GMETRIC_PATH - specify an alternate gmetric location
# GMETRIC_ARGS - specify additional gmetric arguments
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
# "localhost".
#
# If stored in a non-default location the -M option or
# IPMIMONITORING_PATH environment variable must be specified to
# determine the ipmimonitoring location.
#
# If stored in a non-default location the -G option or GMETRIC_PATH
# environment variable must be specified to determine the gmetric
# location.
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
# In order to specify non-defaults for gmetric, use the -g argument
# or GMETRIC_ARGS environment variable.  Typically, this option is
# necessary for non-default gmond.conf paths (i.e. -c
# /myspecial/gmond.conf).
#
# Most users will want to set this script to execute in cron(8).
# Using cron you may monitor at whatever interval you wish.  The
# recommended interval should be atleast longer than 20 seconds, since
# that is the default session timeout length.  
#
# Help:
# 
# Report bugs to freeipmi-users@gnu.org or freeipmi-devel@gnu.org.
#
#############################################################################

use strict;

use Getopt::Std;

use Socket;

my $debug = 0;
my $no_ganglia = 0;

my $IPMI_HOSTS = undef;
my $IPMIMONITORING_PATH = "/usr/sbin/ipmimonitoring";
my $IPMIMONITORING_ARGS = "";
my $GMETRIC_PATH = "/usr/bin/gmetric";
my $GMETRIC_ARGS = "";

my $IPMIMONITORING_OUTPUT;
my @IPMIMONITORING_OUTPUT_LINES;
my $line;

my $cmd;

sub usage
{
    my $prog = $0;
    print "Usage: $prog -h <hostname(s)> -M <path> -m <sensors arguments> -G <path> -g <arguments> -d -H\n";
    print "  -h specify hostname(s) to remotely access\n";
    print "  -M specify an alternate ipmimonitoring path\n";
    print "  -m specify additional ipmimonitoring arguments\n";
    print "  -G specify an alternate gmetric path\n";
    print "  -g specify additional gmetric arguments\n";
    print "  -d print debug info\n";
    print "  -D do not send sensor data to ganglia (useful during debugging)\n";
    print "  -H output help\n";
    exit 0;
}

if (!getopts("h:M:m:G:g:dDH"))
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

if (defined($main::opt_G))
{
    $GMETRIC_PATH = $main::opt_G;
}

if (defined($main::opt_g))
{
    $GMETRIC_ARGS = $main::opt_g;
}

if (defined($main::opt_d))
{
    $debug = 1;
}

if (defined($main::opt_D))
{
    $no_ganglia = 1;
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

if ($ENV{"GMETRIC_PATH"})
{
    $GMETRIC_PATH = $ENV{"GMETRIC_PATH"};
}

if ($ENV{"GMETRIC_ARGS"}) 
{
    $GMETRIC_ARGS = $ENV{"GMETRIC_ARGS"};
}

if ($debug)
{
    print "IPMI_HOSTS=$IPMI_HOSTS\n";
    print "IPMIMONITORING_PATH=$IPMIMONITORING_PATH\n";
    print "IPMIMONITORING_ARGS=$IPMIMONITORING_ARGS\n";
    print "GMETRIC_PATH=$GMETRIC_PATH\n";
    print "GMETRIC_ARGS=$GMETRIC_ARGS\n";
}

if (!(-x $IPMIMONITORING_PATH))
{
    print "$IPMIMONITORING_PATH cannot be executed\n";
    exit(1);
}

if (!$no_ganglia)
{
    if (!(-x $GMETRIC_PATH))
    {
        print "$GMETRIC_PATH cannot be executed\n";
        exit(1);
    }
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

    my $ip_address;

    my $cmd_reading;

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
        next;
    }

    $id_string =~ s/ /_/g;
    $id_string =~ s/\//_/g;
    
    if ($hostname ne "localhost" && $hostname ne "127.0.0.1")
    {
        my $packet_ip = gethostbyname($hostname);
        
        if (defined($packet_ip))
        {
            $ip_address = inet_ntoa($packet_ip);
                }
        else
        {
            print "Cannot resolve ip: $hostname\n";
            next;
        }
    }
   
    $id_string_state = $id_string . "_State";
    if ($hostname ne "localhost" && $hostname ne "127.0.0.1")
    {
        $cmd = "$GMETRIC_PATH $GMETRIC_ARGS -n $id_string_state -v $state -t string -S $ip_address:$hostname";
    }
    else
    {
        $cmd = "$GMETRIC_PATH $GMETRIC_ARGS -n $id_string_state -v $state -t string";
    }

    if ($group eq "Temperature"
        || $group eq "Voltage"
        || $group eq "Fan")
    {
        if ($hostname ne "localhost" && $hostname ne "127.0.0.1")
        {
            $cmd_reading = "$GMETRIC_PATH $GMETRIC_ARGS -n $id_string -v $reading -t double -u $unit -S $ip_address:$hostname";
        }
        else
        {
            $cmd_reading = "$GMETRIC_PATH $GMETRIC_ARGS -n $id_string -v $reading -t double -u $unit";
        }
    }

    if ($debug)
    {
        print "gmetric command = $cmd\n";
        if ($cmd_reading)
        {
            print "gmetric command = $cmd_reading\n";
        }
    }

    if (!$no_ganglia)
    {
        `$cmd`;
        if ($? != 0)
        {
            print "\"$cmd\": failed\n";
            exit(1);
        }
        if ($cmd_reading)
        {
            `$cmd_reading`;
            if ($? != 0)
            {
                print "\"$cmd_reading\": failed\n";
                exit(1);
            }
        }
    }
}

