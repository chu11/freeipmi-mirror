#!/usr/bin/perl
#############################################################################
#
# ganglia_ipmi_sensors.sh
#
# This script can be used to monitor IPMI sensors in ganglia via
# FreeIPMI's ipmi-sensors utility.
#
# Currently only temperature, fan, and voltage sensors are monitored.
#
# Options:
#
# -h - specify hostname(s) to remotely access (don't specify for inband)
# -S - specify an alternate ipmi-sensors location
# -s - specify additional ipmi-sensors arguments
# -G - specify an alternate gmetric location
# -g - specify additional gmetric arguments
#
# Environment Variables:
#
# IPMI_HOSTS - specify hostname(s) to remotely access (don't specify for inband)
# IPMI_SENSORS_PATH - specify an alternate ipmi-sensors location
# IPMI_SENSORS_ARGS - specify additional ipmi-sensors arguments
# GMETRIC_PATH - specify an alternate gmetric location
# GMETRIC_ARGS - specify additional gmetric arguments
#
# Setup Notes:
#
# Specify the remote hosts you wish to access IPMI information from
# via the -h option or IPMI_HOSTS environment variable.  If you wish
# only to monitor the local node, do not specify an ipmi host.  The
# input to the -h option is passed directly to ipmi-sensors.  So you
# may specify anything the ipmi-sensors tool accepts including
# hostranged (i.e. foo[0-127]) or comma separated
# (i.e. foo0,foo1,foo2,foo3) inputs.  If you wish to monitor both
# remote and local system, remember to specify one of the hosts as
# "localhost".
#
# If stored in a non-default location the -S option or
# IPMI_SENSORS_PATH environment variable must be specified to
# determine the ipmi-sensors location.
#
# If stored in a non-default location the -G option or GMETRIC_PATH
# environment variable must be specified to determine the gmetric
# location.
#
# In order to specify non-defaults for ipmi-sensors use the -s
# argument or IPMI_SENSORS_ARGS environment variable.  Typically, this
# option is necessary for non-default communication information or
# authentication information (i.e. driver path, driver type, username,
# password, etc.).
#
# In order to specify a non-defaults for gmetric, use the -g argument
# or GMETRIC_ARGS environment variable.  Typically, this option is
# necessary for non-default gmond.conf paths (i.e. -c
# /myspecial/gmond.conf).
#
# Most users will want to set this script to execute in cron(8).
# Using cron you may monitor at whatever interval you wish.
#
#############################################################################

use strict;

use Getopt::Std;

use Socket;

my $debug = 0;

my $IPMI_HOSTS = undef;
my $IPMI_SENSORS_PATH = "/usr/sbin/ipmi-sensors";
my $IPMI_SENSORS_ARGS = "";
my $GMETRIC_PATH = "/usr/bin/gmetric";
my $GMETRIC_ARGS = "";

my $IPMI_SENSORS_OUTPUT;
my @IPMI_SENSORS_OUTPUT_LINES;
my $line;

my $cmd;

sub usage
{
    my $prog = $0;
    print "Usage: $prog -h <hostname(s)> -S <path> -s <sensors arguments> -G <path> -g <arguments> -d\n";
    print "  -h specify hostname(s) to remotely access\n";
    print "  -S specify an alternate ipmi-sensors path\n";
    print "  -s specify additional ipmi-sensors arguments\n";
    print "  -G specify an alternate gmetric path\n";
    print "  -g specify additional gmetric arguments\n";
    print "  -d print debug info\n";
    exit 0;
}

if (!getopts("h:S:s:G:g:d"))
{
    usage();
}

if (defined($main::opt_h))
{
    $IPMI_HOSTS = $main::opt_h;
}

if (defined($main::opt_S))
{
    $IPMI_SENSORS_PATH = $main::opt_S;
}

if (defined($main::opt_s))
{
    $IPMI_SENSORS_ARGS = $main::opt_s;
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

if ($ENV{"IPMI_HOSTS"})
{
    $IPMI_HOSTS = $ENV{"IPMI_HOSTS"};
}

if ($ENV{"IPMI_SENSORS_PATH"})
{
    $IPMI_SENSORS_PATH = $ENV{"IPMI_SENSORS_PATH"};
}

if ($ENV{"IPMI_SENSORS_ARGS"})
{
    $IPMI_SENSORS_ARGS = $ENV{"IPMI_SENSORS_ARGS"};
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
    print "IPMI_SENSORS_PATH=$IPMI_SENSORS_PATH\n";
    print "IPMI_SENSORS_ARGS=$IPMI_SENSORS_ARGS\n";
    print "GMETRIC_PATH=$GMETRIC_PATH\n";
    print "GMETRIC_ARGS=$GMETRIC_ARGS\n";
}

if (!(-x $IPMI_SENSORS_PATH))
{
    print "$IPMI_SENSORS_PATH cannot be executed\n";
    exit(1);
}

if (!(-x $GMETRIC_PATH))
{
    print "$GMETRIC_PATH cannot be executed\n";
    exit(1);
}

if ($IPMI_HOSTS)
{
    $cmd = "$IPMI_SENSORS_PATH $IPMI_SENSORS_ARGS -h $IPMI_HOSTS --quiet-cache --always-prefix";
}
else
{
    $cmd = "$IPMI_SENSORS_PATH $IPMI_SENSORS_ARGS --quiet-cache --always-prefix"
}

if ($debug)
{
    print "ipmi-sensors command: $cmd\n";
}

$IPMI_SENSORS_OUTPUT = `$cmd`;
if ($? != 0)
{
    print "$IPMI_SENSORS_PATH: failed\n";
    exit(1);
}

@IPMI_SENSORS_OUTPUT_LINES = split(/\n/, $IPMI_SENSORS_OUTPUT);

foreach $line (@IPMI_SENSORS_OUTPUT_LINES)
{
    if ($debug)
    {
        print "Parsing: $line\n";
    }

    if ($line =~ /Temperature/
        || $line =~ /Voltage/
        || $line =~ /Fan/)
    {
        my $hostname;
        my $record_id;
        my $id_string;
        my $group;
        my $reading;
        my $unit;
        my $threshold_low;
        my $threshold_high;
        my $state;

        if ($line =~ /(.+)\: (\d+)\: (.+) \((.+)\)\: (.+) (.+) \((.+)\/(.+)\)\: \[(.+)\]/)
        {
            $hostname = $1;
            $record_id = $2;
            $id_string = $3;
            $group = $4;
            $reading = $5;
            $unit = $6;
            $threshold_low = $7;
            $threshold_high = $8;
            $state = $9;
        }
        elsif ($line =~ /(\d+)\: (.+) \((.+)\)\: (.+) (.+) \((.+)\/(.+)\)\: \[(.+)\]/)
        {
            $hostname = undef;
            $record_id = $1;
            $id_string = $2;
            $group = $3;
            $reading = $4;
            $unit = $5;
            $threshold_low = $6;
            $threshold_high = $7;
            $state = $8;
        }
        else
        {
            print "Line not parsable\n";
            next;
        }

        $id_string =~ s/ /_/;
        $id_string =~ s/\//_/;

        if ($hostname)
        {
            my $packet_ip = gethostbyname($hostname);
            my $ip_address;

            if (defined($packet_ip))
            {
                $ip_address = inet_ntoa($packet_ip);
            }
            else
            {
                print "Cannot resolve ip: $hostname\n";
                next;
            }

            if ($hostname eq "localhost" || $hostname eq "127.0.0.1")
            {
                $cmd = "$GMETRIC_PATH $GMETRIC_ARGS -n $id_string -v $reading -t double -u $unit";
            }
            else
            {
                $cmd = "$GMETRIC_PATH $GMETRIC_ARGS -n $id_string -v $reading -t double -u $unit -S $ip_address:$hostname";
            }
        }
        else
        {
            $cmd = "$GMETRIC_PATH $GMETRIC_ARGS -n $id_string -v $reading -t double -u $unit";
        }

        if ($debug)
        {
            print "gmetric command = $cmd\n";
        }

        `$cmd`;
        if ($? != 0)
        {
            print "\"$cmd\": failed\n";
            exit(1);
        }        
    }
}

