#!/usr/bin/perl -w

# This is snmptrapd handler script to alert Platform Event Traps (PET).
# I wrote it because traptoemail distributed with net-snmp-5.3.2.2 is 
# incapable of handling multi-line hexstrings and restricted to email alert. 
#
# This script operates in two modes, traphandle or embperl.  When in 
# traphandle mode, it concatenates the quoted hex string into one long line, 
# then builds structures to resemble embperl mode. Both modes then invokes 
# helper decoder, ipmi-pet(8) from freeipmi, parses the output and alerts
# in given way like email, nagios external command, nsca, etc.
#
# This script is tested on Dell PowerEdge 1950 and PowerEdge R610 servers. 
# Feel free to adjust to meet your need. It's BSD-licensed. See __DATA__
# at the end of file for an example copy of traphandle input.
#
# USAGE
#
# (Note backslash-newline concatenates adjacent lines, so put them in one)
# 
# Put two lines like these in your snmptrapd.conf file:
#
#   traphandle .1.3.6.1.4.1.3183.1.1 /usr/bin/petalert.pl --mode=traphandle \
#     --alert=email --sdrcache SDRCONF -- -f FROM -s SMTPSERVER ADDRESSES
#   authCommunity execute COMMUNITY_STRING
#
# Or, if you prefer embedded perl, 
#
#   perl do "/usr/bin/petalert.pl";
#   perl IpmiPET::main(qw(--mode=embperl --trapoid=OID --sdrcache=SDRCONF \
#     --alert=email -- -s SMTPSERVER -f FROM ADDRESSES));
#
# where:
#     only --mode is required, see "petalert.pl -h".
#
# Bad news is that you have to use numeric representation, so in addition 
# add " -Of -On " to snmptrapd options.
#
# You have to enable PET on IPMI nodes as well, including LAN access, PEF 
# alerting, community, alert policy and destination. You may use bmc-config 
# and pef-config from freeipmi to do those configuration.
#
# You might wish to set up PTR records for IPMI nodes, otherwise, snmptrapd 
# reports <UNKNOWN> to traphandle and the script will fall back to use ip.
#
#
# SDR CACHE FILE MAPPING
# 
# Notice the underlying helper program ipmi-pet(8) normally depends on some 
# sdr cache file, either preinitialized or created on demand. If no credential 
# is supplied, ipmi-pet(8) simply assumes localhost and creates sdr cache 
# which is usually ~/.freeipmi/sdr-cache/sdr-cache-<hostname>.localhost. 
# You may wish to supply preinitialized ones, then use -c sdrmapping.conf to
# associate with IPMI nodes. 
#
# The sdr cache config syntax is: every unindented line starts an sdr cache
# file, followed by any number of indented lines of IPMI nodes. Every IPMI 
# node line may consist of multiple nodes delimited by whitespaces. Comments 
# follow Shell-style, trailing whitespaces are trimmed, empty lines skipped.
#
# For example,
# |/path/to/sdr-cache-file-1
# |  10.2.3.10    # comment
# |
# |/path/to/sdr-cache-file-2
# |  10.2.3.4           # one node
# |  10.2.3.5  10.2.3.6 # two nodes
# |  10.2.3.[7-9]       # trhee nodes in range form
# |  
# ^-- this is the beginning of lines
#
#
# ChangeLog
#
# * Sun 11 Dec 2011 kaiwang.chen@gmail.com
# - Add -W to pass workaround flags to ipmi-pet
#
# * Wed 7  Dec 2011 kaiwang.chen@gmail.com
# - Add --ack support
# - capture exit code of helper
#
# * Mon 14 Nov 2011 kaiwang.chen@gmail.com
# - complete rewritten, supports embperl mode and additional alert methods
#
# * Sat 12 Nov 2011 kaiwang.chen@gmail.com
# - support sdr cache file mapping with -c option
# - add debug log
# - in case of no PTR, fallback to ip
#
# * Sun 06 Nov 2011 kaiwang.chen@gmail.com
# - Inital version

package IpmiPET;

use strict;
use Getopt::Long;

# mapping IPMI nodes to preinitialized sdr caches
my %cache_mapping=();

# options
my %opts = ();

# options and args pass to specific alert mechanisms
my %alert_opts = (); # when use builtin features to alert
my $alert_prog = ""; # when use external program to alert

# logger
my $log_filename = "/var/log/petalert.log";
my %logger_token = ('warn' => 1); # always warn


sub usage {
    print <<"EOF";
USAGE

$0 [OPTIONS] -- [ALERT_SPECIFIC_OPTIONS] ALERT_SPECIFIC_ARGS

  OPTIONS
    -m
    --mode  {traphandle|embperl} 
                Specify mode of execution. Required.
    --ack
                Acknowledge the trap before alert.
    -W
    --workaround
                Sets workaround flags for ipmi-pet to acknowledge the trap.
    -o
    --trapoid  OID
                Sets trapoid in embperl mode, or defaults to "all".
    -c
    --sdrcache  sdr_cache_config 
                Specify the sdr cache configuration file.
    -f
    --log  log_file
                Specify logging file
    -n
    --alert  {mail|nagios|nsca|noop|MODULE}
                Specify alert method. Defaults to "noop".

  ALERT SPECIFIC OPTIONS AND ARGS
  email
    --prog  mailer
                Sets mailer. If not specified, falls back to Net::SNMP.
    mailer_options_and_args

    --server smtp_server
                Sets the smtpserver for Net::SNMP to send the mail through.
                Defaults to "localhost".
    --from from_address
                Sets the email for Net:SNMP to be used on the From: line.
                Defaults to "root".
    to_addresses
                Sets where you want Net::SNMP to send the email to. Required.
    
  nagios
    command_file
                Sets Nagios external command file, a named pipe (FIFO).
                Required.

  nsca
    --prog send_nsca
                Sets path to send_nsca binary.
    send_nsca_options_and_args

  noop          Yes, it is a no-op.

  MODULE <not implemented>
    --prog plugin
                Path to plugin script, which must provides..
    plugin_options_and_args
                Additional arguments passed to plugin as \@ARGV.
EOF

  exit;
}

sub logger {
  my ($token, $msg, $variable) = @_;
  $token ||= "";

  if (open my $fh, ">>", $log_filename) {
    if ($logger_token{":all"} || $logger_token{$token}) {
      if ($variable) {
        use Data::Dumper;
        my $t = $Data::Dumper::Terse;
        $Data::Dumper::Terse = 1;
        print $fh "[".localtime()."] $token $msg " . Dumper($variable);
        $Data::Dumper::Terse = $t;
      }
      else {
        print $fh "[".localtime()."] $token $msg\n";
      }
    }
    close $fh;
  }
}

# extract ip from value like "UDP: [172.23.252.107]:32768"
sub extract_ip {
  my ($ip) = ($_[0] =~ /\[([\d.]+)\]:/);
  return $ip || "0.0.0.0";
}

# decode specified event hexstring into hash like
#   'Time' => '13:16:24',
#   'Event' => 'General Chassis Intrusion ; Intrusion while system On',
#   'System_ID' => '256',
#   'State' => 'Critical',
#   'GUID' => '44454c4c-5000-1059-8043-b2c04f333358',
#   'Date' => 'Oct-15-2011',
#   'Manufacturer_ID' => 'Dell Inc.',
#   'Name' => 'Sensor #115',
#   'Severity' => 'N/A',
#   'Event_Direction' => 'Assertion Event',
#   'Type' => 'Physical_Security'
#
sub decode_pet {
  my ($specific, $event_hexstring, $sdrcache) = @_;

  my $ipmi_pet = "/usr/sbin/ipmi-pet";
  my @o = qw(-v -v --output-event-severity --output-event-state --interpret-oem-data --comma-separated-output);
  if ($sdrcache) { push @o, "--sdr-cache-file", $sdrcache }
  push @o, $specific;
  $event_hexstring =~ s/[^ 0-9a-fA-F]//g; # sanity check
  push @o, split /\s+/, $event_hexstring;

  my @x = ();
  logger("decode", "command line ", [$ipmi_pet, \@o]);
  if (open my $fh, "-|", $ipmi_pet, @o) {
    @x = <$fh>;
    close $fh;
    if ($? >> 8) {
      logger("warn", "decode failure with CHILD_ERROR: $?");
      return;
    }
  }
  else {
    logger("warn", "decoder failure: $!");
    return;
  }
  chomp(@x);

  logger("decode", "output ", \@x);
  my @headers = split /,/, $x[0];
  my @values = split /,/, $x[1];  # TODO support doubly quoted value
  if (@headers != @values) {
    logger("warn", "Spot malicious comma separated value", \@x);
  }
  my %event = ();
  for my $i (0..$#headers) {
    my $h = $headers[$i];
    $h =~ s/ /_/g;
    $event{$h} = $values[$i];
  }
  logger("decode", "event ", \%event);
  return \%event;
}

sub ack_pet {
  my ($specific, $event_hexstring, $host, $workaround) = @_;

  my $ipmi_pet = "/usr/sbin/ipmi-pet";
  my @o = qw(--pet-acknowledge);
  if ($workaround) {
    push @o, "-W", $workaround;
  }
  push @o, "-h", $host;
  push @o, $specific;
  $event_hexstring =~ s/[^ 0-9a-fA-F]//g; # sanity check
  push @o, split /\s+/, $event_hexstring;

  my @x = ();
  logger("ack", "command line ", [$ipmi_pet, \@o]);
  if (open my $fh, "-|", $ipmi_pet, @o) {
    @x = <$fh>;
    close $fh;
    if ($? >> 8) {
      logger("warn", "ackhelper failure with CHILD_ERROR: $?");
    }
  }
  else {
    logger("warn", "ackhelper failure: $!");
  }
}

# ipmi-pet localtime to calendar time
sub pettime {
  my ($event) = @_;
  require POSIX;
  my ($hour,$min,$sec) = split /:/, $event->{Time};
  my ($mon,$mday,$year) = split /-/, $event->{Date};
  $year -= 1900;
  my %m = (
    Jan => 0, Feb =>  1, Mar =>  2, Apr => 3, May =>  4, Jun =>  5,
    Jul => 6, Aug =>  7, Sep =>  8, Oct => 9, Nov => 10, Dec => 11,
  );
  if (exists $m{$mon}) { $mon = $m{$mon} }
  else {
    logger("warn", "pettime month $mon will map to 0, please check ipmi-pet");
    $mon = 0;
  }
  return POSIX::mktime($sec, $min, $hour, $mday, $mon, $year);
}

# convert event to nagios plugin output
# See http://nagios.sourceforge.net/docs/3_0/pluginapi.html
sub nagios_check {
  my ($event) = @_;
  my ($code, $state);

  $state = uc($event->{State});
  if    ($state eq "WARNING")  {$code = 1}
  elsif ($state eq "CRITICAL") {$code = 2}
  elsif ($state eq "OK")       {$code = 0}
  else                         {$code = 3; $state = "UNKNOWN"}

  my $plugin_output = join(" ", $state, "-", map { defined $_ ? $_ : "" } @{%{$event}}{qw(Name Type Event_Direction Event)});
  $plugin_output =~ tr/\n/_/;

  return ($code, $plugin_output);
}

# assemble SMTP DATA, http://cr.yp.to/smtp/mail.html
# TODO return encoded data
sub mail_data {
  my ($from, $to, $pdu_info, $uptime, $event) = @_;

  local $" = ", "; # " balance
  my $data = <<"DATA";
To: @{$to}
From: $from
Subject: PET from $pdu_info->{hostname}: $event->{State} - $event->{Event}

Host: $pdu_info->{hostname} ($pdu_info->{receivedfrom}) uptime $uptime
DATA

  for my $k (qw(Date Time Name Type Event Event_Direction State Severity GUID Manufacturer_ID System_ID)) {
    $data .= "$k: $event->{$k}\n";
  }

  return $data;
}

# embperl NetSNMP::TrapReceiver trap receiver
sub my_receiver {
  my ($pdu_info, $varbindings) = @_;

  #use Data::Dumper;print Dumper($pdu_info); print Dumper($varbindings);
  logger("embperl", "original input is ", \@_);

  # inject hostname
  unless (exists $pdu_info->{hostname}) {
    use Socket;
    my $ip = extract_ip($pdu_info->{receivedfrom});
    $pdu_info->{hostname} = gethostbyaddr(inet_aton($ip), AF_INET) || $ip;
  }

  # do cleanup before processing; values are untouched if -OQ, see snmpcmd(1)
  for (@$varbindings) {
    $_->[1] =~ s/^OID: //;
    $_->[1] =~ s/^IpAddress: //;
    $_->[1] =~ s/^STRING: //;
    if ($_->[1] =~ s/^Hex-STRING: //) {
      $_->[1] =~ tr/\n//;
    }
    if ($_->[1] =~ s/^Timeticks: //) {
      $_->[1] =~ s/^\(\d+\) //;
      $_->[1] =~ s/ days, /:/;
    }
  }
  logger("embperl", "input after cleanup is ", \@_);

  process($pdu_info, $varbindings);
}

# you got it..
sub process {
  my ($pdu_info, $varbindings) = @_;
  my ($event_oid, $specific, $uptime, $event);

  # locate the PET event hex string, and extract specific trap number
  for my $v (@{$varbindings}) {
    if ($v->[0] =~ /^\Q.1.3.6.1.6.3.1.1.4.3.0\E$/) {
      $event_oid = $v->[1];
    }
    if ($v->[0]=~ /^\Q.1.3.6.1.6.3.1.1.4.1.0\E$/) {
      ($specific)=($v->[1]=~/(\d+)$/);
    }
    if ($v->[0] =~ /^\Q.1.3.6.1.2.1.1.3.0\E$/) {
      $uptime = $v->[1];
    }
  }
  $event_oid .= ".1";

  $uptime ||= "00:00:00:00.00";
  if (my ($d,$H,$M,$S,$x) = ($uptime =~ /(\d+):(\d+):(\d+):(\d+)\.(\d+)/)) {
    if ($d > 0)    { $uptime = "${d}d${H}h" }
    elsif ($H > 0) { $uptime = "${H}h${M}m" }
    elsif ($M > 0) { $uptime = "${M}m${S}s" }
    else           { $uptime = "${S}.${x}s" }
  }
  
  # convert event string to human readable form
  for my $v (@{$varbindings}) {
    if ($v->[0] =~ /^\Q$event_oid\E$/) {
      my $ip = extract_ip($pdu_info->{receivedfrom});
      if ($opts{ack}) {
        ack_pet($specific, $v->[1], $ip, $opts{workaround});
      }

      my $sdrcache = resolve_sdrcache($ip);
  
      # decode octet hex string
      $event = decode_pet($specific, $v->[1], $sdrcache);
    }
  }

  # invalid events cease here
  return unless $event;

  alert($pdu_info, $uptime, $event);
  return;
}

# build NetSNMP::TrapReceiver style structures from standard input
#  See NOTIFICATION PROCESSING snmptrapd.conf(5)
sub get_from_stdin {
  my ($stdin) = @_;
  my $hostname = shift @{$stdin};
  my $ipaddress = shift @{$stdin};

  chomp($hostname);
  chomp($ipaddress);

  # in case of no PTR records available for the IPMI node
  if($hostname eq "<UNKNOWN>" && (my $ip = extract_ip($ipaddress))) {
    $hostname = $ip;
  }

  # some defaults, blindly.. to resemble those by NetSNMP::TrapReceiver
  my %pdu_info = (
    notificationtype   =>  "TRAP",
    hostname           =>  $hostname,
    receivedfrom       =>  $ipaddress,
    version            =>  0,
    errorstatus        =>  0,
    messageid          =>  0,
    transactionid      =>  1,
    errorindex         =>  0,
    requestid          =>  0,
  );
  
  my @varbindings= ();
  my ($oid,$value);
  my $more = 0;
  my $line = "";
  for (@{$stdin}) {
      chomp;
      if ($more == 0 && $line) {
          ($oid, $value) = ($line =~ /([^\s]+)\s+(.*)/);
          $line = "";
          push @varbindings, [$oid, $value, "="];
      }
  
      # recognize doubly quoted context
      my $count = 0;
      my $x = -1;
      $x=index($_, q{"});
      while ($x >= 0) {
         unless ($x > 0 && substr($_, $x-1, 1) eq "\\") {
             $count++;
         }
         $x += 1;
         $x=index($_, q{"}, $x);
      }
      if ($count % 2 == 1) {
          $more = $more == 1 ? 0 : 1;
      }
  
      $line .= $_;
  }
  if ($line) {
      ($oid, $value) = ($line =~ /([^\s]+)\s+(.*)/);
      $line = "";
      push @varbindings, [$oid, $value];
  }

  return (\%pdu_info, \@varbindings);
}

# traphandle handler
sub handle_trap {
  chomp(my @stdin = <STDIN>);
  logger("traphandle", "input text is ", \@stdin);
  my ($pdu_info, $varbindings) = get_from_stdin(\@stdin);
  logger("traphandle", "got pdu_info and varbindings ", [$pdu_info,$varbindings]);
  process($pdu_info, $varbindings);
}

# alert dispatcher
sub alert {
  my ($pdu_info, $uptime, $event) = @_;

  if ($opts{'alert'} eq 'email') {
    my $data = mail_data($alert_opts{'from'}, \@ARGV, $pdu_info, $uptime, $event);
    logger("alert", "mail data is", [\$data]);

    if ($alert_prog) {
      logger("alert", "mailer invoked with ", [$alert_prog,\@ARGV]);
      if (open MAILER, "|-", $alert_prog, @ARGV) {
        print MAILER $data;
        close MAILER;
      }
      else {
        logger("warn", "Unable to alert through mailer[$alert_prog @ARGV]: $!");
      }
    }
    else {
      logger("alert", "mail by Net::SNMP ", [$alert_opts{'server'},$alert_opts{'from'}, \@ARGV]);
      eval {
        my $message = Net::SMTP->new($alert_opts{'server'}) || die "ERROR: can't talk to server $alert_opts{'server'}\n";
        $message->mail($alert_opts{'from'});
        $message->to(@ARGV) || die "ERROR: failed to send to the recepients ",join(",",@ARGV),": $!";
        $message->data();
        $message->datasend($data);
        $message->dataend();
        $message->quit;
      };
      if ($@) {
        logger("warn", "alert mail failure ", $@);
      }
    }
  }
  elsif ($opts{'alert'} eq 'nagios') {
    my $command_file = shift @ARGV;
    logger("alert", "nagios external command file is $command_file");

    if (open NAGIOS, ">>", $command_file) {
      my $t = pettime($event);
      my ($code,$plugin_output) = nagios_check($event);

      # http://nagios.sourceforge.net/docs/3_0/extcommands.html
      my $cmd = "[$t] PROCESS_SERVICE_CHECK_RESULT;$pdu_info->{hostname};IPMI;$code;$plugin_output";
      logger("alert", "nagios command is", $cmd);

      print NAGIOS "$cmd\n";
      close NAGIOS;
    }
    else {
      logger("warn", "nagios failure with $command_file: $!");
    }
  }
  elsif ($opts{'alert'} eq 'nsca') {
    logger("alert", "nsca invoked with ", [$alert_prog, \@ARGV]);

    if (open NSCA, "|-", $alert_prog, @ARGV) {
      my ($code,$plugin_output) = nagios_check($event);

      # http://nagios.sourceforge.net/download/contrib/documentation/misc/NSCA_Setup.pdf
      my $cmd = "$pdu_info->{hostname}\tIPMI\t$code\t$plugin_output";
      logger("alert", "send_nsca command is ", $cmd);

      print NSCA "$cmd\n";
      close NSCA;
    }
    else {
      logger("warn", "nsca failure with $alert_prog @ARGV: $!");
    }
  }
  elsif ($opts{'alert'} eq 'noop') {
    logger('alert', 'noop alert selected');
  }
  else {
    logger("alert", "alert module");
    # TODO module
    die "alert module is not implemented!";
  }
}

# load sdr cache config into global mapping hash
sub load_sdrcache_config {
  my ($conf) = @_;

  my $cache_file = "";
  my $nl = 0;
  for (@{$conf}) {
    $nl++;
    chomp;
    s/#.*$//; # trim comment
    s/\s+$//; # trim trailing whitespaces
    next if /^$/;
    if (/^\S/) {
      if (-e $_) {
        $cache_file = $_;
      }
      else {
        return "ERROR: no such sdr cache file[$_] at line #$nl";
      }
    }
    if (/^\s/) {
      s/^\s+//; # trim leading whitespaces
      if ($cache_file) {
        for (split /\s+/) {
          $cache_mapping{$_} = $cache_file;
        }
      }
      else {
        return "ERROR: missing sdr cache file for host[$_] at line #$nl";
      }
    }
  }

  return;
}

# given an ipv4 address, resolve to related sdr cache
sub resolve_sdrcache {
  my ($ipmi_node) = @_;
  my $sdrcache = "";
  if (exists $cache_mapping{$ipmi_node}) {
     $sdrcache = $cache_mapping{$ipmi_node};
     logger("sdrcache", "$ipmi_node resolved to $sdrcache");
  }
  else {
     my $re = qr/^(.*)\[([-\d,]+)\](.*)$/;  # to match against eg. 10.2.3.[4-7]
     for my $k (keys %cache_mapping) {
        if (my ($prefix,$range,$suffix) = ($k =~ m/$re/)) {
           if (my ($item) = ($ipmi_node =~ /^\Q$prefix\E(.+)\Q$suffix\E$/)) {
              for (split /,+/, $range) {
                 my ($f,$t);
                 if (
                      ((($f,$t) = m/^(\d+)-(\d+)$/) && $f <= $item && $item <= $t)
                      || $item == $_
                    ) {
                    # got it
                    $sdrcache = $cache_mapping{$k};
                    logger("sdrcache", "$ipmi_node resolved to ", [$k => $sdrcache]);
                 }
              }
           }
        }
     }
  }

  $sdrcache || logger("sdrcache", "$ipmi_node will use default cache");

  return $sdrcache;
}

# process and verify args
sub process_args {
  # parse global ARGV for this package
  GetOptions(\%opts, 'help!', 'quiet!', 'mode|m=s', 'ack!', 'workaround|W=s',
    'trapoid|o=s', 'sdrcache|c=s', 'log|f=s', 'Debug|D=s', 'alert|n=s');

  if ($opts{'help'}) {
    usage();
  }

  # log file
  if ($opts{'log'}) {
    if (-w $opts{'log'}) {
      $log_filename = $opts{'log'};
    }
    else {
      die "log file $opts{'log'} is not writable";
    }
  }
  unless ($opts{'quiet'}) {
    print STDERR "petalert.pl is logging to $log_filename, use -q to suppress this tip\n";
  }

  # comma-separted debug tokens
  if ($opts{'Debug'}) {
    $logger_token{$_} = 1 for split /,+/, $opts{'Debug'};
  }
  # logging now ready
  logger("argv", "parsed options is ", \%opts);

  if ($opts{'sdrcache'}) {
    my $conf = $opts{'sdrcache'};
    logger("sdrcache", "config is [$conf]");

    open CONF, "<", $conf || logger("warn", "assumes default cache because failed to config file[$conf]: $!");
    chomp(my @lines = <CONF>);
    close CONF;

    load_sdrcache_config(\@lines);
    logger("sdrcache", "cache_mapping is ", \%cache_mapping);

    if ($opts{'syntax-only'}) {
      exit;
    }
  }

  if ($opts{'mode'} eq 'embperl') {
    unless (exists $opts{trapoid}) {
      $opts{trapoid} = "all";
      logger("argv", "no trapoid specified, defaults to all");
    }
    require NetSNMP::TrapReceiver;
  }
  elsif ($opts{'mode'} eq 'traphandle') {
  }
  else {
    print STDERR "Unknown operation mode: $opts{mode}\n";
    usage();
  }

  # alert method defaults to no-op
  unless (exists $opts{'alert'}) {
    $opts{'alert'} = 'noop';
    logger("argv", "no alert method specified, defaults to noop");
  }

  # alert methods
  if ($opts{'alert'} eq 'email') {
    # use external mail program
    if ($ARGV[0] && $ARGV[0] eq "--prog") {
      shift @ARGV;
      $alert_prog = shift @ARGV;
      unless (-x $alert_prog) {
        die "mailer[$alert_prog] is not executable\n";
      }
    }
    # or use perl module
    else {
      GetOptions(\%alert_opts, "--server=s", "--from=s");
      require Net::SMTP;
    }
  }
  elsif ($opts{'alert'} eq 'nagios') {
    unless ($ARGV[0] && -w $ARGV[0]) {
      die "nagios external command file[$ARGV[0]] is not writable\n";
    }
  }
  elsif ($opts{'alert'} eq 'nsca') {
    if ($ARGV[0] && $ARGV[0] eq "--prog") {
      shift @ARGV;
      $alert_prog = shift @ARGV;
      unless (-x $alert_prog) {
        die "nsca helper[$alert_prog] is not executable\n";
      }
    }
  }
  elsif ($opts{'alert'} eq 'noop') {
  }
  else {
    my $module = $opts{'alert'};
    if (-r $module) {
      require "$module";
      # TODO
      die "<not implemenented yet>";
    }
    else {
      die "Unknown alert module to load: $module\n";
    }
    # invokes handler
  }

  # @ARGV now holds alert specific arguments
}

sub main {
  @ARGV = @_;  # set global ARGV for this package
  process_args();
  if ($opts{'mode'} eq 'traphandle') {
    logger("main", "running as traphandle");
    handle_trap();
  }
  elsif ($opts{'mode'} eq 'embperl') {
    logger("main", "running as embperl");
    NetSNMP::TrapReceiver::register($opts{trapoid}, \&my_receiver) ||
           warn "failed to register our perl trap handler\n";
  }
  else {
    die "Should never reach here!\n";
  }

  return 0;
}


# run the program
if ( !caller ) { exit main(@ARGV); }

1;
__END__

__DATA__
pet.example.com
UDP: [10.2.3.4]:32768
.1.3.6.1.2.1.1.3.0 60:5:11:46.26
.1.3.6.1.6.3.1.1.4.1.0 .1.3.6.1.4.1.3183.1.1.0.356096
.1.3.6.1.4.1.3183.1.1.1 "44 45 4C 4C 50 00 10 59 80 43 B2 C0 4F 33 33 58 
00 42 19 EE AB 64 FF FF 20 20 00 41 73 18 00 80 
01 FF 00 00 00 00 00 19 00 00 02 A2 01 00 C1 "
.1.3.6.1.6.3.18.1.3.0 10.2.3.4
.1.3.6.1.6.3.18.1.4.0 "public"
.1.3.6.1.6.3.1.1.4.3.0 .1.3.6.1.4.1.3183.1.1
