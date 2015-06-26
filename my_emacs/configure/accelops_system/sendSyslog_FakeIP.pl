#!/usr/bin/perl -w
#
###############################################################################
#
# File:  sendSyslog_FakeIP.pl 
#
# Purpose: To replay syslog event back to prospect workers by spoofing the source IP as the router/pix IP
#
# Author: WenYong Yang
#

require Net::RawIP;
use strict;

my $spoof_addr = $ARGV[0] || '';
my $dst_addr   = $ARGV[1] || '';
my $file = $ARGV[2] || '';
my $loop = $ARGV[3] || '';

die "$0 <spoof IP> <dst IP> <File with syslog message> <number of loops>"
    unless $spoof_addr and $dst_addr and $file and $loop;

my $event_sent = 0;
#open FILE, "< $file" or die "[*] Could not open $file: $!";

for(my $count = 1; $count <= $loop; $count++)
{
open FILE, "< $file" or die "[*] Could not open $file: $!";
  while (my $line = <FILE>) { 
    my $text = $line;
    if ($text =~m/^#/)
    {
	$text = '';
    }

    if ($text !~ m/^\s*$/) 
    {
	my $content = $text;
	my $hex_mode = 0;
	my $proto = '';
	my $spt = 10000;
	my $dpt = 514;

	### make sure it is an inbound sig
        $proto = 'udp';

        my $rawpkt = '';
        if ($proto eq 'tcp') {
            $rawpkt = new Net::RawIP({'ip' => {
                saddr => $spoof_addr, daddr => $dst_addr},
				      'tcp' => { source => $spt, dest => $dpt, 'ack' => 1,
						 data => $content}})
		or die "[*] Could not get Net::RawIP object: $!";
        } else {
            $rawpkt = new Net::RawIP({'ip' => {
                saddr => $spoof_addr, daddr => $dst_addr},
				      'udp' => { source => $spt, dest => $dpt,
						 data => $content}})
		or die "[*] Could not get Net::RawIP object: $!";
        }
        $rawpkt->send();
        $event_sent++;
    }
  }
close (FILE);
}

#close (FILE);
print "$event_sent events sent.\n";
exit 0;
