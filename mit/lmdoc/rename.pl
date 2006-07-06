#!/bin/perl -w

use strict;

foreach my $f (<*>) {
    my $o = $f;
    if ($f =~ s/\.\d+$/.text/) {
        print "svn mv $o $f\n";
    }
}
