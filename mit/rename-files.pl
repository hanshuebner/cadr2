#!/usr/bin/perl -w

use strict;

use File::Find;

my %seen;

find({ no_chdir => 1,
       wanted => sub {
           if (/(.*)\.\d+$/) {
               my $name = $1;
               if (!$seen{$name}) {
                   $seen{$name} = 1;
                   open(F, $_) or die "$0: can't open $File::Find::name: $!\n";
                   my $line = scalar <F>;
                   close(F);
                   if ($line && $line =~ /-\*-.*\s*(|Mode:)\s*(SYMBOLS|MIDAS|LISP|TEXT)\s*(-\*-|;)/i) {
                       my $type = lc($2);
                       my ($first, @versions) = map { s/.*\.//; $_ } <$name.*>;
                       print "svn mv $name.$first $name.$type\n";
                       print "svn commit -m 'rename $name.$first to $name.$type'\n";
                       foreach my $version (@versions) {
                           if ($version =~ /^\d+$/) {
                               print "cp $name.$version $name.$type\n";
                               print "svn rm $name.$version\n";
                               print "svn commit -m 'bump $name.$type to version $version'\n";
                           } elsif ($version =~ /fasl$/) {
                               print "svn rm $name.$version\n";
                               print "svn commit -m 'remove fasl file $name.$version'\n";
                           }
                       }
                   }
               }
           }
       }
   }, ".");
