#!/usr/bin/perl -w

use strict;
use FileHandle;

my $F90 = shift;

my @code = do { my $fh = 'FileHandle'->new ("<$F90"); <$fh> };

for my $code (@code)
  {
    if ($code =~ m/READ/o)
      {
        chomp ($code);
        $code = "$code ; WRITE (0, *) __FILE__, ':', __LINE__, \"$code\"\n";
      }
  }


'FileHandle'->new (">$F90")->print (join ('', @code));
