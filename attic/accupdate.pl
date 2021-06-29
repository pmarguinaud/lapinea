#!/usr/bin/perl -w

use strict;
use FileHandle;

my $F90 = shift;

my @code = do { my $fh = 'FileHandle'->new ("<$F90"); <$fh> };

my %L;

for my $code (@code)
  {
    if ($code =~ m/LOGICAL :: (\S.*\S)/o)
      {
        my @L = split (m/\s*,\s*/o, $1);
        for (@L)
          {
            $L{$_} = 1;
          }
      }
    if ($code =~ m/READ \(KLUN\)\s+(\w+)\s$/o)
      {
        my $v = $1;
        if ((! ($v =~ m/^(?:IL1|IU1|IL2|IU2)$/o)) && ! $L{$v})
          {
            $code = "$code!\$acc update device ($v)\n";
          }
      }
  }

'FileHandle'->new (">$F90")->print (join ('', @code));
