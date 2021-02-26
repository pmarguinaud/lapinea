#!/usr/bin/perl -w

use strict;
use FileHandle;

my $F90 = shift;

my @code = do { my $fh = 'FileHandle'->new ("<$F90"); <$fh> };

for my $code (@code)
  {
    if ($code =~ m/READ \(KLUN\)\s+(\w+)\s$/o)
      {
        my $v = $1;
        if (! ($v =~ m/^(?:IL1|IU1)$/o))
          {
            $code = "$code!\$acc update device ($v)\n";
          }
      }
  }

'FileHandle'->new (">$F90")->print (join ('', @code));
