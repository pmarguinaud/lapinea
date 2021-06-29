#!/home/gmap/mrpm/marguina/install/perl-5.32.0/bin/perl -w
#
use strict;
use FileHandle;
use Data::Dumper;

use FindBin qw ($Bin);
use lib $Bin;
use Fxtran;

my $F90 = shift;

my $doc = &Fxtran::fxtran (location => $F90);

my @expr = &f ('.//f:named-E[./f:R-LT/f:parens-R/f:array-R]', $doc);

for my $expr (@expr)
  {
    my ($rlt) = &f ('./f:R-LT', $expr);
    my @as = &f ('./f:R-LT/f:parens-R/f:array-R/f:section-subscript-LT/f:section-subscript', $expr, 2);
    next if (grep { $_ ne ':' } @as);
    $rlt->unbindNode ();
  }

'FileHandle'->new (">$F90")->print ($doc->textContent);
