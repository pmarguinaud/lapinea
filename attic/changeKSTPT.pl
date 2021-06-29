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

my @ISTPT = &f ('//f:call-stmt//f:arg/f:named-E/f:N/f:n[text ()="KSTPT"]', $doc);

for my $ISTPT (@ISTPT)
  {
    $ISTPT->replaceNode (&t ("ISTPT"));
  }

'FileHandle'->new (">$F90.new")->print ($doc->textContent);
