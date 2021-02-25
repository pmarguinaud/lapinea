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

my @pu = &f ('./f:object/f:file/f:program-unit', $doc);

my $pu = $pu[0];

my @args = &f ('.//f:dummy-arg-LT//f:arg-N/f:N/f:n/text ()', $pu->firstChild, 1);

my %dims;

for my $arg (@args)
  {
    my @en = &f ('.//f:EN-decl[./f:EN-N/f:N/f:n[text ()="' . $arg . '"]]', $pu);
   
    for my $en (@en)
      {
        my $stmt = &Fxtran::stmt ($en);
        my @ss = &f ('.//f:array-spec//f:shape-spec//f:upper-bound/*', $stmt);
        $dims{$arg} = [map { $_->textContent } @ss];
      }

  }

my @expr = &f ('.//f:named-E[./f:N/f:n/text ()="SIZE"]', $doc);

for my $expr (@expr)
  {
    my ($name, $rank) = &f ('./f:R-LT/f:parens-R/f:element-LT/f:element', $expr, 2);
    next unless ($rank && $dims{$name});
    $expr->replaceNode (&t ($dims{$name}[$rank-1]));
  }

'FileHandle'->new (">$F90")->print ($doc->textContent);


