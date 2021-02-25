#!/usr/bin/perl -w
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

print $pu->toString, "\n";

my $stmt = $pu->firstChild;

my ($cr) = &f ('following::text ()[contains (., "' . "\n" . '")]', $stmt);
$cr->parentNode->insertAfter (&t ("\n#include \"create.h\"\n\n"), $cr);

my %dims;

my @en = &f ('.//f:EN-decl', $pu);

for my $en (@en)
  {
    my ($name) = &f ('.//f:EN-N/f:N/f:n/text ()', $en, 1);
    my $stmt = &Fxtran::stmt ($en);

    my ($cr) = &f ('following::text ()[contains (., "' . "\n" . '")]', $stmt);
    $cr->parentNode->insertAfter (&t ("create ($name)\n"), $cr);
    
  }

'FileHandle'->new (">$F90")->print ($doc->textContent);
