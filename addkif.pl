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

for my $pu (@pu)
  {
    my @call = &f ('.//f:call-stmt', $pu);
    
    for my $call (@call)
      {
        my ($KLON) = &f ('.//f:arg[./f:named-E/f:N/f:n/text ()="KLON"]', $call);
        next unless ($KLON);
        $KLON->parentNode->insertAfter (&t (',KIDIA,KFDIA'), $KLON);
      }
    
    
    {
      my $stmt = $pu->firstChild;
      my ($KLON) = &f ('./f:dummy-arg-LT/f:arg-N[./f:N/f:n/text ()="KLON"]', $stmt);
      $KLON->parentNode->insertAfter (&t (',KIDIA,KFDIA'), $KLON);
    }
    
    
    my ($stmt) = &f ('.//' . &Fxtran::xpath_by_type ('stmt') . '[.//f:EN-decl[./f:EN-N/f:N/f:n/text ()="KLON"]]', $pu);
    
    for my $v (qw (KFDIA KIDIA))
      {
        my $st = $stmt->cloneNode (1);
        my ($n) = &f ('.//f:EN-decl/f:EN-N/f:N/f:n[text ()="KLON"]', $st);
        $n->replaceNode (&t ($v));
        $stmt->parentNode->insertAfter ($st, $stmt);
        $stmt->parentNode->insertAfter (&t ("\n"), $stmt);
      }
  }

'FileHandle'->new (">$F90")->print ($doc->textContent);
