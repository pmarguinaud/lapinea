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
    my ($stmt) = $pu->firstChild;

    my ($darglt) = &f ('./f:dummy-arg-LT', $stmt);

    for (',', 'KSTPT', ',', 'KSTSZ', ',', 'PSTACK')
      {
        $darglt->appendChild (&t ($_));
      }


    my @darglt = map { $_->textContent } &f ('./f:arg-N', $darglt);

    ($stmt) = &f ('.//f:T-decl-stmt[.//f:EN-decl/f:EN-N/f:N/f:n/text ()="KLON"]', $pu);

    my $xpath = './/f:T-decl-stmt[' . join (' or ',
                  map { './/f:EN-decl/f:EN-N/f:N/f:n/text ()="' . $_ . '"' } @darglt) . ']';

    my ($last) = (&f ($xpath, $pu))[-1];
 

    ($last) = &f ('following::text ()[contains (., "' . "\n" . '")]', $last); # Goto end of line
    $last = $last->nextSibling;

    for my $v (qw (KSTSZ KSTPT))
      {
        my $st = $stmt->cloneNode (1);
        my ($n) = &f ('.//f:EN-decl/f:EN-N/f:N/f:n/text ()', $st);
        $n->replaceNode (&t ($v));
        $last->parentNode->insertBefore ($st, $last);
        $last->parentNode->insertBefore (&t ("\n"), $last);
      }

    {
      my $st = $stmt->cloneNode (1);
      my ($spec) = &f ('./f:_T-spec_/f:intrinsic-T-spec/f:T-N/text ()', $st);
      $spec->replaceNode (&t ('REAL   '));
      my ($intent) = &f ('./f:attribute/f:intent-spec/text ()', $st);
      $intent->replaceNode (&t ('INOUT'));
      my ($n) = &f ('.//f:EN-decl/f:EN-N/f:N/f:n/text ()', $st);
      $n->replaceNode (&t ("PSTACK (KSTSZ)"));
      $last->parentNode->insertBefore ($st, $last);
      $last->parentNode->insertBefore (&t ("\n"), $last);
      my ($dd) = &f ('./text ()[contains (., "::")]', $st);
      my $tt = $dd->data;
      for (1 .. 3)
        {
          $tt =~ s/^\s//o;
        }
      $dd->setData ($tt);
    }

    

    my @call = &f ('.//f:call-stmt', $pu);
    
    for my $call (@call)
      {
        my ($argspec) = &f ('./f:arg-spec', $call);
        next unless ($argspec);

        for (',', 'KSTPT', ',', 'KSTSZ', ',', 'PSTACK')
          {
            $argspec->appendChild (&t ($_));
          }
      }

  }


'FileHandle'->new (">$F90")->print ($doc->textContent);
