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

    my ($cr) = &f ('following::text ()[contains (., "' . "\n" . '")]', $stmt);

    $cr->parentNode->insertAfter (&t ("\n#include \"temp.h\"\n\n"), $cr);

    my @args = &f ('./f:dummy-arg-LT/f:arg-N', $stmt, 1);
    my %args = map { ($_, 1) } @args;

    my @name;

    my @en = &f ('.//f:EN-decl', $pu);

    for my $en (@en)
      {

        my ($name) = &f ('.//f:EN-N/f:N/f:n/text ()', $en, 1);
        my $stmt = &Fxtran::stmt ($en);

        my ($as) = &f ('.//f:array-spec', $stmt);
        my @ss = &f ('.//f:array-spec//f:shape-spec//f:upper-bound/*', $stmt);
        next unless (@ss);
        next if ($args{$name});

        push @name, $name;

        my ($spec) = &f ('./f:_T-spec_/f:intrinsic-T-spec', $stmt);
        
        my ($cr) = &f ('following::text ()[contains (., "' . "\n" . '")]', $stmt);
        $cr->parentNode->insertAfter (&t ("temp (" . $spec->textContent . ", $name, " . $as->textContent . ")\n"), $cr);

        &Fxtran::remove_element_from_list ($en);

        my @en = &f ('.//f:EN-decl', $stmt);

        unless (@en)
          {
            $stmt->unbindNode ();
          }

      }
    
    my @stmt = &f ('./f:' . &Fxtran::xpath_by_type ('stmt'), $pu);

    for my $i (0 .. $#stmt)
      {
        if (&Fxtran::stmt_is_executable ($stmt[$i+1]))
          {
            $stmt = $stmt[$i];
             last;
          }
      }
    
    ($cr) = &f ('following::text ()[contains (., "' . "\n" . '")]', $stmt);
   
    $cr->parentNode->insertAfter (&t ("\n"), $cr);
    for (@name)
      {
        $cr->parentNode->insertAfter (&t ("alloc ($_)\n"), $cr);
      }
    $cr->parentNode->insertAfter (&t ("\n"), $cr);
    


  }


'FileHandle'->new (">$F90")->print ($doc->textContent);
