#!/home/gmap/mrpm/marguina/install/perl-5.32.0/bin/perl -w
#
use strict;
use FileHandle;
use Data::Dumper;

use FindBin qw ($Bin);
use lib $Bin;
use Fxtran;


sub pu_stmt
{
  my $pu = shift;

  my $xpath = './f:' . &Fxtran::xpath_by_type ('stmt') 
           . '|./f:' . &Fxtran::xpath_by_type ('construct') 
           . '|./f:' . &Fxtran::xpath_by_type ('block');

  my @stmt = &f ($xpath, $pu);

  while (1)
    {
      last unless (my @i = reverse grep { $stmt[$_]->nodeName =~ m/-(?:construct|block)$/o } (0 .. $#stmt));
      for my $i (@i)
        {
          splice (@stmt, $i, 1, &f ($xpath, $stmt[$i]));
        }
    }

  return @stmt;
}


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
        next if ($args{$name});

        my $stmt = &Fxtran::stmt ($en);

        my ($as) = &f ('.//f:array-spec', $stmt);
        my @ss = &f ('.//f:array-spec//f:shape-spec', $en, 1);
        next unless (@ss);
 
        next unless ($ss[0] =~ m/(%NPROMA$|^KPROMA$|^KPROMB$)/o);



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
    
    my @stmt = &pu_stmt ($pu);


    for my $i (0 .. $#stmt)
      {
        if (&Fxtran::stmt_is_executable ($stmt[$i]))
          {
            $stmt = $stmt[$i];
             last;
          }
      }

    ($cr) = &f ('(preceding::text ()[contains (., "' . "\n" . '")])[last ()]', $stmt);
   
    $cr->parentNode->insertBefore (&t ("\n"), $cr);
    $cr->parentNode->insertBefore (&t ("\n"), $cr);
    $cr->parentNode->insertBefore (&t ("init_stack ()\n"), $cr);
    $cr->parentNode->insertBefore (&t ("\n"), $cr);
    for (@name)
      {
        $cr->parentNode->insertBefore (&t ("alloc ($_)\n"), $cr);
      }
    $cr->parentNode->insertBefore (&t ("\n"), $cr);
    
    


  }


'FileHandle'->new (">$F90.new")->print ($doc->textContent);
