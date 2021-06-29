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

    my %dims;
    
    my @en = &f ('.//f:EN-decl', $pu);
    
    for my $en (@en)
      {
        my ($name) = &f ('.//f:EN-N/f:N/f:n/text ()', $en, 1);
        my $stmt = &Fxtran::stmt ($en);
        my @ss = &f ('.//f:array-spec//f:shape-spec//f:upper-bound/*', $stmt);
        next unless (@ss);
        next unless ($ss[0]->textContent eq 'KLON');
        $dims{$name} = [map { $_->textContent } @ss];
      }
    
    
    for my $name (sort keys (%dims))
      {
        my @expr = &f ('.//f:named-E[./f:N/f:n/text ()="?"]', $name, $pu);
    
    #next unless ($name eq 'ZDELTVPT');
      
        for my $expr (@expr)
          {
            my $stmt = &Fxtran::stmt ($expr);
            next if ($stmt->nodeName eq 'call-stmt');
    
            my @ss = &f ('./f:R-LT/f:parens-R/f:element-LT/f:element/*', $expr);
            unless (@ss)
              {
                @ss = &f ('./f:R-LT/f:parens-R/f:array-R/f:section-subscript-LT/f:section-subscript', $expr);
              }
    
            my @d = @{ $dims{$name} };
    
            if (@ss)
               {
                 if ($ss[0] =~ m/:/o)
                   {
                     $ss[0]->replaceNode (&t ('KIDIA:KFDIA'));
                   }
                 else
                  {
                    my ($do) = (&f ('ancestor::f:do-construct/f:do-stmt[./f:do-V/f:named-E/f:N/f:n/text () = "?"]', $ss[0]->textContent, $expr))[-1];
                    my ($lb) = &f ('./f:lower-bound/*', $do);
                    my ($ub) = &f ('./f:upper-bound/*', $do);
                    $lb->firstChild->replaceNode (&t ('KIDIA'));
                    $ub->firstChild->replaceNode (&t ('KFDIA'));
    
                  }
               }
            else
               {
                 @ss = ('KIDIA:KFDIA', (':') x (scalar (@d) - 1));
                 $expr->appendChild (&t ('(' . join (',', @ss) . ')'));
               }
    
          }
      }
  }

'FileHandle'->new (">$F90.new")->print ($doc->textContent);
