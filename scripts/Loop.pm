package Loop;
#
use strict;
use FileHandle;
use Data::Dumper;

use Fxtran;

sub expandJlonLoops
{
  my $d = shift;
  
  
  my @dim = qw (KPROMA KPROMB YDGEOMETRY%YRDIM%NPROMA);
  my $var = 'JROF';
  my $vari = 'KST';
  my $varf = 'KPROF';
  
  
  my @nr;
  
  # Find variables with NPROMA dimension
  
  for my $dim (@dim)
    {
      my @en_decl = &f ('.//f:EN-decl[./f:array-spec/f:shape-spec-LT'
                      . '/f:shape-spec/f:upper-bound[string (.)="?"]]', $dim, $d);
      
      # For each variable, find the rank of NPROMA dimension
      
      
      for my $en_decl (@en_decl)
        {
          my ($n) = &f ('./f:EN-N/f:N/f:n/text ()', $en_decl, 1);
          my @ss = &f ('./f:array-spec/f:shape-spec-LT/f:shape-spec/f:upper-bound', $en_decl);
          my ($r) = grep { $ss[$_]->textContent eq $dim } (0 .. $#ss);
          push @nr, [$n, $r];
        }
    }
  
  
  
  for my $nr (@nr)
    {
      my ($n, $r) = @$nr;
  
      # Find assignment statements where variable is involved, with array section on NPROMA dimension
  
      my @a = &f ('.//f:a-stmt[.//f:named-E[./f:N/f:n/text ()="?"]'
               . '[./f:R-LT/f:array-R/f:section-subscript-LT/f:section-subscript[?]'
               . '[contains (string (.),":")]]]', $n, $r+1, $d);
  
  
      for my $a (@a)
        {
  
          my $sp = $a->previousSibling;
          $sp = $sp->textContent;
          $sp =~ s/^\s*\n//o;
    
          # For all expressions with NPROMA dimension, replace NPROMA slice by NPROMA loop variable
          for my $nr (@nr)
            {
              my ($n, $r) = @$nr;
              
              my @s = 
                  &f ('.//f:named-E[./f:N/f:n/text ()="?"]'
                   . '/f:R-LT/f:array-R/f:section-subscript-LT/f:section-subscript[?]'
                   . '[contains (string (.),":")]', $n, $r+1, $a);
  
              for my $s (@s)
                {
                  $s->replaceNode (&n ("<section-subscript><lower-bound><named-E><N><n>$var</n></N></named-E></lower-bound></section-subscript>"));
                }
  
            }
  
          my @dob = &n (<< "EOF");
<do-construct><do-stmt>DO <do-V><named-E><N><n>$var</n></N></named-E></do-V> = <lower-bound><named-E><N><n>$vari</n></N></named-E></lower-bound>, <upper-bound><named-E><N><n>$varf</n></N></named-E></upper-bound></do-stmt>
$sp$sp<C/>
$sp<end-do-stmt>ENDDO</end-do-stmt></do-construct>
EOF
    
  
          for (@dob)
            {
              $a->parentNode->insertBefore ($_, $a);
            }
  
          my ($C)= &f ('.//f:C', $dob[0]);
  
          $C->replaceNode ($a);
        }
      
  
    }

}

1;
