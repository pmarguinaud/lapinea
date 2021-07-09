package SingleBlock;
#
use strict;
use FileHandle;
use Data::Dumper;
use List::Util qw (uniqstr);

use Fxtran;


sub hoistJlonLoops
{
  my $doc = shift;
  
  my @pu = &f ('./f:object/f:file/f:program-unit', $doc);
  
  my ($JLON, $KIDIA, $KFDIA) = ('JROF', 'KST', 'KPROF');

  for my $pu (@pu)
    {
      # Find DO loops with JLON variable
  
      my @do = &f ('.//f:do-construct[./f:do-stmt/f:do-V/f:named-E/f:N/f:n/text ()="?"]', $JLON, $pu);

      my %lh;
  
      # For each JLON DO loop, find the outermost loop enclosing this JLON DO loop when possible
  
      for my $do (@do)
        {
          my @doo = &f ('ancestor-or-self::f:do-construct[./f:do-stmt/f:do-V/f:named-E/f:N/f:n/text ()!="?"]', $JLON, $do);


          my $doo;

          for my $dooo (@doo)
            {
              my ($var) = &f ('./f:do-stmt/f:do-V', $dooo, 1);
              $doo = $dooo if ($var && ($var ne $JLON) && ($var ne 'JITER'));
              last if ($var && ($var ne 'JITER'));
            }


          if ($doo)
            {
              my ($write) = &f ('.//f:write-stmt', $doo);
              $lh{$doo->unique_key} = $doo unless ($write);
            }
          
        }

      my @lh = values (%lh);

      for my $lh (@lh)
        {
          # Find current level of indentation
  
          my $sp = $lh->previousSibling;
          $sp = $sp->textContent;
          $sp =~ s/^\s*\n//o;
  
          # Create a JLON loop nest
  
          my @dob = &n (<< "EOF");
<do-construct><do-stmt>DO <do-V><named-E><N><n>$JLON</n></N></named-E></do-V> = <lower-bound><named-E><N><n>$KIDIA</n></N></named-E></lower-bound>, <upper-bound><named-E><N><n>$KFDIA</n></N></named-E></upper-bound></do-stmt>
$sp<C/>
$sp<end-do-stmt>ENDDO</end-do-stmt></do-construct>
EOF
  
          # Inject the nest before the outermost loop
  
          for my $dob (@dob)
            {
              $lh->parentNode->insertBefore ($dob, $lh);
            }
  
          # Re-nest outermost loop
          my ($dob)= &f ('.//f:C', $dob[0]);
  
          $dob->replaceNode ($lh);
  
          # Remove innermost JLON DO loops
  
          my @do = &f ('descendant-or-self::f:do-construct[./f:do-stmt/f:do-V/f:named-E/f:N/f:n/text ()="?"]', $JLON, $lh);
  
          for my $do  (@do)
            {
              $do->firstChild->unbindNode ();
              $do->lastChild->unbindNode ();
              # Use a pseudo target to remove the loop construct
              my $C = &n ('<C/>');
              $do->replaceNode ($C);
              for my $c ($do->childNodes ())
                {
                  $C->parentNode->insertBefore ($c, $C);
                }
              $C->unbindNode ();
            } 
        }

    }
  
}

sub addParallelLoopDirectives
{
  my $d = shift;

  my @pu = &f ('./f:object/f:file/f:program-unit', $d);

  my $JLON = 'JROF';
  
  # Insert OpenACC parallel directives
  
  for my $pu (@pu)
    {
      my @do = &f ('.//f:do-construct[./f:do-stmt/f:do-V/f:named-E/f:N/f:n/text()="?"]', $JLON, $d);
  
      for my $do (@do)
        {
          my %p;


          if (&f ('.//f:write-stmt', $do))
            {
              # Loops with a WRITE statement should run on the host
     
              my @v = (&f ('.//f:named-E[not (./f:R-LT/f:component-R)]'
                        . '[./f:R-LT/f:parens-R/f:element-LT/f:element[string (.)="?"]]/f:N/f:n/text ()', 
                           $JLON, $do, 1),
                       &f ('.//f:named-E[not (./f:R-LT/f:component-R)]'
                        .  '[./f:R-LT/f:array-R/f:section-subscript-LT/f:section-subscript[string (.)="?"]]/f:N/f:n/text ()', 
                           $JLON, $do, 1));
              @v = &uniqstr (@v);

              my $sp = &Fxtran::getIndent ($do);
              for my $v (@v)
                {
                  $do->parentNode->insertBefore (&n ("<C>!\$acc update host ($v)</C>"), $do);
                  $do->parentNode->insertBefore (&t ("\n" . (' ' x $sp)), $do);
                }
            }
          else
            {
              # Loop variables & temporary scalars; these are meant to be private
              my @s = &f ('.//f:E-1/f:named-E[not (./f:R-LT)]/f:N/f:n/text ()', $do);
              my @v = &f ('descendant-or-self::f:do-construct/f:do-stmt/f:do-V/f:named-E/f:N/f:n/text()', $do);
            
              for (@s, @v)
                {
                  $p{$_->textContent}++;
                }
  
              my @p = sort keys (%p);
  
              my $sp = $do->previousSibling;
              ($sp = $sp->textContent) =~ s/^\s*\n//o;
              $do->parentNode->insertBefore (&n ('<C>!$acc parallel loop gang vector '
#                                              . (@p ? 'private (' . join (', ', @p) . ') ' : '')
#                                              . 'default (none)'
                                               . '</C>'), $do);
              $do->parentNode->insertBefore (&t ("\n"), $do);
              $do->parentNode->insertBefore (&t ($sp), $do);
            }

        }
  
    }

}

1;
