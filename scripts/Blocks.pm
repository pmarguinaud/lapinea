package Blocks;
#
use strict;
use FileHandle;
use Data::Dumper;

use Fxtran;

sub getShapeSpecList
{
  my ($var, $doc, %opts) = @_; 

  my $cr = $opts{create};

  my @en_decl = &f ('.//f:EN-decl[./f:EN-N/f:N/f:n/text ()="' . $var . '"]', $doc);

  my $sslt;

  # Update dimensions : look for existing array spec

  for my $en_decl (@en_decl)
    {   
      ($sslt) = &f ('./f:array-spec/f:shape-spec-LT', $en_decl);
      if ($sslt)
        {
          last;
        }
    }   

  # No dimensions: add array spec

  if ((! $sslt) && ($cr))
    {   
      for my $en_decl (@en_decl)
        {
          my $as = $en_decl->appendChild (&n ("<array-spec/>"));
          $as->appendChild (&t ("("));
          $sslt = $as->appendChild (&n ("<shape-spec-LT/>"));
          $as->appendChild (&t (")"));
          last;
        }
    }   

  return $sslt;
}

sub getAttributes
{
  my ($var, $doc) = @_;
  my ($decl) = &f ('.//f:T-decl-stmt[.//f:EN-decl/f:EN-N/f:N/f:n/text ()="?"]', $var, $doc);
  $decl or die "var=$var\n";
  my @attr = &f ('./f:attribute', $decl);
  return @attr;
}

sub getTypeSpec
{
  my ($var, $doc) = @_;
  
  my ($ts) = &f ('.//f:T-decl-stmt[.//f:EN-decl/f:EN-N/f:N/f:n/text ()="' . $var . '"]/f:_T-spec_/node ()', $doc);

  return $ts;
}

sub addBlocks
{
  my $doc = shift;
  
  my @pu = &f ('./f:object/f:file/f:program-unit', $doc);
  
# my ($KGPBLKS_TARGET, $JLON_NAME, $KLON_NAME) = qw (KLON JLON KLON);
  my ($KGPBLKS_TARGET, $JLON_NAME, @KLON_NAME) = qw (KST JROF KPROMA KPROMB YDGEOMETRY%YRDIM%NPROMA);

  for my $pu (@pu)
    {
  
      # Add KGPBLKS argument
      {
        my ($target) = &f ('.//f:arg-N[./f:N/f:n/text ()="?"]', $KGPBLKS_TARGET, $pu);
        $target->parentNode->insertBefore (&n ('<arg-N><N><n>KGPBLKS</n></N></arg-N>'), $target);
        $target->parentNode->insertBefore (&t (','), $target);
      }
  
      # Declare KGPBLKS argument
      {
        my ($target) = &f ('.//f:T-decl-stmt//f:EN-decl[./f:EN-N/f:N/f:n/text ()="?"]', $KGPBLKS_TARGET, $pu);
        $target->parentNode->insertBefore (&n ('<EN-decl><EN-N><N><n>KGPBLKS</n></N></EN-N></EN-decl>'), $target);
        $target->parentNode->insertBefore (&t (','), $target);
      }
  
      # Insert KGPBLKS argument in CALL statements
      {
        my @target = &f ('.//f:arg[./f:named-E/f:N/f:n/text ()="?"]', $KGPBLKS_TARGET, $pu);
        for my $target (@target)
          {
            $target->parentNode->insertBefore (&n ('<arg><named-E><N><n>KGPBLKS</n></N></named-E></arg>'), $target);
            $target->parentNode->insertBefore (&t (','), $target);
          }
      }
  
      # Add JBLK loop variable
      {
        my ($jlon) = &f ('.//f:T-decl-stmt//f:EN-decl[./f:EN-N/f:N/f:n/text ()="?"]', $JLON_NAME, $pu);
        $jlon->parentNode->insertBefore (&n ('<EN-decl><EN-N><N><n>JBLK</n></N></EN-N></EN-decl>'), $jlon);
        $jlon->parentNode->insertBefore (&t (','), $jlon);
      }
  
  
      # Add JBLK dimension to arrays whose first dimension is KLON
      my @en_decl = map { my $var = $_; 
                       &f ('.//f:EN-decl[./f:array-spec/f:shape-spec-LT' 
                         . '[./f:shape-spec/f:upper-bound/f:named-E[string (.)="?"]]]', 
                           $var, $pu) } @KLON_NAME;
  
      for my $en_decl (@en_decl)
        {
          my ($sslt) = &f ('./f:array-spec/f:shape-spec-LT', $en_decl);
          my @ss = &f ('./f:shape-spec', $sslt);
          my @SS = map { $_->textContent } @ss;
#         my ($r) = grep { $SS[$_+1] =~ m/^(?:KDIMK|YDML_DYN%YYTLSCAW%NDIM)$/o } (0 .. $#SS-1);
#         $r = $#SS unless (defined ($r));
          my $r = $#SS;
          $sslt->insertAfter (&n ('<shape-spec><upper-bound><named-E><N><n>KGPBLKS</n></N></named-E></upper-bound></shape-spec>'), $ss[$r]);
          $sslt->insertAfter (&t (','), $ss[$r]);
        }
  
      # Find DO loops with JLON variable
  
      my @do = &f ('.//f:do-construct[./f:do-stmt/f:do-V/f:named-E/f:N/f:n/text ()="?"]', $JLON_NAME, $pu);

      my %lh;
  
      # For each JLON DO loop, find the outermost loop enclosing this JLON DO loop
  
      for my $do (@do)
        {
          my @doo = &f ('ancestor-or-self::f:do-construct', $do);
          my $doo;
          for my $dooo (@doo)
            {
              my ($var) = &f ('./f:do-stmt/f:do-V', $dooo, 1);
              $doo = $dooo;
              last if ($var && ($var ne 'JITER'));
            }
          $lh{$doo->unique_key} = $doo;
        }

      my @lh = values (%lh);
  
      # For each top level loop, enclose it in JBLK and JLON loops
  
      for my $lh (@lh)
        {
          # Find current level of indentation
  
          my $sp = $lh->previousSibling;
          $sp = $sp->textContent;
          $sp =~ s/^\s*\n//o;
  
          # Create a JBLK/JLON loop nest
  
          my @dob = &n (<< "EOF");
<do-construct><do-stmt>DO <do-V><named-E><N><n>JBLK</n></N></named-E></do-V> = <lower-bound><literal-E><l>1</l></literal-E></lower-bound>, <upper-bound><named-E><N><n>KGPBLKS</n></N></named-E></upper-bound></do-stmt>
$sp<do-construct><do-stmt>DO <do-V><named-E><N><n>$JLON_NAME</n></N></named-E></do-V> = <lower-bound><named-E><N><n>KIDIA</n></N></named-E></lower-bound>, <upper-bound><named-E><N><n>KFDIA</n></N></named-E></upper-bound></do-stmt>
$sp<C/>
$sp<end-do-stmt>ENDDO</end-do-stmt></do-construct>
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
  
          # Add JBLK index to variables
  

          for my $elt (&f ('.//f:element-LT[./f:element/f:named-E/f:N/f:n/text ()="?"]', $JLON_NAME, $lh))
            {
              $elt->appendChild (&t (','));
              $elt->appendChild (&n ('<element><named-E><N><n>JBLK</n></N></named-E></element>'));
            }
  
          # Is this one necessary ?
          for my $elt (&f ('.//f:section-subscript-LT[./f:section-subscript/f:named-E/f:N/f:n/text ()="?"]', $JLON_NAME, $lh))
            {
              $elt->appendChild (&t (','));
              $elt->appendChild (&n ('<section-subscript><named-E><N><n>JBLK</n></N></named-E></section-subscript>'));
            }

          for my $elt (&f ('.//f:section-subscript-LT[./f:section-subscript/f:lower-bound/f:named-E/f:N/f:n/text ()="?"]', $JLON_NAME, $lh))
            {
              $elt->appendChild (&t (','));
              $elt->appendChild (&n ('<section-subscript><lower-bound><named-E><N><n>JBLK</n></N></named-E></lower-bound></section-subscript>'));
            }
  
          # Remove innermost JLON DO loops
  
          my @do = &f ('descendant-or-self::f:do-construct[./f:do-stmt/f:do-V/f:named-E/f:N/f:n/text ()="?"]', $JLON_NAME, $lh);
  
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
  
  
  # Insert OpenACC parallel directives
  
  for my $pu (@pu)
    {
      my @do = &f ('.//f:do-construct[./f:do-stmt/f:do-V/f:named-E/f:N/f:n/text()="JBLK"]', $d);
  
      for my $do (@do)
        {
          my %p;
  
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
          $do->parentNode->insertBefore (&n ('<C>!$acc parallel loop gang vector collapse (2) '
                                           . (@p ? 'private (' . join (', ', @p) . ') ' : '')
                                           . 'default (none)'
                                           . '</C>'), $do);
          $do->parentNode->insertBefore (&t ("\n"), $do);
          $do->parentNode->insertBefore (&t ($sp), $do);
        }
  
  
    }

}

sub addKernelDirectives
{
  my $d = shift;
  
  my @pu = &f ('./f:object/f:file/f:program-unit', $d);
  
  for my $pu (@pu)
    {
      my @do = &f ('.//f:do-construct[./f:do-stmt/f:do-V/f:named-E/f:N/f:n/text()="JBLK"]', $d);
  
      for my $do (@do)
        {
          my %p;
  
          my $sp = $do->previousSibling;
          ($sp = $sp->textContent) =~ s/^\s*\n//o;
  
          $do->parentNode->insertBefore (&n ('<C>!$acc kernels default (present)</C>'), $do);
          $do->parentNode->insertBefore (&t ("\n$sp"), $do);
  
          $do->parentNode->insertAfter (&n ('<C>!$acc end kernels</C>'), $do);
          $do->parentNode->insertAfter (&t ("\n$sp"), $do);
  
        }
  
  
    }
}

sub addDataDirectives
{
  my $d = shift;
  
  my @pu = &f ('./f:object/f:file/f:program-unit', $d);
  
  for my $pu (@pu)
    {
  
      # Find list of dummy arguments
      my @arg = map { $_->textContent } &f ('.//f:dummy-arg-LT/f:arg-N/f:N/f:n/text ()', $pu);
      my %arg = map { ($_, 1) } @arg;
  
      # Keep those which are arrays or derived types in @aa
  
      my @aa;  # Arguments (derived types & arrays)
      my @ao;  # Optional arguments (derived types & arrays)

      for my $arg (@arg)
        {
          my $as = &getShapeSpecList ($arg, $pu);
          my $ts = &getTypeSpec ($arg, $pu);
          
          if ($as || $ts->nodeName eq 'derived-T-spec')
            {
              my @attr = map { $_->textContent } &getAttributes ($arg, $pu);

              if (grep { $_ eq 'OPTIONAL' } @attr)
                {
                  push @ao, $arg;
                }
              else
                {
                  push @aa, $arg;
                }
            }
        }
  
      @aa = sort @aa;
  
      # Local arrays 
  
      my @la = sort grep { ! $arg{$_} } 
               map { $_->textContent } &f ('.//f:T-decl-stmt//f:EN-decl[./f:array-spec]/f:EN-N/f:N/f:n/text ()', $pu);
      
      my @stmt = &f ('.//f:T-decl-stmt|.//f:include', $pu);
      my $stmt = $stmt[-1];
  
      my ($cr) = &f ('following::text ()[contains (., "' . "\n" . '")]', $stmt);
      (my $sp = $cr->textContent) =~ s/\n\s*//o;
  
  
      my $nacc = 0;
  
      # Create local arrays
  
      while (@la)
        {
          $cr->parentNode->insertAfter (&t ("\n"), $cr);
          $cr->parentNode->insertAfter (&n ('<C>!$acc data '
                                          . 'create (' . join (', ', splice (@la, 0, 10)) . ')'
                                          . '</C>'),  $cr);
          $cr->parentNode->insertAfter (&t ($sp), $cr);
          $nacc++;
        }
  
      # Declare arrays & derived type arguments as present
     
      while (@aa)
        {
          $cr->parentNode->insertAfter (&t ("\n"), $cr);
          $cr->parentNode->insertAfter (&n ('<C>!$acc data '
                                          . 'present (' . join (', ', splice (@aa, 0, 10)) . ')'
                                          . '</C>'),  $cr);
          $cr->parentNode->insertAfter (&t ($sp), $cr);
          $nacc++;
        }
  
      # Declare arrays & derived type arguments as present (optional arguments)
     
      for my $ao (@ao)
        {
          $cr->parentNode->insertAfter (&t ("\n"), $cr);
          $cr->parentNode->insertAfter (&n ("<C>!\$acc data present ($ao) if (PRESENT ($ao))</C>"), $cr);
          $cr->parentNode->insertAfter (&t ($sp), $cr);
          $nacc++;
        }
  
      # Add OpenACC end data directives
     
      while ($nacc)
        {
          $pu->insertBefore (&t ($sp), $pu->lastChild);
          $pu->insertBefore (&n ('<C>!$acc end data</C>'), $pu->lastChild);
          $pu->insertBefore (&t ("\n"), $pu->lastChild);
          $nacc--;
        }
      
  
    }

}

sub exchangeJlonJlevLoops
{
  my $d = shift;

  my @pu = &f ('./f:object/f:file/f:program-unit', $d);

  for my $pu (@pu)
    {
      my @do_jlon = &f ('.//f:do-construct[./f:do-stmt/f:do-V/f:named-E/f:N/f:n/text()="JLON"]' .
                        '[./f:do-construct/f:do-stmt/f:do-V/f:named-E/f:N/f:n/text ()="JLEV"]/f:do-stmt', $d);

      for my $do_jlon (@do_jlon)
        {
          my @do_jlev = &f ('../f:do-construct/f:do-stmt[./f:do-V/f:named-E/f:N/f:n/text()="JLEV"]', $do_jlon);
          my $do_jlev = $do_jlev[0]->cloneNode (1);
          for my $do_jlev (@do_jlev)
            {
              $do_jlev->replaceNode ($do_jlon->cloneNode (1));
            }
          $do_jlon->replaceNode ($do_jlev);
        }
    }
}


sub mergeKernels
{

  my $d = shift;
  
  my @pu = &f ('./f:object/f:file/f:program-unit', $d);
  
  for my $pu (@pu)
    {
  
      my @end = &f ('./f:C[text ()="!$acc end kernels"]', $pu);
  
      for my $end (@end)
        {
          my @n = &f ('following-sibling::node ()', $end);
          for my $n (@n)
            {
              if (($n->nodeName eq 'C') && ($n->textContent eq '!$acc kernels'))
                {
                  $end->unbindNode ();
                  $n->unbindNode ();
                  last;
                }
              last unless ($n->nodeName eq '#text');
            }
        }
  
  
  
    }
}

1;


