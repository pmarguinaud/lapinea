#!/usr/bin/perl -w
#
#
use strict;
use FileHandle;
use Data::Dumper;
use FindBin qw ($Bin);
use lib "$Bin/../scripts";
use Fxtran;

use Carp;
$SIG{__DIE__} = \&Carp::confess;


sub replaceDummyArgumentByActual
{
  my ($e, $a) = @_;

  if ($a->nodeName eq 'named-E')
    {
      return &replaceDummyArgumentByActualNamedE ($e, $a);
    }
  elsif ($a->nodeName eq 'op-E')
    {
      return &replaceDummyArgumentByActualOpE ($e, $a);
    }
  else
    {
      die $a->toString;
    }

}

sub replaceDummyArgumentByActualOpE
{
  my ($e, $a) = @_;

  $a = &n ('<parens-E>(' . $a->textContent . ')</parens-E>');

  my @re = &f ('./f:R-LT/' . &Fxtran::xpath_by_type ('R'), $e);

  die $a->toString if (@re);

  $e->replaceNode ($a);

}

sub replaceDummyArgumentByActualNamedE
{
  my ($e, $a) = @_;

  my ($ne) = &f ('./f:N/f:n/text ()', $e);
  my ($na) = &f ('./f:N/f:n/text ()', $a);

  my @re = &f ('./f:R-LT/' . &Fxtran::xpath_by_type ('R'), $e);
  my @ra = &f ('./f:R-LT/' . &Fxtran::xpath_by_type ('R'), $a);

  my $se = $e->toString; my $te = $e->textContent;
  my $sa = $a->toString; my $ta = $a->textContent;

  if ((scalar (@ra) != 0) && (scalar (@re) == 0))
    {
      $e->replaceNode ($a->cloneNode (1));
      goto RETURN;
    }

  if (scalar (@ra))
    {
      unless (grep { $_->nodeName ne 'component-R' } @ra)
        {
          $a = $a->cloneNode (1);
          my ($ral) = &f ('./f:R-LT', $a);
          for my $re (@re)
            {
              $ral->appendChild ($re->cloneNode (1));
            }
          $e->replaceNode ($a);
          goto RETURN;
        }
    }

  # Use actual argument name

  $ne->replaceNode (&t ($na->textContent));

  if (scalar (@ra) == 0)
    {
      goto RETURN;
    }

  if ((scalar (@ra) == 0) && (scalar (@re) == 1))
    {
      goto RETURN;
    }
  
  # Resolve array references

  if ((scalar (@ra) == 1) && (scalar (@re) == 1))
    {
      my ($re, $ra) = (@re, @ra);
  
      $ra = $ra->cloneNode (1);

      goto DIE unless ($re->nodeName =~ m/(?:parens|array)-R$/o);
      goto DIE unless ($ra->nodeName =~ m/(?:parens|array)-R$/o);

      my @ele = &f ('./f:element-LT/f:element/' . &Fxtran::xpath_by_type ('E'), $re);
      my @ssa = &f ('./f:section-subscript-LT/f:section-subscript', $ra);

      for (my ($ia, $ie) = (0, 0); $ia < @ssa; $ia++)
        {
          if ($ssa[$ia]->textContent eq ':')
            {
              $ssa[$ia]->replaceNode ($ele[$ie]->cloneNode (1));
              $ie++;
            }
        }
        
      $re->replaceNode ($ra);
      goto RETURN;
    }

DIE:

  print " e = $te\n";
  print " a = $ta\n";
  print &Dumper ([map { $_->toString } @re]);
  print &Dumper ([map { $_->toString } @ra]);

  die "$se\n$sa\n";

RETURN:

  print " E = $te\n A = $ta\n     " . $e->textContent . "\n\n";

}

sub removeStmt
{
  my $stmt = shift;
  # Remove everything until eol
 
  my @n;
  for (my $n = $stmt; $n; $n = $n->nextSibling)
    {
      push @n, $n;
      if ($n->nodeName eq '#text')
        {
          last if ($n->data =~ m/\n/o);
        }
    }
  
  # Remove everything from start of line

  for (my $n = $stmt->previousSibling; $n; $n = $n->previousSibling)
    {
      if ($n->nodeName eq '#text')
        {
          if ($n->data =~ m/\n/o)
            {
              my $t = $n->data;
              $t =~ s/\n\s*$/\n/o;
              $n->setData ($t);
              last;
            }
        }
      else
        {
          last;
        }
      push @n, $n;
    }
  
  for (@n)
    {
      $_->unbindNode ();
    }
}

sub getIndent
{
  # get statement indentation

  my $stmt = shift;

  my $indent = 0;

  my $n = $stmt->previousSibling;

  unless ($n)
    {
      if ($stmt->parentNode)
        {
          return &getIndent ($stmt->parentNode);
        }
      $indent = 0;
      goto RETURN;
    }


  if (($n->nodeName eq '#text') && ($n->data =~ m/\n/o))
    {
      (my $t = $n->data) =~ s/^.*\n//smo;
      $indent = length ($t);
      goto RETURN;
    }

  $indent = 0;

RETURN:

  return $indent;
}

sub reIndent
{
  my ($node, $ns) = @_;

  my $sp = ' ' x $ns;

  my @cr = &f ('.//text ()[contains (.,"' . "\n" . '")]', $node);

  for my $cr (@cr)
    {
      (my $t = $cr->data) =~ s/\n/\n$sp/g;
      $cr->setData ($t);
    }

}

sub inlineSubroutine
{
  my ($d1, $d2) = @_;


  # Subroutine to be inlined
  my ($D2) = &f ('.//f:program-unit', $d2);
  my ($S2) = &f ('.//f:subroutine-stmt', $D2);

  my ($n2) = &f ('./f:subroutine-N/f:N/f:n/text ()', $S2);

  # Subroutine calls to be replaced by subroutine contents
  my @call = &f ('.//f:call-stmt[./f:procedure-designator/f:named-E/f:N/f:n/text ()="?"]', $n2, $d1);
  
  for my $call (@call)
    {
      my $d2 = $D2->cloneNode (1);
      my $s2 = $S2->cloneNode (1);
      my @da = &f ('./f:dummy-arg-LT/f:arg-N/f:N/f:n/text ()', $s2, 1);
  

      # Dummy arguments to actual arguments
      my %da2aa;
  
      {
        my @aa = &f ('.//f:arg-spec/f:arg/*', $call);
        die $call->toString unless (@aa == @da);
        for my $aa (@aa)
          {
            # Check we have a simple named expression without any reference 
            if (($aa->nodeName ne 'named-E') && (&f ('.//f:R-LT//parens-R', $aa)))
              {
                die $aa->toString;
              }
          }
        for my $i (0 .. $#aa)
          {
            $da2aa{$da[$i]} = $aa[$i];
          }
      }
  
      # Remove dummy arguments declaration
      
      for my $da (@da)
        {
          my @en_decl = &f ('.//f:EN-decl/f:EN-N/f:N/f:n[text ()="?"]', $da, $d2);
          for my $en_decl (@en_decl)
            {
              my ($stmt) = &Fxtran::stmt ($en_decl);
              &removeStmt ($stmt);
            }
        }
  
      # Replace dummy arguments by actual arguments
      for my $da (@da)
        {
          my @e = &f ('.//f:named-E[./f:N/f:n/text ()="?"]', $da, $d2);
      
          for my $e (@e)
            {
              &replaceDummyArgumentByActual ($e, $da2aa{$da});
            }
        }
      
      # Remove possible IMPLICIT NONE statement
      
      for (&f ('.//f:implicit-none-stmt', $d2))
        {
          $_->unbindNode (); # Should move includes to d1
        }
  
      my @node = &f ('descendant-or-self::f:program-unit/node ()', $d2);
  
      # Drop subroutine && end subroutine statements

      shift (@node);
      pop (@node);
  
      # Get indentation level of CALL statement

      my $ci = &getIndent ($call);
      my $sp = ' ' x $ci;

      my @B = &n (<< "EOF");
<block-construct><block-statement>BLOCK</block-statement>
$sp<C/>
$sp<end-block-stmt>ENDBLOCK</end-block-stmt></block-construct>
EOF


      for my $B (@B)
        {
          $call->parentNode->insertBefore ($B, $call);
        }
  
      my ($C) = &f ('./f:C', $B[0]);

      # Insert statements from inlined routine + a few comments
  
      $C->parentNode->insertAfter (&t ("\n"), $C);
      $C->parentNode->insertAfter (&n ("<C>!----- END INLINE $n2</C>"), $C);
      $C->parentNode->insertAfter (&t ("\n"  . (' ' x $ci)), $C);
  
      for my $node (reverse @node)
        {
          my $si = &getIndent ($node);
          my $di = $ci - $si; $di = $di > 0 ? $di : 0;
          &reIndent ($node, $di);
          $C->parentNode->insertAfter ($node, $C);
          $C->parentNode->insertAfter (&t (' ' x $di), $C);
        }
  
      # Comment old code (CALL)
      my @c = split (m/\n/o, $call->textContent ());
      for my $i (reverse (0 .. $#c))
        {
          my $c = $c[$i];
          $c = (' ' x ($ci)) . "! $c";
          $c = &t ($c);
          $c = $c->toString ();
          $C->parentNode->insertAfter (&t ("\n"), $C);
          $C->parentNode->insertAfter (&n ("<C>" . $c . "</C>"), $C);
        }
  
      $C->parentNode->insertAfter (&t ("\n"), $C);
      $C->parentNode->insertAfter (&t ("\n"), $C);
      $C->parentNode->insertAfter (&n ("<C>!----- BEGIN INLINE $n2</C>"), $C);
      $C->parentNode->insertAfter (&t ("\n"  . (' ' x $ci)), $C);
  
      $C->unbindNode ();

      # Remove CALL statement 

      $call->unbindNode ();
  
    }

}


my $f1 = "elascaw.F90";
my $f2 = "lascaw_vintw.F90";

my $d1 = &Fxtran::fxtran (location => $f1);
my $d2 = &Fxtran::fxtran (location => $f2);

&inlineSubroutine ($d1, $d2);

'FileHandle'->new (">$f1.new")->print ($d1->textContent);










