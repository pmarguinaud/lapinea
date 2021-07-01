#!/usr/bin/perl -w
#
#
use strict;
use FileHandle;
use Data::Dumper;
use FindBin qw ($Bin);
use lib $Bin;
use Fxtran;

use Carp;
$SIG{__DIE__} = \&Carp::confess;

sub getDeclStmt
{
  my ($pu, $n) = @_;
  my ($stmt) = &f ('.//f:T-decl-stmt[.//f:EN-N/f:N/f:n/text ()="?"]', $n, $pu);
  return $stmt;
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

sub pass
{
  my ($d1, $d2, $a2) = @_;

  my ($s1) = &f ('.//f:program-unit/f:subroutine-stmt', $d1);
  my ($s2) = &f ('.//f:program-unit/f:subroutine-stmt', $d2);
  my ($n2) = &f ('.//f:subroutine-N/f:N/f:n/text ()', $s2, 1);
  
  
  my ($call) = &f ('.//f:call-stmt[./f:procedure-designator/f:named-E/f:N/f:n/text ()="?"]', $n2, $d1);
  
  my @da = &f ('./f:dummy-arg-LT/f:arg-N/f:N/f:n/text ()', $s2, 1); 
  
  # Dummy arguments to actual arguments
  my %da2aa;
  
  {   
    my @aa = &f ('.//f:arg-spec/f:arg/*', $call);
    die $call->toString unless (@aa == @da);
    for my $i (0 .. $#aa)
      {
        $da2aa{$da[$i]} = $aa[$i];
      }
  }   
  
  my $aa = $da2aa{$a2};
  my ($a1) = &f ('./f:N/f:n/text ()', $aa);
  
  
  my $decl1 = &getDeclStmt ($d1, $a1);
  my $decl2 = &getDeclStmt ($d2, $a2);
  
  my @ss1 = &f ('.//f:EN-decl[./f:EN-N/f:N/f:n/text ()="?"]//f:shape-spec', $a1, $decl1);
  my @ss2 = &f ('.//f:EN-decl[./f:EN-N/f:N/f:n/text ()="?"]//f:shape-spec', $a2, $decl2);
  
  my $nd = scalar (@ss1);
  
  my $AA1 = $aa->cloneNode (1);
  
  my $aa1 = &n ("<named-E><N><n>$a1</n></N></named-E>");
  $aa->replaceNode ($aa1);
  $aa = $aa1;
  
  {
    my $t = $aa;
  
    my @lb1 = map { &f ('./f:lower-bound/node ()', $_) || &n ("<literal-E><l>1</l></literal-E>") } @ss1;
  
    my @bb1 = map { $_->textContent eq ':' ? &n ("<literal-E><l>1</l></literal-E>") : &f ('./f:lower-bound/node ()', $_) } 
              &f ('./f:R-LT/f:array-R/f:section-subscript-LT/f:section-subscript', $AA1);
  
    for my $i (1 .. $nd)
      {
        for my $x (&t (', '),
                    &n ("<named-E><N><n>SIZE</n></N> <R-LT><parens-R>(<element-LT>"
                      . "<element><named-E><N><n>$a1</n></N></named-E></element>, "
                      . "<element><literal-E><l>$i</l></literal-E></element>"
                      . "</element-LT>)</parens-R></R-LT></named-E>"),
                    &t (', '),
                    &n ("<op-E>" . $bb1[$i-1]->textContent 
                      . "<op>-</op><literal-E><l>1</l></literal-E>"
                      . "</op-E>"),
                  )
          {
            $aa->parentNode->insertAfter ($x, $t);
            $t = $x;
          }
      }
  
  }
  
  my ($da) = &f ('./f:dummy-arg-LT/f:arg-N[./f:N/f:n/text ()="?"]', $a2, $s2);
  
  {
    my $t = $da;
    for my $i (1 .. $nd)
      {
        for my $x (&t (","), &n ("<arg><N><n>KDIM$i\_$a2</n></N></arg>"),
                   &t (","), &n ("<arg><N><n>KOFF$i\_$a2</n></N></arg>"),
                  )
          {
            $da->parentNode->insertAfter ($x, $t);
            $t = $x;
          }
      }
  }
  
  {
    my ($as2) = &f ('.//f:EN-decl[./f:EN-N/f:N/f:n/text ()="?"]/f:array-spec', $a2, $d2);
  
    $as2->replaceNode (&n ('<array-spec>(<shape-spec-LT>' 
      . join (',', map { "<shape-spec><upper-bound><named-E><N><n>KDIM$_\_$a2</n></N></named-E></upper-bound></shape-spec>" } (1 .. $nd)) 
      . '</shape-spec-LT>)</array-spec>'));
  
  }
  
  
  for my $e2 (&f ('.//f:named-E[./f:N/f:n/text ()="?"]', $a2, $d2))
    {
      my $stmt = &Fxtran::stmt ($e2);
      if ($stmt->nodeName eq 'call-stmt')
        {
          my @r2 = &f ('./f:R-LT/f:R', $e2);
  
          if (@r2)
            {
              die;
            }
          else
            {
              $e2->replaceNode (&n ("<named-E><N><n>$a2</n></N><R-LT><array-R>(<section-subscript-LT>"
                                  . join (',', map  { "<section-subscript><lower-bound><op-E><named-E><N><n>KOFF$_\_$a2</n></N>"
                                                    . "</named-E>"
                                                    . "<op>+</op><literal-E><l>1</l></literal-E></op-E></lower-bound>"
                                                    . ":</section-subscript>" } (1 .. $nd))
                                  . "</section-subscript-LT>)</array-R></R-LT></named-E>"));
            }
  
        }
      else
        {
          die;
        }
    }
  
  
  {
  
    my ($DECL) = &f ('.//f:T-decl-stmt[.//f:EN-decl/f:EN-N/f:N/f:n/text ()="?"]', 'KST', $d2);
    my $decl = $DECL->cloneNode (1);
    
    my ($lst) = &f ('.//f:EN-decl-LT', $decl);
  
    for ($lst->childNodes ())
      {
        $_->unbindNode ();
      }
  
    my $sp = &getIndent ($DECL);
  
    for my $i (1 .. $nd)
      {
        $lst->appendChild (&n ("<EN-decl><EN-N><N><n>KOFF$i\_$a2</n></N></EN-N></EN-decl>"));
        $lst->appendChild (&t (', '));
        $lst->appendChild (&n ("<EN-decl><EN-N><N><n>KDIM$i\_$a2</n></N></EN-N></EN-decl>"));
        $lst->appendChild (&t (', ')) if ($i != $nd);
      }
  
    $DECL->parentNode->insertBefore ($decl, $DECL);
    $DECL->parentNode->insertBefore (&n (' ' x $sp . "\n"), $DECL);
  
 }

}

my $f1 = "lapinea.F90";
my $f2 = "larcina.F90";

my $d1 = &Fxtran::fxtran (location => $f1);
my $d2 = &Fxtran::fxtran (location => $f2);
#my @a2 = qw (PKAPPA PKAPPAT PKAPPAM PKAPPAH);
my @a2 = qw (PKAPPA);


for my $a2 (@a2)
  {
    &pass ($d1, $d2, $a2);
  }


'FileHandle'->new (">$f1.new")->print ($d1->textContent);
'FileHandle'->new (">$f2.new")->print ($d2->textContent);










