package Implicit;
#
#
use strict;
use FileHandle;
use Data::Dumper;
use Fxtran;

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


sub makeImplicit
{
  my ($d, %opts) = @_;

  my @a;

  if ($opts{name})
    {
      @a = @{ $opts{name} };
    }
  elsif ($opts{rank}) 
    {
      my @args = &f ('.//f:subroutine-stmt//f:dummy-arg-LT/f:arg-N/f:N/f:n', $d, 1);
      @a = @args[@{ $opts{rank} }];
    }
  
  for my $a (@a)
    {
      my ($en_decl) = &f ('.//f:EN-decl[./f:EN-N/f:N/f:n/text ()="?"]', $a, $d);
      my @ss = &f ('.//f:shape-spec-LT/f:shape-spec', $en_decl);
  
      my $stmt = &Fxtran::stmt ($ss[0]);
  
      my $C = &n ("<C>! " . $en_decl->textContent . "</C>");
  
      my $sp = &getIndent ($stmt);
      $stmt->parentNode->insertBefore ($C, $stmt);
      $stmt->parentNode->insertBefore (&t ("\n" . (' ' x $sp)), $stmt);
  
      for my $ss (@ss)
        {
          $ss->replaceNode (&n ("<shape-spec>:</shape-spec>"));
        }
  
    }

}

1;










