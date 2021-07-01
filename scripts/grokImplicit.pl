#!/usr/bin/perl -w
#
#
use strict;
use FileHandle;
use Data::Dumper;
use FindBin qw ($Bin);
use lib $Bin;
use Fxtran;
use Implicit;

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



my %grok;

for my $f (@ARGV)
  {
    my $d = &Fxtran::fxtran (location => $f);
    
    my @call = &f ('.//f:call-stmt', $d);
    
    for my $call (@call)
      {
        my ($name) = &f ('./f:procedure-designator/f:named-E/f:N/f:n/text ()', $call, 1);
        my @arg = &f ('./f:arg-spec/f:arg/node ()', $call);
    
        for my $i (0 .. $#arg)
          {
            my $arg = $arg[$i];
    
            my @ss = &f ('./f:R-LT/f:array-R/f:section-subscript-LT/f:section-subscript[./f:upper-bound|./f:lower-bound]', $arg);
    
            if (@ss)  
              {
                $grok{$name}{$i}++;
              }
          }
      }
  }

for (values (%grok))
  {
    $_ = [sort { $a <=> $b } keys (%$_)];
  }


while (my ($name, $rank) = each (%grok))
  {
    printf ("%-30s | %s\n", $name, join (' ', map { sprintf ('%2d', $_) } @$rank));

    my $f = lc ($name) . '.F90';

    my $d = &Fxtran::fxtran (location => $f);

    &Implicit::makeImplicit ($d, rank => $rank);

    'FileHandle'->new (">$f.new")->print ($d->textContent);
  }








