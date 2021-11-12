package Stack;

use Fxtran;
use strict;


sub addStack
{
  my $d = shift;

  my @call = &F ('.//call-stmt[string(procedure-designator)!="ABOR1" and string(procedure-designator)!="REDUCE"]', $d);

  for my $call (@call)
    {
      my ($argspec) = &F ('./arg-spec', $call);
      $argspec->appendChild (&t (', '));
      $argspec->appendChild (&n ("<named-E><N><n>YLSTACK</n></N></named-E>"));
    }

  my ($dummy_arg_lt) = &F ('.//subroutine-stmt/dummy-arg-LT', $d);

  my ($last) = &F ('./arg-N[last()]', $dummy_arg_lt, 1);

  $dummy_arg_lt->appendChild (&t (', '));
  $dummy_arg_lt->appendChild (&n ("<arg-N><N><n>YDSTACK</n></N></arg-N>"));

  my ($use) = &F ('.//use-stmt[last()]', $d);
  $use->parentNode->insertAfter (&n ("<use-stmt>USE <module-N><N><n>STACK_MOD</n></N></module-N></use-stmt>"), $use);
  $use->parentNode->insertAfter (&t ("\n"), $use);


  my ($decl) = &F ('.//T-decl-stmt[.//EN-N[string(.)="?"]]', $last, $d);
  $decl->parentNode->insertAfter (&n (
'<T-decl-stmt><_T-spec_><derived-T-spec>TYPE(<T-N><N><n>STACK</n></N></T-N>)</derived-T-spec></_T-spec_> ' .
':: <EN-decl-LT><EN-decl><EN-N><N><n>YDSTACK</n></N></EN-N></EN-decl>' .
', <EN-decl><EN-N><N><n>YLSTACK</n></N></EN-N></EN-decl></EN-decl-LT></T-decl-stmt>'), $decl);
  $decl->parentNode->insertAfter (&t ("\n"), $decl);

  
  my ($exec) = grep { &Fxtran::stmt_is_executable ($_) } &F ('.//ANY-stmt', $d);

  $exec->parentNode->insertBefore (&t ("\n"), $exec);
  $exec->parentNode->insertBefore (&t ("\n"), $exec);
  $exec->parentNode->insertBefore (&n (
'<a-stmt><E-1><named-E><N><n>YLSTACK</n></N></named-E></E-1><a>=</a>' .
'<E-2><named-E><N><n>YDSTACK</n></N></named-E></E-2></a-stmt>'), $exec);
  $exec->parentNode->insertBefore (&t ("\n"), $exec);
  $exec->parentNode->insertBefore (&t ("\n"), $exec);


  

}

1;
