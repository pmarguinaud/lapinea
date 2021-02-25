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
my $pu = $pu[0];

my %dims;

my @en = &f ('.//f:EN-decl', $pu);

for my $en (@en)
  {
    my ($name) = &f ('.//f:EN-N/f:N/f:n/text ()', $en, 1);
    my $stmt = &Fxtran::stmt ($en);
    my @ss = &f ('.//f:array-spec//f:shape-spec//f:upper-bound/*', $stmt);
    $dims{$name} = [map { $_->textContent } @ss];
  }



my @call = &f ('.//f:call-stmt', $doc);

for my $call (@call)
  {
    my ($name) = &f ('.//f:procedure-designator/f:named-E/f:N/f:n/text ()', $call, 1);
    my @actual = &f ('./f:arg-spec/f:arg/*', $call);

    my $file = lc ($name) . '.F90';
    next unless (@actual && -f $file);


    my $doc = &Fxtran::fxtran (location => $file);

    my @pu = &f ('./f:object/f:file/f:program-unit', $doc);
    my $pu = $pu[0];
    my $stmt = $pu->firstChild;
    my @dummy = &f ('./f:dummy-arg-LT/f:arg-N/f:N/f:n/text ()', $stmt, 1);
    my @optional;

    
    for my $iarg (0 .. $#dummy)
      {
        my @en = &f ('.//f:EN-decl[./f:EN-N/f:N/f:n[text ()="' . $dummy[$iarg] . '"]]', $pu);
        for my $en (@en)
          {
            my $stmt = &Fxtran::stmt ($en);
            $optional[$iarg] = &f ('./f:attribute/f:attribute-N[text ()="OPTIONAL"]', $stmt) ? 1 : 0;
          }
      }

    if (scalar (@actual) < scalar (grep { ! $_ } @optional))
      {
        die $call->textContent;
      }

    if (scalar (@actual) > scalar (@optional))
      {
        die $call->textContent;
      }

    die if (grep { $_ } @optional);

    for my $iarg (0 .. $#dummy)
      {
        my @en = &f ('.//f:EN-decl[./f:EN-N/f:N/f:n[text ()="' . $dummy[$iarg] . '"]]', $pu);
        for my $en (@en)
          {
            my $stmt = &Fxtran::stmt ($en);
            my @ssd = &f ('.//f:array-spec//f:shape-spec', $stmt);

            next unless (@ssd);
 
            next if ($actual[$iarg]->nodeName eq 'literal-E');

            unless ($actual[$iarg]->nodeName eq 'named-E')
              {
                die $call->textContent . ' ' . $actual[$iarg]->textContent;
              }

            my ($ref) = &f ('.//f:R-LT', $actual[$iarg]);

            if ($ref) # Handle (:,:,10)
              {
                my @ss = &f ('./f:parens-R/f:array-R/f:section-subscript-LT/f:section-subscript/node ()', $ref);
                die unless (@ss);
                @ss = map { $_->textContent } @ss;
                for my $iss (0 .. $#ssd)
                  {
                    die "$iss: $ss[$iss]" unless ($ss[$iss] eq ':');
                  }
              }


            my ($actual) = &f ('./f:N/f:n/text ()', $actual[$iarg], 1);

            die $call->textContent unless ($actual);

            my @ssa = @{ $dims{$actual} };

            for my $iss (0 .. $#ssd)
              {
                $ssd[$iss]->replaceNode (&t ($ssa[$iss]));
              }
          }
      }

    'FileHandle'->new (">$file.new")->print ($doc->textContent ());


  }


