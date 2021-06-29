#!/usr/bin/perl -w

use strict;
use FindBin qw ($Bin);
use lib $Bin;
use FileHandle;
use File::Copy;
use File::Basename;
use File::stat;
use File::Path;
use Getopt::Long;

my %opts;

sub newer
{
  my ($f1, $f2)  = @_;
  die unless (-f $f1);
  return 1 unless (-f $f2);
  return stat ($f1)->mtime > stat ($f2)->mtime;
}

sub copyIfNewer
{
  my ($f1, $f2) = @_;

  if (&newer ($f1, $f2))
    {
      print "Copy $f1 to $f2\n"; 
      &copy ($f1, $f2); 
    }
}

sub saveToFile
{
  my ($x, $f) = @_;

  unless (-d (my $d = &dirname ($f)))
    {
      &mkpath ($d);
    }

  'FileHandle'->new (">$f")->print ($x->textContent ());
  'FileHandle'->new (">$f.xml")->print ($x->toString ());
}

sub preProcessIfNewer
{
  use Inline;
  use Associate;
  use Fxtran;
  use Blocks;
  use SingleBlock;

  my ($f1, $f2) = @_;

  if (&newer ($f1, $f2))
    {
      print "Preprocess $f1\n";

      my $d = &Fxtran::fxtran (location => $f1);
      &saveToFile ($d, "tmp/$f2");

      &Inline::inlineContainedSubroutines ($d);
      &saveToFile ($d, "tmp/inlineContainedSubroutines/$f2");

      &Associate::resolveAssociates ($d);
      &saveToFile ($d, "tmp/resolveAssociates/$f2");

      unless ($opts{'single-block'})
        {
          &Blocks::addBlocks ($d);
          &saveToFile ($d, "tmp/addBlocks/$f2");
        }
 
      if ($opts{'kernels'})
        {
#         &Blocks::exchangeJlonJlevLoops ($d);
#         &Blocks::addKernelDirectives ($d);
        }
      else
        {
          if ($opts{'single-block'})
            {
#             &SingleBlock::hoistJlonLoops ($d);
#             &SingleBlock::addParallelLoopDirectives ($d);
            }
          else
            {
              &Blocks::addParallelLoopDirectives ($d);
            }
        }


      &Blocks::addDataDirectives ($d);
      &saveToFile ($d, "tmp/addDirectives/$f2");

      'FileHandle'->new (">$f2")->print ($d->textContent ());

      &Fxtran::intfb ($f2);
    }
}

my @opts_f = qw (update compile kernels single-block);
my @opts_s = qw (arch bin);

&GetOptions
(
  map ({ ($_,     \$opts{$_}) } @opts_f),
  map ({ ("$_=s", \$opts{$_}) } @opts_s),
);

my @compute = map { &basename ($_) } (<compute/*.F90>, <compute/*.h>);
my @support = map { &basename ($_) } (<support/*.F90>, <support/*.h>);

&mkpath ("compile.$opts{arch}");

chdir ("compile.$opts{arch}");

if ($opts{update})
  {
    for my $f (@support)
      {
        &copyIfNewer ("../support/$f", $f);
      }
    
    for my $f (@compute)
      {
        &preProcessIfNewer ("../compute/$f", $f);
      }

    &copy ("../Makefile.$opts{arch}", "Makefile.inc");

    system ("$Bin/Makefile.PL") and die;
  }

if ($opts{compile})
  {
    system ("make -j4 $opts{bin}") and die;
  }





