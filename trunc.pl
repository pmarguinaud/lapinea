#!/usr/bin/perl -w
#
use File::Copy;
use File::stat;
use FileHandle;
use strict;

my @f = qw (SHALLOW_MF.IN SHALLOW_MF.OUT);

my $count1 = 20;

my $count = do { my $fh = 'FileHandle'->new ("<SHALLOW_MF.COUNT"); local $/ = undef; <$fh> };

for my $f (@f)
  {
    my $g = "$f.$count1";
    my $st = stat ($f);
    my $size = $st->size () / $count;
 
    die "size=$size" unless (int ($size) == $size);

    $size *= $count1;
  
    &copy ($f, $g);

    truncate ($g, $size);
    
  }

'FileHandle'->new (">SHALLOW_MF.COUNT.$count1")->print ("$count1\n");

for my $f (qw (SHALLOW_MF.IN SHALLOW_MF.OUT SHALLOW_MF.COUNT))
  {
    my $g = "$f.$count1";
    rename ($g, $f);
  }


