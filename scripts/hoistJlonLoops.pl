#!/usr/bin/perl -w
#
use strict;
use FileHandle;
use Data::Dumper;

use FindBin qw ($Bin);
use lib $Bin;
use Fxtran;
use SingleBlock;

my $f = shift;

my $d = &Fxtran::fxtran (location => $f);

&SingleBlock::hoistJlonLoops ($d);

'FileHandle'->new (">$f.new")->print ($d->textContent);
