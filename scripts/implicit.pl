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

my ($f, @a) = @ARGV;

my $d = &Fxtran::fxtran (location => $f);

&Implicit::makeImplicit ($d, name => \@a);

'FileHandle'->new (">$f.new")->print ($d->textContent);










