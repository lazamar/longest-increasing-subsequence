#!/usr/bin/perl

use List::Util qw(min);

my @alloc;
my @in_use;
my @time;

while (<>) {
  m!<<ghc: (\d+) bytes, \d+ GCs, \d+/\d+ avg/max bytes residency \(\d+ samples\), (\d+)M in use, [\d.]+ INIT \(([\d.]+) elapsed\), [\d.]+ MUT \(([\d.]+) elapsed\), [\d.]+ GC \(([\d.]+) elapsed\) :ghc>>! or die $!;
  push @alloc, 0+$1;
  push @in_use, $2;
  push @time, $3+$4+$5;
}

printf "%d;%d;%f\n", min(@alloc), min(@in_use), min(@time);
