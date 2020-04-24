use Test;
use lib 'lib';
use Binex;
use Binex::Classes;

my $blob = blob8.new(1,2,3,4,0);

my $bx = bx 'b...._...1+ %% b...._...0';
dump $bx;
$blob ~~ $bx;

say $/.Blob;

done-testing;