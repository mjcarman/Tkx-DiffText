#!perl -T

use Test::More tests => 1;

BEGIN {
	use_ok( 'Tkx::DiffText' );
}

diag( "Testing Tkx::DiffText $Tkx::DiffText::VERSION, Perl $], $^X" );
