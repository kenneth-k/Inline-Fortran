#!/usr/bin/perl
use strict;
use warnings;

use Test::More;

my (@trees, @todos, $use_worked, $base_count);
BEGIN {
    $base_count = 15;
    @trees = (
        
        );
    @todos = (

        );
    
    plan tests => ($base_count + @trees + @todos);
    
    $use_worked = use_ok 'Inline::Fortran::Types';
}

my $typer = bless {
                        do{
                            %{Types::get_mapper()};
                        }
                   }, 'Types';
$typer->{data}{build_dir} = '/home/kennethk/development/InlineFortran/Inline/Fortran/tmp_fort';
$typer->{data}{FC} = 'gfortran';
$typer->{data}{FFLAGS} = '-r8';
$typer->{data}{make} = 'make';
SKIP: {
    unless ($use_worked) {
        skip "Types module failed on load", --$base_count + @trees + @todos;
    }
    if (not ok($typer->_name_mangling(), "Name mangling determination")) {
        skip "Name mangling determination failed", --$base_count + @trees + @todos
    }
    
    my $pass = 1;
    $pass &&= ok($typer->_integer(), "Default integer determination");
    $pass &&= ok($typer->_real(), "Default real determination");
    $pass &&= ok($typer->_logical(), "Default logical determination");
    $pass &&= ok($typer->_complex(), "Default complex determination");
    $base_count -= 4;
    skip "Default mapping failure", $base_count + @trees + @todos unless $pass;

    if (not ok($typer->_string_passing(), "String passing determination")) {
        skip "String passing determination failed", --$base_count + @trees + @todos
    }

    if (not ok($typer->_all_defaults(), "Bulk defaults determination")) {
        skip "Bulk defaults determinations failed", --$base_count + @trees + @todos
    }

    $pass &&= ok($typer->_integer('INTEGER(4)'), 'INTEGER(4)');
    $pass &&= ok($typer->_integer('INTEGER(8)'), 'INTEGER(8)');
    $pass &&= ok($typer->_real('REAL(4)'), 'REAL(4)');
    $pass &&= ok($typer->_real('REAL(8)'), 'REAL(8)');
    $pass &&= ok($typer->_real('DOUBLE PRECISION'), 'DOUBLE PRECISION');
    $pass &&= ok($typer->_logical('LOGICAL(1)'), 'LOGICAL(1)');
    $pass &&= ok($typer->_complex('DOUBLE COMPLEX'), 'DOUBLE COMPLEX');
}
1;