use strict;
use warnings;
use Module::Build;

my $builder = Module::Build->new(
    module_name         => 'Inline::Fortran',
    license             => 'perl',
    dist_author         => 'Kenneth Kroenlein <kennethk@cpan.org>',
    dist_version_from   => 'lib/Inline/Fortran.pm',
    build_requires => {
        'Test::Most' => '0.31',
    },
    add_to_cleanup      => [ 'Inline-Fortran-*' ],
);

$builder->create_build_script();
