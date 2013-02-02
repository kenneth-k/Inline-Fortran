package Inline::Fortran;
use strict;
use warnings;

our $VERSION = '0.01';
our @ISA = qw(Inline);

require Inline;
use Carp;
use Config;

#===========================================================
# Register Fortran as an Inline Language Support Module (ILSM)
#===========================================================
sub register {
    return {
        language => 'Fortran',
        aliases => [qw(fortran
                       FORTRAN
                       F90
                       f90
                       F95
                       f95
                       F77
                       f77
                       F
                       f
                    )],
        type => 'compiled',
        suffix => $Config{so},
       };
}

#===========================================================
# Validate the Fortran Config Options
#===========================================================
sub validate {
    my $self = shift;
    $self->{ILSM}{KEY} ||= 'DEFAULT';
    while (@_) {
        my ($key, $value) = splice @_, 0, 2;
        if ($key eq 'KEY') {
            if ($value and not $value) {
                # Value is valid
                croak "Can't get here";
                $self->{ILSM}{KEY} = $value;
                next;
            }
            croak usage_config_value($key, $value);
        }
        croak usage_config_key($key);
    }
}

# Error messages
sub usage_config_key { 
    my ($key) = @_;
    return "'$key' is not a valid config option for ".__PACKAGE__;
}
sub usage_config_value { 
    my ($key, $value) = @_;
    return "'$value' is not a valid value for ".__PACKAGE__." config option '$key'";
}

#===========================================================
# Parse and compile Fortran code
#===========================================================
my $total_build_time;
sub build {
    my $self = shift;

    if ($self->{CONFIG}{BUILD_TIMERS}) {
        eval {require Time::HiRes};
        croak "You need Time::HiRes for BUILD_TIMERS option:\n$@" if $@;
        $total_build_time = Time::HiRes::time();
    }
    $self->call('parse', 'Determine Fortran call stacks');
    $self->call('type', 'Determine variable mappings');
    $self->call('write_XS', 'Autogenerate XS code');
    $self->call('write_Inline_headers', 'Autogenerate header files');
    $self->call('write_Makefile_PL', 'Autogenerate Makefile.pl');
    $self->call('compile', 'Compile');
    if ($self->{CONFIG}{BUILD_TIMERS}) {
        $total_build_time = Time::HiRes::time() - $total_build_time;
        printf STDERR "Total Build Time: %5.4f secs\n", $total_build_time;
    }
}

sub call {
    my ($self, $method, $header, $indent) = (@_, 0);
    my $time; 
    my $i = ' ' x $indent;
    print STDERR "${i}Starting $header Stage\n" if $self->{CONFIG}{BUILD_NOISY};
    $time = Time::HiRes::time() 
        if $self->{CONFIG}{BUILD_TIMERS};
    
    $self->$method();

    $time = Time::HiRes::time() - $time 
        if $self->{CONFIG}{BUILD_TIMERS};
    print STDERR "${i}Finished $header Stage\n" if $self->{CONFIG}{BUILD_NOISY};
    printf STDERR "${i}Time for $header Stage: %5.4f secs\n", $time 
        if $self->{CONFIG}{BUILD_TIMERS};
    print STDERR "\n" if $self->{CONFIG}{BUILD_NOISY};
}

sub system_call {
    my ($self, $cmd, $output_file) = @_;
    my $build_noisy = 
        defined $ENV{PERL_INLINE_BUILD_NOISY}
        ? $ENV{PERL_INLINE_BUILD_NOISY}
        : $self->{CONFIG}{BUILD_NOISY};
    if (not $build_noisy) {
        $cmd = "$cmd > $output_file 2>&1";
    }
    ($cmd) = $cmd =~ /(.*)/ if $self->UNTAINT;
    system($cmd) == 0 
        or croak($self->build_error_message($cmd, $output_file, $build_noisy));
}

sub build_error_message {
    my ($self, $cmd, $output_file, $build_noisy) = @_;
    my $build_dir = $self->{API}{build_dir};
    my $output = '';
    if (not $build_noisy and
        open(OUTPUT, $output_file)
       ) {
        local $/;
        $output = <OUTPUT>;
        close OUTPUT;
    }
    
    return $output . <<END;

A problem was encountered while attempting to compile and install your Inline
$self->{API}{language} code. The command that failed was:
  $cmd

The build directory was:
$build_dir

To debug the problem, cd to the build directory, and inspect the output files.

END
}
#==============================================================================
# Parse the function definition information out of the Fortran code
#==============================================================================
sub parse {
    my $self = shift;
    return if $self->{ILSM}{parser};
    return if $self->{ILSM}{XSMODE};
    my $parser = $self->{ILSM}{parser} = $self->get_parser;
    $parser->{data}{language_id} = $self->{ILSM}{language_id};
    #$parser->{data}{build_dir} = $self->{API}{build_dir};
    #$parser->{data}{make} = $self->{ILSM}{MAKE} || $Config{make}
    #    or croak "Can't locate your make binary";
    #$parser->{data}{system_call} = sub {$self->system_call(@_)};
    $parser->code($self->{ILSM}{code})
      or croak <<END;
Bad $self->{API}{language} code passed to Inline at @{[caller(2)]}
END
}

# Create and initialize a parser
sub get_parser {
    my $self = shift;
    require Inline::Fortran::Parse;
    return Inline::Fortran::Parse::get_parser($self);
}

#===========================================================
# Determine type mapping between C and Fortran
#===========================================================
sub type {
    my $self = shift;
    return if $self->{ILSM}{mapper};
    my $mapper = $self->{ILSM}{mapper} = $self->get_mapper;
    $mapper->{data}{build_dir} = $self->{API}{build_dir};
    $mapper->{data}{make} = $self->{ILSM}{MAKE} || $Config{make}
        or croak "Can't locate your make binary";
    #$mapper->{data}{system_call} = sub {$self->system_call(@_)};

    $mapper->types($self->{ILSM}{parser}{types})
      or croak <<END;
Mapping failure in $self->{API}{language} code passed to Inline at @{[caller(2)]}
END
}

# Create and initialize a type mapper
sub get_mapper {
    my $self = shift;
    require Inline::Fortran::Types;
    return Inline::Fortran::Types::get_mapper($self);
}

#==============================================================================
# Run the build process.
#==============================================================================
sub compile {
    my $self = shift;

    my $build_dir = $self->{API}{build_dir};
    my $cwd = &cwd;
    ($cwd) = $cwd =~ /(.*)/ if $self->UNTAINT;
    
    chdir $build_dir;
    $self->call('makefile_pl', '"perl Makefile.PL"', 2);
    $self->call('make', '"make"', 2);
    $self->call('make_install', '"make install"', 2);
    chdir $cwd;
    $self->call('cleanup', 'Cleaning Up', 2);
}

sub makefile_pl {
    my ($self) = @_;
    my $perl;
    -f ($perl = $Config{perlpath})
      or ($perl = $^X)
      or croak "Can't locate your perl binary";
    $self->system_call("$perl Makefile.PL", 'out.Makefile_PL');
    $self->fix_make;
}
sub make {
    my ($self) = @_;
    my $make = $self->{ILSM}{MAKE} || $Config{make}
      or croak "Can't locate your make binary";
    $self->system_call("$make", 'out.make');
}
sub make_install {
    my ($self) = @_;
    my $make = $self->{ILSM}{MAKE} || $Config{make}
      or croak "Can't locate your make binary";
    $self->system_call("$make pure_install", 'out.make_install');
}
sub cleanup {
    my ($self) = @_;
    my ($modpname, $modfname, $install_lib) = 
      @{$self->{API}}{qw(modpname modfname install_lib)};
    if ($self->{API}{cleanup}) {
        $self->rmpath(File::Spec->catdir($self->{API}{directory},'build'),
                   $modpname);
        my $autodir = File::Spec->catdir($install_lib,'auto',$modpname);
        unlink (File::Spec->catfile($autodir,'.packlist'),
                File::Spec->catfile($autodir,'$modfname.bs'),
                File::Spec->catfile($autodir,'$modfname.exp'), #MSWin32
                File::Spec->catfile($autodir,'$modfname.lib'), #MSWin32
               );
    }
}

#===========================================================
# Return a small report about the Fortran code.
#===========================================================
sub info {
    my $self = shift;
    my $text =  <<"HEADER";
Information generated in parsing the code inlined with $self->{API}{language_id}
HEADER
    if (my @subs = keys %{$self->{ILSM}{parser}{subs}}) {
        $text .=    <<"SUBROUTINES";
The following subroutines and functions were successfully located and parsed:
SUBROUTINES
        foreach (@subs) {
            $text .= "    $_\n"
        }
    }
    if (my @comments = @{$self->{ILSM}{parser}{comments}}) {
        $text .=    <<"SUBROUTINES";
The following comments were generated while attempting to parse:
SUBROUTINES
        foreach (@comments) {
            $text .= "    $_\n"
        }
    }
    return $text;
}

1;
__DATA__

=head1 NAME

Inline::Fortran - Write Perl Subroutines in Fortran

=head1 DESCRIPTION

Inline::Fortran is a module that allows you to write Perl subroutines in
Fortran. Since version 0.30 the Inline module supports multiple programming
languages and each language has its own support module. This document describes
how to use Inline with the Fortran programming language.

For more information on Inline in general, see Inline.

=head1 SEE ALSO

For general information about Inline see Inline.

For information on supported languages and platforms see Inline-Support.

For information on writing your own Inline Language Support Module, see Inline-API.

Inline's mailing list is inline@perl.org

To subscribe, send email to inline-subscribe@perl.org

=head1 AUTHOR

Kenneth Kroenlein <kennethk@cpan.org>

=head1 COPYRIGHT

This software is released under the terms of the Artistic license.

Copyright (c) 2009. Kenneth Kroenlein. All rights reserved.

See http://www.perl.com/perl/misc/Artistic.html

=cut