package Inline::Fortran::Types;
use strict;
use warnings;

use Carp;
use File::Spec;
use Cwd qw(getcwd);

our $VERSION = '0.01';
 
#===============================================================================
# Files required for calling protocol and type map determination
#===============================================================================
# All files are either direct copies or derived from the config program from the
# ATLAS package
#===============================================================================
# Register Fortran::Parse as an Inline Language Support Module (ILSM)
#===============================================================================
sub register {
    return  {   extends => [qw(Fortran)],
                overrides => [qw(get_types)],
            }
}

#===============================================================================
# Constructor
#===============================================================================
sub get_mapper {
    my $self = bless {}, 'Inline::Fortran::Types';

#-------------------------------------------------------------------------------
# Files required for calling protocol and type map determination.  These are
# localized in case the end-user desires to use different compilers with
# different mappings for different code chunks.
#
# Some of this material is derived from the config program in the Automatically
# Tuned Linear Algebra Software (ATLAS) package,
# (C) Copyright 1999 R. Clint Whaley

    $self->{f2c}{param}{order} = [qw(name int string real logical complex)];
    $self->{f2c}{param}{test_vars} = {int => 'INTEGER',
                                      real => 'REAL',
                                      logical => 'LOGICAL',
                                      complex => 'COMPLEX',
                                      };
    $self->{f2c}{param}{pre_code} = {};
    $self->{f2c}{param}{post_code} = {
        name => sub {
                my ($self,$mapping) = @_;
                # Create mangling subref
                # Called with ($code,$sub_name), with side effects
                if      ($mapping eq 'Add_') {
                    $self->{mangle} = sub {$_[0] =~ s/\b$_[1]\b/$_[1]_/g;}
                } elsif ($mapping eq 'Add__') {
                    $self->{mangle} = sub {$_[0] =~ s/\b$_[1]\b/$_[1]__/g;}
                } elsif ($mapping eq '_Add') {
                    $self->{mangle} = sub {$_[0] =~ s/\b$_[1]\b/_$_[1]/g;}
                } elsif ($mapping eq 'UpCase') {
                    $self->{mangle} = sub {$_[0] =~ s/\b$_[1]\b/uc $_[1]/eg;}
                }
                return $mapping;
            },
        logical => sub {
                my ($self,$mapping) = @_;
                if ($mapping =~ ',') {
                    ($mapping,$self->{true_value}) = split /,/, $mapping;
                }
                return $mapping;
            }
    };


# Determine Fortran compiler name mangling
    $self->{f2c}{param}{func}{name} = [qw(cname)];
    $self->{f2c}{code}{name}{f} =  <<'F2C_NAME_F';
      PROGRAM NAMTST
      EXTERNAL CNAME
*
      CALL CNAME()
*
      STOP
      END
F2C_NAME_F

    $self->{f2c}{code}{name}{c} =  <<'F2C_NAME_C';
#include <stdio.h>
void cname_(double *d)  { printf("result=<Add_>\n");     }
void cname(double *d)   { printf("result=<NoChange>\n"); }
void CNAME(double *d)   { printf("result=<UpCase>\n");   }
void cname__(double *d) { printf("result=<Add__>\n");    }
void _cname(double *d)  { printf("result=<_Add>\n");     }
F2C_NAME_C

# Determine Fortran compiler integer default
    $self->{f2c}{param}{func}{int} = [qw(cint)];
    $self->{f2c}{code}{int}{f} =  <<'F2C_INT_F';
      PROGRAM INTTST
      INTEGER X(8)
      INTEGER I
      EXTERNAL CINT
*
      DO 100 I = 1,8
          X(I) = -1
100   CONTINUE
      X(1) = 1
      CALL CINT(X)
*
      STOP
      END
F2C_INT_F

    $self->{f2c}{code}{int}{c} =  <<'F2C_INT_C';
#include <stdio.h>
void cint(void *vp)
{
    long long *llp=vp;
    long *lp=vp;
    int *ip=vp;
    short *sp=vp;
    char *cp=vp;

    if      ( (sizeof(long long) != sizeof(long)) && (*llp == 1) )
        printf("result=<long long>\n");
    else if ( (sizeof(long) != sizeof(int))       && (*lp  == 1) )
        printf("result=<long>\n");
    else if (                                        (*ip  == 1) )
        printf("result=<int>\n");
    else if (                                        (*sp  == 1) )
        printf("result=<short>\n");
    else if (                                        (*cp  == 1) )
        printf("result=<char>\n");
    else
        printf("Integer Determination Failed\n");
}
F2C_INT_C

# Determine Fortran compiler logical default
    $self->{f2c}{param}{func}{logical} = [qw(cbool)];
    $self->{f2c}{code}{logical}{f} =  <<'F2C_LOGICAL_F';
      PROGRAM LOGTST
      LOGICAL BOOL(64)
*      
      DO 100 I = 1,64
          BOOL(I) = .TRUE.
100   CONTINUE
      BOOL(3) = .FALSE.
      CALL CBOOL(BOOL)
*
      STOP
      END
F2C_LOGICAL_F

    $self->{f2c}{code}{logical}{c} =  <<'F2C_LOGICAL_C';
#include<stdio.h>
void cbool(void *vp) {
    signed char *cp=vp;
    signed short int *sp=vp;
    signed int *ip=vp;
    signed long *lp=vp;
    signed long long *llp=vp;

    if     (cp[0] && cp[1] && ! cp[2]) {
        printf("result=<signed char,%hhd>\n", cp[0]);
    }
    else if(sp[0] && sp[1] && ! sp[2]) {
        printf("result=<signed short,%hd>\n", sp[0]);
    }
    else if(ip[0] && ip[1] && ! ip[2]) {
        printf("result=<signed int,%d>\n", ip[0]);
    }
    else if(lp[0] && lp[1] && ! lp[2]) {
        printf("result=<signed long,%ld>\n", lp[0]);
    }
    else if(llp[0] && llp[1] && ! llp[2]) {
        printf("result=<signed long long,%lld>\n", llp[0]);
    }
    else {
        printf("Failed\n");
    }
}
F2C_LOGICAL_C

# Determine Fortran compiler real default
    $self->{f2c}{param}{func}{real} = [qw(creal)];
    $self->{f2c}{code}{real}{f} =  <<'F2C_FLOAT_F';
      PROGRAM FLTTST
      REAL X(4)
      INTEGER I
      EXTERNAL CREAL
*
      DO 100 I = 1,4
          X(I) = -2.0e2
100   CONTINUE
      CALL CREAL(X)
*
      STOP
      END
F2C_FLOAT_F

    $self->{f2c}{code}{real}{c} =  <<'F2C_FLOAT_C';
#include <stdio.h>
void creal(void *vp) {
    float *fp = vp;
    double *dp = vp;
    long double *ldp = vp;
    
    if      ( *fp  == -2.0e2f ) 
        printf("result=<float>\n");
    else if ( *dp  == -2.0e2  )
        printf("result=<double>\n");
    else if ( *ldp == -2.0e2l )
        printf("result=<long double>\n");
    else
        printf("Real Determination Failed\n");
}
F2C_FLOAT_C

# Determine Fortran compiler complex default
    $self->{f2c}{param}{func}{complex} = [qw(ccmplx)];
    $self->{f2c}{code}{complex}{f} =  <<'F2C_COMPLEX_F';
      PROGRAM CPXTST
      COMPLEX X(4)
      INTEGER I
      EXTERNAL CCMPLX
*
      DO 100 I = 1,4
          X(I) = (-2.0e2,2.0e2)
100   CONTINUE
      CALL CCMPLX(X)
*
      STOP
      END
F2C_COMPLEX_F

    $self->{f2c}{code}{complex}{c} =  <<'F2C_COMPLEX_C';
#include<stdio.h>
void ccmplx(void *vp) {
    struct {float real; float cplx;} *fp = vp;
    struct {double real; double cplx;} *dp = vp;
    struct {long double real; long double cplx;} *ldp = vp;
    
    if      ( fp->real  == -2.0e2f && fp->cplx  ==  2.0e2f ) 
        printf("result=<struct {float real; float cplx;}>\n");
    else if ( dp->real  == -2.0e2  && dp->cplx  ==  2.0e2  )
        printf("result=<struct {double real; double cplx;}>\n");
    else if ( ldp->real == -2.0e2l && ldp->cplx ==  2.0e2l )
        printf("result=<struct {long double real; long double cplx;}>\n");
    else
        printf("Complex Determination Failed\n");
}
F2C_COMPLEX_C

# Determine Fortran compiler string passing method
    $self->{f2c}{param}{func}{string} = [qw(cstr01 cstr02 cstr03 cstr04)];
    $self->{f2c}{code}{string}{f} =  <<'F2C_STR_F';
      PROGRAM STRTST
      INTEGER N1, N2, N3, N4
      CHARACTER*3 STR1
      CHARACTER*5 STR2
      
      EXTERNAL CSTR01, CSTR02, CSTR03, CSTR04
*
      STR1 = '123'
      STR2 = '12345'
      N1 = -1
      N2 = -2
*      
      CALL CSTR01(STR1,N1,STR2,N2)
      CALL CSTR02(STR1,N1,STR2,N2)
      CALL CSTR03(STR1,N1,STR2,N2)
      CALL CSTR04(STR1,N1,STR2,N2)
*
      STOP
      END
F2C_STR_F

    $self->{f2c}{code}{string}{c} =  <<'F2C_STR_C';
#include<stdio.h>
#ifdef _CRAY
    #include<fortran.h>
#else
    #define _fcd char
    #define _fcdtocp(str) n1
#endif
typedef struct {char *cp; F77_INTEGER len;} F77_CHAR;

// Sun style
void cstr01_(char *str1, F77_INTEGER *n1, char *str2, F77_INTEGER *n2,
            F77_INTEGER three, F77_INTEGER five)
{
    if ( (*n1 != -1) || (*n2 != -2) || (three != 3) || (five != 5) ) return;
    if (str1[0] != '1' || str1[1] != '2' || str1[2] != '3') return;
    if (str2[0] != '1' || str2[1] != '2' || str2[2] != '3' ||
        str2[3] != '4' || str2[4] != '5') return;
    printf("result=<SunStyle>\n");
}

// Cray style
void cstr02_(_fcd *str1, F77_INTEGER *n1, _fcd *str2, F77_INTEGER *n2)
{
    if ( (*n1 != -1) || (*n2 != -2) ) return;
    if (*(_fcdtocp(str1)) != '1' || *(_fcdtocp(str2)) != '1' ) return;
    printf("result=<CrayStyle>\n");
}

// Struct value style
void cstr03_(F77_CHAR str1, F77_INTEGER *n1, F77_CHAR str2, F77_INTEGER *n2)
{
   if ( (*n1 != -1) || (*n2 != -2) || (str1.len != 3) || (str2.len != 5) ) return;
   if (str1.cp[0] != '1' || str1.cp[1] != '2' || str1.cp[2] != '3') return;
   if (str2.cp[0] != '1' || str2.cp[1] != '2' || str2.cp[2] != '3' ||
       str2.cp[3] != '4' || str2.cp[4] != '5') return;
    printf("result=<StructVal>\n");
}

// Struct pointer style
void cstr04_(F77_CHAR *str1, F77_INTEGER *n1, F77_CHAR *str2, F77_INTEGER *n2)
{
   if ( (*n1 != -1) || (*n2 != -2) || (str1->len != 3) || (str2->len != 5) ) return;
   if (str1->cp[0] != '1' || str1->cp[1] != '2' || str1->cp[2] != '3') return;
   if (str2->cp[0] != '1' || str2->cp[1] != '2' || str2->cp[2] != '3' ||
       str2->cp[3] != '4' || str2->cp[4] != '5') return;
    printf("result=<StructPtr>\n");
}
F2C_STR_C

    $self->{f2c}{code}{makefile} = <<'MAKEFILE'; # Makefile
.SUFFIXES:
.SUFFIXES: .c .f .o

OJ = -c
NM = -o
LOADER = $(FC)

ifeq 	($(FROM_PERL),TRUE)

default: all
all: 
	@echo ERROR: Should only be invoked automatically
%: %F.o %C.o
	$(LOADER) $(LFLAGS) $^ $(NM) $@     ;\
     	./$@                                ;\
	rm -f *.o $@

else

default : all
all: 
	@echo ERROR: Should only be invoked automatically
%:	
	@echo ""

endif

%.o : %.f
	$(FC) $(FFLAGS) $(OJ) $< $(NM) $@
%.o : %.c
	$(CC) $(CFLAGS) $(OJ) $< $(NM) $@
MAKEFILE

    return $self;
}

#===============================================================================
# Main subroutine
#===============================================================================
sub _all_defaults {
    my $self = shift;
    
    my $old_path = getcwd();
    my $working_path = File::Spec->catdir($self->{data}{build_dir},'map');
    if (not -d $working_path) {
        mkdir $working_path or croak "Could not create directory $working_path";
    }
    chdir($working_path) or croak "Could not chdir into directory $working_path";

    _write_map_file('Makefile',   $self->{f2c}{code}{makefile});

    foreach my $test (@{$self->{f2c}{param}{order}}) {

        $self->{$test} = $self->_determine_mapping($test)
            or return chdir($old_path) &&undef;

    }
    
    chdir($old_path);
    return 1;
}
#===============================================================================
sub _name_mangling {
    my $self = shift;
    
    my $old_path = getcwd();
    my $working_path = File::Spec->catdir($self->{data}{build_dir},'map');
    if (not -d $working_path) {
        mkdir $working_path or croak "Could not create directory $working_path";
    }
    chdir($working_path) or croak "Could not chdir into directory $working_path";

    _write_map_file('Makefile',   $self->{f2c}{code}{makefile});

    my $mapping = $self->_determine_mapping('name');
    $self->{name} = $mapping;
    
    chdir($old_path);
    return $mapping;
}
#===============================================================================
sub _integer {
    my $self = shift;
    my ($kind) = @_;
    
    my $old_path = getcwd();
    my $working_path = File::Spec->catdir($self->{data}{build_dir},'map');
    if (not -d $working_path) {
        mkdir $working_path or croak "Could not create directory $working_path";
    }
    chdir($working_path) or croak "Could not chdir into directory $working_path";

    _write_map_file('Makefile',   $self->{f2c}{code}{makefile});

    if (not defined $self->{mangle}) {
        $self->_determine_mapping('name') or return chdir($old_path) &&undef;
    }
    my $mapping = $self->_determine_mapping('int',$kind);
    $self->{int} = $mapping unless defined $kind;
    
    chdir($old_path);
    return $mapping;
}
#===============================================================================
sub _real {
    my $self = shift;
    my ($kind) = @_;
    
    my $old_path = getcwd();
    my $working_path = File::Spec->catdir($self->{data}{build_dir},'map');
    if (not -d $working_path) {
        mkdir $working_path or croak "Could not create directory $working_path";
    }
    chdir($working_path) or croak "Could not chdir into directory $working_path";

    _write_map_file('Makefile',   $self->{f2c}{code}{makefile});

    if (not defined $self->{mangle}) {
        $self->_determine_mapping('name') or return chdir($old_path) &&undef;
    }
    my $mapping = $self->_determine_mapping('real',$kind);
    $self->{real} = $mapping unless defined $kind;
    
    chdir($old_path);
    return $mapping;
}
#===============================================================================
sub _logical {
    my $self = shift;
    my ($kind) = @_;
    
    my $old_path = getcwd();
    my $working_path = File::Spec->catdir($self->{data}{build_dir},'map');
    if (not -d $working_path) {
        mkdir $working_path or croak "Could not create directory $working_path";
    }
    chdir($working_path) or croak "Could not chdir into directory $working_path";

    _write_map_file('Makefile',   $self->{f2c}{code}{makefile});

    if (not defined $self->{mangle}) {
        $self->_determine_mapping('name') or return chdir($old_path) &&undef;
    }
    my $mapping = $self->_determine_mapping('logical',$kind);
    $self->{logical} = $mapping unless defined $kind;
    
    chdir($old_path);
    return $mapping;
}
#===============================================================================
sub _complex {
    my $self = shift;
    my ($kind) = @_;
    
    my $old_path = getcwd();
    my $working_path = File::Spec->catdir($self->{data}{build_dir},'map');
    if (not -d $working_path) {
        mkdir $working_path or croak "Could not create directory $working_path";
    }
    chdir($working_path) or croak "Could not chdir into directory $working_path";

    _write_map_file('Makefile',   $self->{f2c}{code}{makefile});

    if (not defined $self->{mangle}) {
        $self->_determine_mapping('name') or return chdir($old_path) &&undef;
    }
    my $mapping = $self->_determine_mapping('complex',$kind);
    $self->{complex} = $mapping unless defined $kind;
    
    chdir($old_path);
    return $mapping;
}
#===============================================================================
sub _string_passing {
    my $self = shift;
    
    my $old_path = getcwd();
    my $working_path = File::Spec->catdir($self->{data}{build_dir},'map');
    if (not -d $working_path) {
        mkdir $working_path or croak "Could not create directory $working_path";
    }
    chdir($working_path) or croak "Could not chdir into directory $working_path";

    _write_map_file('Makefile',   $self->{f2c}{code}{makefile});

    if (not defined $self->{mangle}) {
        $self->_determine_mapping('name') or return chdir($old_path) &&undef;
    }

    $self->{int} = $self->_determine_mapping('int') unless defined $self->{int};
    if (not defined $self->{int}) {
        chdir($old_path);
        return undef;
    }

    my $mapping = $self->_determine_mapping('string');
    $self->{string} = $mapping;

    chdir($old_path);
    return $mapping;
}
#===============================================================================
sub _determine_mapping {
    my $self = shift;
    my ($test,$kind) = @_;
    
    if (defined(my $pre_code = $self->{f2c}{param}{pre_code}{$test})) {
        $pre_code->($self);
    }

    my $f_code = $self->{f2c}{code}{$test}{f};
    if (defined $kind and defined $self->{f2c}{param}{test_vars}{$test}) {
        $f_code =~ s/$self->{f2c}{param}{test_vars}{$test}/$kind/;
    }
    _write_map_file("${test}F.f", $f_code);
    
    my $c_code = $self->{f2c}{code}{$test}{c};
    if ($test eq 'name') {
        return $self->{'name'} if defined $self->{mangle};
    } else {
        if (defined $self->{mangle}) {
            foreach my $func (@{$self->{f2c}{param}{func}{$test}}) {
                $self->{mangle}($c_code,$func) ;
            }
        } else {
            return undef;
        }
    }
    $self->_rename_variable_types($c_code);
    _write_map_file("${test}C.c", $c_code);

    my $make_command = $self->{data}{make};
    $make_command .= " $test FROM_PERL=TRUE 2>&1";
    $make_command .= " CC=$self->{data}{CC}"
        if defined $self->{data}{CC};
    $make_command .= " CFLAGS=$self->{data}{CFLAGS}"
        if defined $self->{data}{CFLAGS};
    $make_command .= " FC=$self->{data}{FC}"
        if defined $self->{data}{FC};
    $make_command .= " FFLAGS=$self->{data}{FFLAGS}"
        if defined $self->{data}{FFLAGS};
    my $result = `$make_command`;
    
    my ($mapping) = $result =~ /result\=\<([^<>]*)\>/;
    if ($mapping) {
        if (defined(my $post_code = $self->{f2c}{param}{post_code}{$test})) {
            $mapping = $post_code->($self,$mapping);
        }
    } else {
        push @{$self->{comments}}, "$test test failure:\n$result";
    }
    
    return $mapping;
}
#===============================================================================
# Support functions
#===============================================================================
sub _write_map_file{
# Simple sub for writing a file to disk
    my ($filename,$filecontents) = @_;
    open my $fh, '>', $filename or croak "Open failure on $filename: $!";
    print $fh $filecontents;
    close $fh;
    return 1;
}
#===============================================================================
sub _rename_variable_types {
# Rename the C variable types according to previously determined type mappings
# Note that this uses magic to modify the code in place
    my $self = shift;
    #my $code = $_[0];

    # Map native integer type
    return 0 if not defined $self->{int};
    $_[0] =~ s/F77_INTEGER/$self->{int}/g;

    return 1;
}
#-------------------------------------------------------------------------------
1;
__DATA__
