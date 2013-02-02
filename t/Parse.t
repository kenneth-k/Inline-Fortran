#!/usr/bin/perl
use strict;
use warnings;

use Test::More;

my (@sources, @todos, $use_worked);
BEGIN {
    @sources = (
                \&f77_dirty,
                \&f90_cleaning,
                \&f90_constants,
                \&f90_variables,
                \&f90_variations,
        );
    @todos = (
                \&implicit_external,
                \&zeroin,
        );
    
    plan tests => (1 + 2*@sources + 2*@todos);
    
    $use_worked = use_ok 'Inline::Fortran::Parse';
}

my $parser = bless {}, 'Parse';
#my $parser = Inline::Fortran::Parse::get_parser();

#use Data::Dumper;
#$Data::Dumper::Purity = 1;
#my ($version,$name,$code,$clean,$tree) = f77_dirty();
#$parser->{data}{language_id} = $version;
#print join "\n", $parser->_clean($code);
#print Dumper($parser->_parse(@$clean));
#$parser->_parse(@$clean);
#1;

SKIP: {
    skip "Parse module failed on load", 2*@sources + 2*@todos unless $use_worked;

    foreach (@sources) {
        my ($version,$name,$code,$clean,$tree) = $_->();
        $parser->{data}{language_id} = $version;
        is_deeply([$parser->_clean($code)], $clean, "$parser->{data}{language_id} clean $name");
        my $parsed = $parser->_parse(@$clean);
        remove_code($parsed);
        is_deeply($parsed, $tree, "$parser->{data}{language_id} parse $name");
        1;
    }
    
    
    foreach (@todos) {
        my ($version,$name,$code,$clean,$tree) = $_->();
        $parser->{data}{language_id} = $version;
        is_deeply([$parser->_clean($code)], $clean, "$parser->{data}{language_id} clean $name");
        TODO: {
            local $TODO = "Known unaddressed parsing failures";
            my $parsed = $parser->_parse(@$clean);
            remove_code($parsed);
            is_deeply($parsed, $tree, "$parser->{data}{language_id} parse $name");
        }
    }
}
#-------------------------------------------------------------------------------
sub remove_code {
    # Crawls a tree, removing _head, _body and _tail elements
    my ($hash_ref) = @_;
    foreach my $key (keys %$hash_ref) {
        if ($key eq '_head' or $key eq '_body' or $key eq '_tail') {
            delete $hash_ref->{$key};
        }
    }
    foreach my $value (values %$hash_ref) {
        if (ref $value eq 'HASH') {
            remove_code($value);
        }
    }
}
#-------------------------------------------------------------------------------
# The code sets for testing
# Each subroutine returns a string that corresponds to the code and a hashref
# that corresponds to the expected tree
#-------------------------------------------------------------------------------
sub f77_dirty {
    my $version = 'f77';
    my ($name) = (caller(0))[3] =~ /:([^:]*$)/;
    my $code = <<'EOF';
C It is important that all styles of F77 commenting are included
c comment line
! comment line
* comment line
      SUBROUTINE somesub
C Check that common blocks and implicits are handled properly
      COMMON a,b,c
      COMMON /ints/ i,j,k
      COMMON /reals/ q,r,s

      write(*,*) "This line checks "//
     1           "whether continuations "//
     2           "are respected

C     Blank lines are meaningless

1     Write(*,*) "Line label 1"
 2    wRite(*,*) "Line label 2"
  3   wrIte(*,*) "Line label 3"
   4  wriTe(*,*) "Line label 4"
    5 writE(*,*) "Line label 5"
6	write(*,*) "It's also important to make sure"
    7	write(*,*) "leading tabs are treated correctly"

      END
      
      block data initial
      dimension list(3,2), ints(3)
      COMMON a,b,c
      Common /ints/ ints
      common /reals/ q, r, s
      common/array/list
      DATA a,b,c /1.0,2.0,3.0/
      Data ints /1, 2, 3/, q,r,s /3*0.0/
      data list/6*1/
      end
EOF
    my $clean = [split /\n/, <<'EOF'];
subroutine somesub
common a,b,c
common/ints/i,j,k
common/reals/q,r,s
write(*,*){str}"This line checks "{/str}//{str}"whether continuations "{/str}//"are respected
1 write(*,*)"line label 1"
2 write(*,*)"line label 2"
3 write(*,*)"line label 3"
4 write(*,*)"line label 4"
5 write(*,*)"line label 5"
6 write(*,*)"it's also important to make sure"
7 write(*,*)"leading tabs are treated correctly"
end
block data initial
dimension list(3,2),ints(3)
common a,b,c
common/ints/ints
common/reals/q,r,s
common/array/list
data a,b,c/1.0,2.0,3.0/
data ints/1,2,3/,q,r,s/3*0.0/
data list/6*1/
end
EOF
    my $tree  = {
                _class=> '_root',
                'block data' => {
                            _class=> 'block data',
                            _name => 'initial',
                            _implicit => {
                                    a => 'real',
                                    b => 'real',
                                    c => 'real',
                                    d => 'real',
                                    e => 'real',
                                    f => 'real',
                                    g => 'real',
                                    h => 'real',
                                    i => 'integer',
                                    j => 'integer',
                                    k => 'integer',
                                    l => 'integer',
                                    m => 'integer',
                                    n => 'integer',
                                    o => 'real',
                                    p => 'real',
                                    q => 'real',
                                    r => 'real',
                                    s => 'real',
                                    t => 'real',
                                    u => 'real',
                                    v => 'real',
                                    w => 'real',
                                    x => 'real',
                                    y => 'real',
                                    z => 'real',
                                },
                            _commons => {
                                    _anon => {
                                            _name => '_anon',
                                            _vars => [
                                                         'a',
                                                         'b',
                                                         'c',
                                                       ],
                                            _class=> 'common',
                                        },
                                    ints => {
                                            _name => 'ints',
                                            _vars => [
                                                         'ints',
                                                       ],
                                            _class=> 'common',
                                        },
                                    reals => {
                                            _name => 'reals',
                                            _vars => [
                                                         'q',
                                                         'r',
                                                         's',
                                                       ],
                                            _class=> 'common',
                                        },
                                    array => {
                                            _name => 'array',
                                            _vars => [
                                                         'list',
                                                       ],
                                            _class=> 'common',
                                        },
                                },
                            _data => [
                                    'a,b,c/1.0,2.0,3.0/',
                                    'ints/1,2,3/',
                                    'q,r,s/3*0.0/',
                                    'list/6*1/',
                                ],
                            a => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'a',
                                    _type => 'real',
                                    _common => '_anon',
                                    _data => 'a,b,c/1.0,2.0,3.0/',
                                },
                            b => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'b',
                                    _type => 'real',
                                    _common => '_anon',
                                    _data => 'a,b,c/1.0,2.0,3.0/',
                                },
                            c => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'c',
                                    _type => 'real',
                                    _common => '_anon',
                                    _data => 'a,b,c/1.0,2.0,3.0/',
                                },
                            ints => {
                                    _class=> 'variable',
                                    _dims => [
                                                3,
                                            ],
                                    _name => 'ints',
                                    _type => 'integer',
                                    _common => 'ints',
                                    _data => 'ints/1,2,3/',
                                },
                            q => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'q',
                                    _type => 'real',
                                    _common => 'reals',
                                    _data => 'q,r,s/3*0.0/',
                                },
                            r => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'r',
                                    _type => 'real',
                                    _common => 'reals',
                                    _data => 'q,r,s/3*0.0/',
                                },
                            s => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 's',
                                    _type => 'real',
                                    _common => 'reals',
                                    _data => 'q,r,s/3*0.0/',
                                },
                            list => {
                                    _class=> 'variable',
                                    _dims => [
                                                3,
                                                2,
                                            ],
                                    _name => 'list',
                                    _type => 'integer',
                                    _common => 'array',
                                    _data => 'list/6*1/',
                                },
                            },
                somesub => {
                            _args => [],
                            _mods => [],
                            _class=> 'subroutine',
                            _name => 'somesub',
                            _implicit => {
                                        a => 'real',
                                        b => 'real',
                                        c => 'real',
                                        d => 'real',
                                        e => 'real',
                                        f => 'real',
                                        g => 'real',
                                        h => 'real',
                                        i => 'integer',
                                        j => 'integer',
                                        k => 'integer',
                                        l => 'integer',
                                        m => 'integer',
                                        n => 'integer',
                                        o => 'real',
                                        p => 'real',
                                        q => 'real',
                                        r => 'real',
                                        s => 'real',
                                        t => 'real',
                                        u => 'real',
                                        v => 'real',
                                        w => 'real',
                                        x => 'real',
                                        y => 'real',
                                        z => 'real',
                                    },
                            _commons => {
                                        _anon => {
                                                  _name => '_anon',
                                                  _vars => [
                                                               'a',
                                                               'b',
                                                               'c',
                                                             ],
                                                  _class=> 'common',
                                                },
                                        ints => {
                                                  _name => 'ints',
                                                  _vars => [
                                                               'i',
                                                               'j',
                                                               'k',
                                                             ],
                                                  _class=> 'common',
                                                },
                                        reals => {
                                                  _name => 'reals',
                                                  _vars => [
                                                               'q',
                                                               'r',
                                                               's',
                                                             ],
                                                  _class=> 'common',
                                                },
                                    },
                            a => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'a',
                                    _type => 'real',
                                    _common => '_anon',
                                },
                            b => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'b',
                                    _type => 'real',
                                    _common => '_anon',
                                },
                            c => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'c',
                                    _type => 'real',
                                    _common => '_anon',
                                },
                            i => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'i',
                                    _type => 'integer',
                                    _common => 'ints',
                                },
                            j => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'j',
                                    _type => 'integer',
                                    _common => 'ints',
                                },
                            k => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'k',
                                    _type => 'integer',
                                    _common => 'ints',
                                },
                            q => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'q',
                                    _type => 'real',
                                    _common => 'reals',
                                },
                            r => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'r',
                                    _type => 'real',
                                    _common => 'reals',
                                },
                            s => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 's',
                                    _type => 'real',
                                    _common => 'reals',
                                },
                            }
                };
    $tree->{_subs}{somesub}             = $tree->{somesub};
    $tree->{somesub}{_vars}{a}          = $tree->{somesub}{a};
    $tree->{somesub}{_vars}{b}          = $tree->{somesub}{b};
    $tree->{somesub}{_vars}{c}          = $tree->{somesub}{c};
    $tree->{somesub}{_vars}{i}          = $tree->{somesub}{i};
    $tree->{somesub}{_vars}{j}          = $tree->{somesub}{j};
    $tree->{somesub}{_vars}{k}          = $tree->{somesub}{k};
    $tree->{somesub}{_vars}{q}          = $tree->{somesub}{q};
    $tree->{somesub}{_vars}{r}          = $tree->{somesub}{r};
    $tree->{somesub}{_vars}{s}          = $tree->{somesub}{s};
    $tree->{'block data'}{_vars}{a}     = $tree->{'block data'}{a};
    $tree->{'block data'}{_vars}{b}     = $tree->{'block data'}{b};
    $tree->{'block data'}{_vars}{c}     = $tree->{'block data'}{c};
    $tree->{'block data'}{_vars}{ints}  = $tree->{'block data'}{ints};
    $tree->{'block data'}{_vars}{q}     = $tree->{'block data'}{q};
    $tree->{'block data'}{_vars}{r}     = $tree->{'block data'}{r};
    $tree->{'block data'}{_vars}{s}     = $tree->{'block data'}{s};
    $tree->{'block data'}{_vars}{list}  = $tree->{'block data'}{list};
    
    return $version, $name, $code, $clean, $tree;
}
#-------------------------------------------------------------------------------
sub implicit_external {
    # This subroutine takes an implicitly defined external as an argument, and
    # is wholly legal Fortran
    my $version = 'f77';
    my ($name) = (caller(0))[3] =~ /:([^:]*$)/;
    my $code = <<'EOF';
      SUBROUTINE impext(f, x)
      WRITE(*,*) f(x)
      END
EOF
    my $clean = [split /\n/, <<'EOF'];
subroutine impext(f,x)
write(*,*) f(x)
end
EOF
    my $tree  = {
                _class=> '_root',
                impext => {
                            _args => [
                                        'f',
                                        'x',
                                    ],
                            _mods => [],
                            _class=> 'subroutine',
                            _name => 'impext',
                            _implicit => {
                                        a => 'real',
                                        b => 'real',
                                        c => 'real',
                                        d => 'real',
                                        e => 'real',
                                        f => 'real',
                                        g => 'real',
                                        h => 'real',
                                        i => 'integer',
                                        j => 'integer',
                                        k => 'integer',
                                        l => 'integer',
                                        m => 'integer',
                                        n => 'integer',
                                        o => 'real',
                                        p => 'real',
                                        q => 'real',
                                        r => 'real',
                                        s => 'real',
                                        t => 'real',
                                        u => 'real',
                                        v => 'real',
                                        w => 'real',
                                        x => 'real',
                                        y => 'real',
                                        z => 'real',
                                    },
                            f => {
                                    _args => [
                                            'x',
                                        ],
                                    _mods => [],
                                    _class=> 'function',
                                    _dims => [],
                                    _name => 'f',
                                    _intent => 'in',
                                    _external => 1,
                                    f => {
                                            _class=> 'variable',
                                            _dims => [],
                                            _name => 'f',
                                            _type => 'real',
                                            _intent => 'out',
                                        },
                                    x => {
                                            _class=> 'variable',
                                            _dims => [],
                                            _name => 'x',
                                            _type => 'real',
                                            _intent => 'inout',
                                        },
                                },
                            x => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'x',
                                    _type => 'real',
                                    _intent => 'inout',
                                },
                        },
            };
     $tree->{_subs}{impext}             = $tree->{impext};
     $tree->{impext}{_subs}{f}          = $tree->{impext}{f};
     $tree->{impext}{_vars}{x}          = $tree->{impext}{x};
   
    return $version, $name, $code, $clean, $tree;
}
#-------------------------------------------------------------------------------
sub zeroin{
    # The zeroin function from Netlib's Golden Oldies (http://www.netlib.org/go/zeroin.f)
    my $version = 'f77';
    my ($name) = (caller(0))[3] =~ /:([^:]*$)/;
    my $code = <<'EOF';
c  To get d1mach, mail netlib
c       send d1mach from core
      double precision function zeroin(ax,bx,f,tol)
      double precision ax,bx,f,tol
c
c      a zero of the function  f(x)  is computed in the interval ax,bx .
c
c  input..
c
c  ax     left endpoint of initial interval
c  bx     right endpoint of initial interval
c  f      function subprogram which evaluates f(x) for any x in
c         the interval  ax,bx
c  tol    desired length of the interval of uncertainty of the
c         final result (.ge.0.)
c
c  output..
c
c  zeroin abscissa approximating a zero of  f  in the interval ax,bx
c
c      it is assumed  that   f(ax)   and   f(bx)   have  opposite  signs
c  this is checked, and an error message is printed if this is not
c  satisfied.   zeroin  returns a zero  x  in the given interval
c  ax,bx  to within a tolerance  4*macheps*abs(x)+tol, where macheps  is
c  the  relative machine precision defined as the smallest representable
c  number such that  1.+macheps .gt. 1.
c      this function subprogram is a slightly  modified  translation  of
c  the algol 60 procedure  zero  given in  richard brent, algorithms for
c  minimization without derivatives, prentice-hall, inc. (1973).
c
      double precision  a,b,c,d,e,eps,fa,fb,fc,tol1,xm,p,q,r,s
      double precision  dabs, d1mach
   10 eps = d1mach(4)
      tol1 = eps+1.0d0
c
      a=ax
      b=bx
      fa=f(a)
      fb=f(b)
c     check that f(ax) and f(bx) have different signs
      if (fa .eq.0.0d0 .or. fb .eq. 0.0d0) go to 20
      if (fa * (fb/dabs(fb)) .le. 0.0d0) go to 20
         write(6,2500)
2500     format(1x,'f(ax) and f(bx) do not have different signs,',
     1             ' zeroin is aborting')
         return
   20 c=a
      fc=fa
      d=b-a
      e=d
   30 if (dabs(fc).ge.dabs(fb)) go to 40
      a=b
      b=c
      c=a
      fa=fb
      fb=fc
      fc=fa
   40 tol1=2.0d0*eps*dabs(b)+0.5d0*tol
      xm = 0.5d0*(c-b)
      if ((dabs(xm).le.tol1).or.(fb.eq.0.0d0)) go to 150
c
c see if a bisection is forced
c
      if ((dabs(e).ge.tol1).and.(dabs(fa).gt.dabs(fb))) go to 50
      d=xm
      e=d
      go to 110
   50 s=fb/fa
      if (a.ne.c) go to 60
c
c linear interpolation
c
      p=2.0d0*xm*s
      q=1.0d0-s
      go to 70
c
c inverse quadratic interpolation
c
   60 q=fa/fc
      r=fb/fc
      p=s*(2.0d0*xm*q*(q-r)-(b-a)*(r-1.0d0))
      q=(q-1.0d0)*(r-1.0d0)*(s-1.0d0)
   70 if (p.le.0.0d0) go to 80
      q=-q
      go to 90
   80 p=-p
   90 s=e
      e=d
      if (((2.0d0*p).ge.(3.0d0*xm*q-dabs(tol1*q))).or.(p.ge.
     *dabs(0.5d0*s*q))) go to 100
      d=p/q
      go to 110
  100 d=xm
      e=d
  110 a=b
      fa=fb
      if (dabs(d).le.tol1) go to 120
      b=b+d
      go to 140
  120 if (xm.le.0.0d0) go to 130
      b=b+tol1
      go to 140
  130 b=b-tol1
  140 fb=f(b)
      if ((fb*(fc/dabs(fc))).gt.0.0d0) go to 20
      go to 30
  150 zeroin=b
      return
      end
EOF
    my $clean = [split /\n/, <<'EOF'];
doubleprecision function zeroin(ax,bx,f,tol)
doubleprecision ax,bx,f,tol
doubleprecision a,b,c,d,e,eps,fa,fb,fc,tol1,xm,p,q,r,s
doubleprecision dabs,d1mach
10 eps=d1mach(4)
tol1=eps+1.0d0
a=ax
b=bx
fa=f(a)
fb=f(b)
if(fa.eq.0.0d0.or.fb.eq.0.0d0) go to 20
if(fa*(fb/dabs(fb)).le.0.0d0) go to 20
write(6,2500)
2500 format(1x,{str}'f(ax) and f(bx) do not have different signs,'{/str},{str}' zeroin is aborting'{/str})
return
20 c=a
fc=fa
d=b-a
e=d
30 if(dabs(fc).ge.dabs(fb)) go to 40
a=b
b=c
c=a
fa=fb
fb=fc
fc=fa
40 tol1=2.0d0*eps*dabs(b)+0.5d0*tol
xm=0.5d0*(c-b)
if((dabs(xm).le.tol1).or.(fb.eq.0.0d0)) go to 150
if((dabs(e).ge.tol1).and.(dabs(fa).gt.dabs(fb))) go to 50
d=xm
e=d
go to 110
50 s=fb/fa
if(a.ne.c) go to 60
p=2.0d0*xm*s
q=1.0d0-s
go to 70
60 q=fa/fc
r=fb/fc
p=s*(2.0d0*xm*q*(q-r)-(b-a)*(r-1.0d0))
q=(q-1.0d0)*(r-1.0d0)*(s-1.0d0)
70 if(p.le.0.0d0) go to 80
q=-q
go to 90
80 p=-p
90 s=e
e=d
if(((2.0d0*p).ge.(3.0d0*xm*q-dabs(tol1*q))).or.(p.ge.dabs(0.5d0*s*q))) go to 100
d=p/q
go to 110
100 d=xm
e=d
110 a=b
fa=fb
if(dabs(d).le.tol1) go to 120
b=b+d
go to 140
120 if(xm.le.0.0d0) go to 130
b=b+tol1
go to 140
130 b=b-tol1
140 fb=f(b)
if((fb*(fc/dabs(fc))).gt.0.0d0) go to 20
go to 30
150 zeroin=b
return
end
EOF
    my $tree  = {
                _class=> '_root',
                zeroin => {
                            _args => [
                                        'ax',
                                        'bx',
                                        'f',
                                        'tol',
                                    ],
                            _mods => [
                                        'doubleprecision',
                                    ],
                            _class=> 'function',
                            _name => 'zeroin',
                            _implicit => {
                                        a => 'real',
                                        b => 'real',
                                        c => 'real',
                                        d => 'real',
                                        e => 'real',
                                        f => 'real',
                                        g => 'real',
                                        h => 'real',
                                        i => 'integer',
                                        j => 'integer',
                                        k => 'integer',
                                        l => 'integer',
                                        m => 'integer',
                                        n => 'integer',
                                        o => 'real',
                                        p => 'real',
                                        q => 'real',
                                        r => 'real',
                                        s => 'real',
                                        t => 'real',
                                        u => 'real',
                                        v => 'real',
                                        w => 'real',
                                        x => 'real',
                                        y => 'real',
                                        z => 'real',
                                    },
                            zeroin => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'zeroin',
                                    _type => 'doubleprecision',
                                    _intent => 'out',
                                },
                            ax => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'ax',
                                    _type => 'doubleprecision',
                                    _intent => 'inout',
                                },
                            bx => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'bx',
                                    _type => 'doubleprecision',
                                    _intent => 'inout',
                                },
                            f => {
                                    _args => [
                                            'x',
                                        ],
                                    _mods => [
                                            'doubleprecision',
                                        ],
                                    _class=> 'function',
                                    _dims => [],
                                    _name => 'f',
                                    _intent => 'in',
                                    _external => 1,
                                    f => {
                                            _class=> 'variable',
                                            _dims => [],
                                            _name => 'f',
                                            _type => 'doubleprecision',
                                            _intent => 'out',
                                        },
                                    x => {
                                            _class=> 'variable',
                                            _dims => [],
                                            _name => 'x',
                                            _type => 'doubleprecision',
                                            _intent => 'inout',
                                        },
                                },
                            tol => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'tol',
                                    _type => 'doubleprecision',
                                    _intent => 'inout',
                                },
                            a => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'a',
                                    _type => 'doubleprecision',
                                },
                            b => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'b',
                                    _type => 'doubleprecision',
                                },
                            c => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'c',
                                    _type => 'doubleprecision',
                                },
                            d => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'd',
                                    _type => 'doubleprecision',
                                },
                            e => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'e',
                                    _type => 'doubleprecision',
                                },
                            eps => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'eps',
                                    _type => 'doubleprecision',
                                },
                            fa => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'fa',
                                    _type => 'doubleprecision',
                                },
                            fb => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'fb',
                                    _type => 'doubleprecision',
                                },
                            fc => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'fc',
                                    _type => 'doubleprecision',
                                },
                            tol1 => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'tol1',
                                    _type => 'doubleprecision',
                                },
                            xm => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'xm',
                                    _type => 'doubleprecision',
                                },
                            p => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'p',
                                    _type => 'doubleprecision',
                                },
                            q => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'q',
                                    _type => 'doubleprecision',
                                },
                            r => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'r',
                                    _type => 'doubleprecision',
                                },
                            s => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 's',
                                    _type => 'doubleprecision',
                                },
                            dabs => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'dabs',
                                    _type => 'doubleprecision',
                                    _external => 1,
                                },
                            d1mach => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'd1mach',
                                    _type => 'doubleprecision',
                                    _external => 1,
                            },
                },
        };
    $tree->{_subs}{zeroin}          = $tree->{zeroin};
    $tree->{zeroin}{_vars}{zeroin}  = $tree->{zeroin}{zeroin};
    $tree->{zeroin}{_vars}{ax}      = $tree->{zeroin}{ax};
    $tree->{zeroin}{_vars}{bx}      = $tree->{zeroin}{bx};
    $tree->{zeroin}{_subs}{f}       = $tree->{zeroin}{f};
    $tree->{zeroin}{_vars}{tol}     = $tree->{zeroin}{tol};
    $tree->{zeroin}{_vars}{a}       = $tree->{zeroin}{a};
    $tree->{zeroin}{_vars}{b}       = $tree->{zeroin}{b};
    $tree->{zeroin}{_vars}{c}       = $tree->{zeroin}{c};
    $tree->{zeroin}{_vars}{d}       = $tree->{zeroin}{d};
    $tree->{zeroin}{_vars}{e}       = $tree->{zeroin}{e};
    $tree->{zeroin}{_vars}{eps}     = $tree->{zeroin}{eps};
    $tree->{zeroin}{_vars}{fa}      = $tree->{zeroin}{fa};
    $tree->{zeroin}{_vars}{fb}      = $tree->{zeroin}{fb};
    $tree->{zeroin}{_vars}{fc}      = $tree->{zeroin}{fc};
    $tree->{zeroin}{_vars}{tol1}    = $tree->{zeroin}{tol1};
    $tree->{zeroin}{_vars}{xm}      = $tree->{zeroin}{xm};
    $tree->{zeroin}{_vars}{p}       = $tree->{zeroin}{p};
    $tree->{zeroin}{_vars}{q}       = $tree->{zeroin}{q};
    $tree->{zeroin}{_vars}{r}       = $tree->{zeroin}{r};
    $tree->{zeroin}{_vars}{s}       = $tree->{zeroin}{s};
    $tree->{zeroin}{_subs}{dabs}    = $tree->{zeroin}{dabs};
    $tree->{zeroin}{_subs}{d1mach}  = $tree->{zeroin}{d1mach};
    
    return $version, $name, $code, $clean, $tree;
}
#-------------------------------------------------------------------------------
sub f90_cleaning {
    my $version = 'f90';
    my ($name) = (caller(0))[3] =~ /:([^:]*$)/;
    my $code = <<'EOF';
! Comments are easier to identify in free form
subroutine long_name
implicit none
write(*,*) 'This is a statement'
! A comment
write(*,*) 'This is a statement' ! followed by a comment
write(*,*) 'This is a statement', ' with two clauses' ! followed by a comment
write(*,*) 'This is not a comment!'
write(*,*) 'A double quote looks like "!'
write(*,*) "A single quote looks like '!"
write(*,*) 'This is a line'!followed by a comment!
write(*,*) 'Sometimes "creativity" is required; ' // "it's too bad but it's true!"
write(*,*) 'This is a statement'

write(*,*) 'This statement',& 
' is continued'
write(*,*) 'This statement',& 

' is continued with extra lines'
write(*,*) 'This is not continued&'
write(*,*) "Nor is this&"
write(*,*) 'Lines can be continued ' & ! and be followed by comments
        // 'and joined together'
write(*,*) '2 statements'; write(*,*) 'one line'; 
write(*,*) 'This is one statement;'
write(*,*) "As is this;"
write(*,*) "Two lines"; write(*,*) "On one line" ! and a comment to boot
200 write(*,*) 'Line numbering is still legal'

end
EOF
    my $clean = [split /\n/, <<'EOF'];
subroutine long_name
implicit none
write(*,*){str}'This is a statement'{/str}
write(*,*){str}'This is a statement'{/str}
write(*,*){str}'This is a statement'{/str},{str}' with two clauses'{/str}
write(*,*){str}'This is not a comment!'{/str}
write(*,*){str}'A double quote looks like "!'{/str}
write(*,*){str}"A single quote looks like '!"{/str}
write(*,*){str}'This is a line'{/str}
write(*,*){str}'Sometimes "creativity" is required; '{/str}//{str}"it's too bad but it's true!"{/str}
write(*,*){str}'This is a statement'{/str}
write(*,*){str}'This statement'{/str},{str}' is continued'{/str}
write(*,*){str}'This statement'{/str},{str}' is continued with extra lines'{/str}
write(*,*){str}'This is not continued&'{/str}
write(*,*){str}"Nor is this&"{/str}
write(*,*){str}'Lines can be continued '{/str}//{str}'and joined together'{/str}
write(*,*){str}'2 statements'{/str}
write(*,*){str}'one line'{/str}
write(*,*){str}'This is one statement;'{/str}
write(*,*){str}"As is this;"{/str}
write(*,*){str}"Two lines"{/str}
write(*,*){str}"On one line"{/str}
200 write(*,*){str}'Line numbering is still legal'{/str}
end
EOF
    my $tree  = {
                _class=> '_root',
                long_name => {
                            _args => [],
                            _mods => [],
                            _class=> 'subroutine',
                            _name => 'long_name',
                            _implicit => undef,
                    }
                };
    $tree->{_subs}{long_name}           = $tree->{long_name};
    
    return $version, $name, $code, $clean, $tree;
}
#-------------------------------------------------------------------------------
sub f90_constants {
    my $version = 'f90';
    my ($name) = (caller(0))[3] =~ /:([^:]*$)/;
    my $code = <<'EOF';
module constants_module
implicit none

private DUMMY_R8,DUMMY_I4

! Double precision (8-bit) real
integer, parameter :: R8  = selected_real_kind(p=13, r=200)
! Double precision (4-bit) integer
integer, parameter :: I4 = selected_int_kind(9)

! Mathematical constants
!********************************************************************
! Irrational numbers - From OEIS (Sloane's) ca. 2007 or inhouse via
! high-presision (0 round-off) math from OEIS source data
real(R8), parameter :: PI =                                         &
 3.14159265358979323846264338327950288419716939937510582097494e+0_R8
real(R8), parameter :: LN_TWO =                                     &
 6.93147180559945309417232121458176568075500134360255254120680e-1_R8
real(R8), parameter :: LN_TEN =                                     &
 2.30258509299404568401799145468436420760110148862877297603333e+0_R8
real(R8), parameter :: SQRT_TWO =                                   &
 1.41421356237309504880168872420969807856967187537694807317668e+0_R8

! Small increments for pertubations to avoid singular values
integer(i4),parameter :: DUMMY_I4  = 1
integer(i4),parameter :: HUGE_INT  = huge(DUMMY_I4) 
real(R8),parameter :: DUMMY_R8     = 1._R8
real(R8),parameter :: ROUND_OFF    = 2._R8*epsilon(DUMMY_R8)
real(R8),parameter :: X_SMALL      = 2._R8*tiny(DUMMY_R8)
real(R8),parameter :: V_SMALL      = max(ROUND_OFF**5,X_SMALL)
real(R8),parameter :: HUGE_REAL    = huge(DUMMY_R8)
end module constants_module
EOF
    my $clean = [split /\n/, <<'EOF'];
module constants_module
implicit none
private dummy_r8,dummy_i4
integer,parameter::r8=selected_real_kind(p=13,r=200)
integer,parameter::i4=selected_int_kind(9)
real(r8),parameter::pi=3.14159265358979323846264338327950288419716939937510582097494e+0_r8
real(r8),parameter::ln_two=6.93147180559945309417232121458176568075500134360255254120680e-1_r8
real(r8),parameter::ln_ten=2.30258509299404568401799145468436420760110148862877297603333e+0_r8
real(r8),parameter::sqrt_two=1.41421356237309504880168872420969807856967187537694807317668e+0_r8
integer(i4),parameter::dummy_i4=1
integer(i4),parameter::huge_int=huge(dummy_i4)
real(r8),parameter::dummy_r8=1._r8
real(r8),parameter::round_off=2._r8*epsilon(dummy_r8)
real(r8),parameter::x_small=2._r8*tiny(dummy_r8)
real(r8),parameter::v_small=max(round_off**5,x_small)
real(r8),parameter::huge_real=huge(dummy_r8)
end module constants_module
EOF
    my $tree  = {
                _class=> '_root',
                constants_module => {
                            _class=> 'module',
                            _name => 'constants_module',
                            _private_default => 0,
                            _privates => [
                                    'dummy_r8',
                                    'dummy_i4',
                                ],
                            _implicit => undef,
                            r8 => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'r8',
                                    _type => 'integer',
                                    _parameter => 1,
                                    _value => 'selected_real_kind(p=13,r=200)',
                                    _private => 0,
                                },
                            i4 => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'i4',
                                    _type => 'integer',
                                    _parameter => 1,
                                    _value => 'selected_int_kind(9)',
                                    _private => 0,
                                },
                            pi => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'pi',
                                    _type => 'real(r8)',
                                    _parameter => 1,
                                    _value => '3.14159265358979323846264338327950288419716939937510582097494e+0_r8',
                                    _private => 0,
                                },
                            ln_two => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'ln_two',
                                    _type => 'real(r8)',
                                    _parameter => 1,
                                    _value => '6.93147180559945309417232121458176568075500134360255254120680e-1_r8',
                                    _private => 0,
                                },
                            ln_ten => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'ln_ten',
                                    _type => 'real(r8)',
                                    _parameter => 1,
                                    _value => '2.30258509299404568401799145468436420760110148862877297603333e+0_r8',
                                    _private => 0,
                                },
                            sqrt_two => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'sqrt_two',
                                    _type => 'real(r8)',
                                    _parameter => 1,
                                    _value => '1.41421356237309504880168872420969807856967187537694807317668e+0_r8',
                                    _private => 0,
                                },
                            dummy_i4 => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'dummy_i4',
                                    _type => 'integer(i4)',
                                    _parameter => 1,
                                    _value => '1',
                                    _private => 1,
                                },
                            huge_int => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'huge_int',
                                    _type => 'integer(i4)',
                                    _parameter => 1,
                                    _value => 'huge(dummy_i4)',
                                    _private => 0,
                                },
                            dummy_r8 => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'dummy_r8',
                                    _type => 'real(r8)',
                                    _parameter => 1,
                                    _value => '1._r8',
                                    _private => 1,
                                },
                            round_off => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'round_off',
                                    _type => 'real(r8)',
                                    _parameter => 1,
                                    _value => '2._r8*epsilon(dummy_r8)',
                                    _private => 0,
                                },
                            x_small => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'x_small',
                                    _type => 'real(r8)',
                                    _parameter => 1,
                                    _value => '2._r8*tiny(dummy_r8)',
                                    _private => 0,
                                },
                            v_small => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'v_small',
                                    _type => 'real(r8)',
                                    _parameter => 1,
                                    _value => 'max(round_off**5,x_small)',
                                    _private => 0,
                                },
                            huge_real => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'huge_real',
                                    _type => 'real(r8)',
                                    _parameter => 1,
                                    _value => 'huge(dummy_r8)',
                                    _private => 0,
                                },
                    },
                };
    $tree->{_modules}{constants_module}         = $tree->{constants_module};
    $tree->{constants_module}{_vars}{r8}        = $tree->{constants_module}{r8};
    $tree->{constants_module}{_vars}{i4}        = $tree->{constants_module}{i4};
    $tree->{constants_module}{_vars}{pi}        = $tree->{constants_module}{pi};
    $tree->{constants_module}{_vars}{ln_two}    = $tree->{constants_module}{ln_two};
    $tree->{constants_module}{_vars}{ln_ten}    = $tree->{constants_module}{ln_ten};
    $tree->{constants_module}{_vars}{sqrt_two}  = $tree->{constants_module}{sqrt_two};
    $tree->{constants_module}{_vars}{dummy_i4}  = $tree->{constants_module}{dummy_i4};
    $tree->{constants_module}{_vars}{huge_int}  = $tree->{constants_module}{huge_int};
    $tree->{constants_module}{_vars}{dummy_r8}  = $tree->{constants_module}{dummy_r8};
    $tree->{constants_module}{_vars}{round_off} = $tree->{constants_module}{round_off};
    $tree->{constants_module}{_vars}{x_small}   = $tree->{constants_module}{x_small};
    $tree->{constants_module}{_vars}{v_small}   = $tree->{constants_module}{v_small};
    $tree->{constants_module}{_vars}{huge_real} = $tree->{constants_module}{huge_real};

    return $version, $name, $code, $clean, $tree;
}
#-------------------------------------------------------------------------------
sub f90_variables {
    my $version = 'f90';
    my ($name) = (caller(0))[3] =~ /:([^:]*$)/;
    my $code = <<'EOF';
subroutine somesub(a,b,c,d)
implicit none
real,intent(in),dimension(:,:),pointer::a
integer,intent(inout),target::b
real,intent(out),dimension(b,b,b)::c
real,intent(out),optional::d
real,save e=1.0
end subroutine somesub
EOF
    my $clean = [split /\n/, <<'EOF'];
subroutine somesub(a,b,c,d)
implicit none
real,intent(in),dimension(:,:),pointer::a
integer,intent(inout),target::b
real,intent(out),dimension(b,b,b)::c
real,intent(out),optional::d
real,save e=1.0
end subroutine somesub
EOF
    my $tree  = {
                _class=> '_root',
                somesub => {
                            _args => [
                                        'a',
                                        'b',
                                        'c',
                                        'd',
                                    ],
                            _mods => [],
                            _class=> 'subroutine',
                            _name => 'somesub',
                            _implicit => undef,
                            a => {
                                    _class=> 'variable',
                                    _dims => [
                                            ':',
                                            ':',
                                        ],
                                    _name => 'a',
                                    _type => 'real',
                                    _intent => 'in',
                                    _pointer => 1,
                                },
                            b => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'b',
                                    _type => 'integer',
                                    _intent => 'inout',
                                    _target => 1,
                                },
                            c => {
                                    _class=> 'variable',
                                    _dims => [
                                            'b',
                                            'b',
                                            'b',
                                        ],
                                    _name => 'c',
                                    _type => 'real',
                                    _intent => 'out',
                                },
                            d => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'd',
                                    _type => 'real',
                                    _intent => 'out',
                                    _optional => 1,
                                },
                            e => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'e',
                                    _type => 'real',
                                    _value => '1.0',
                                    _save => 1,
                                },
                    }
                };
    $tree->{_subs}{somesub}             = $tree->{somesub};
    $tree->{somesub}{_vars}{a}          = $tree->{somesub}{a};
    $tree->{somesub}{_vars}{b}          = $tree->{somesub}{b};
    $tree->{somesub}{_vars}{c}          = $tree->{somesub}{c};
    $tree->{somesub}{_vars}{d}          = $tree->{somesub}{d};
    $tree->{somesub}{_vars}{e}          = $tree->{somesub}{e};
    return $version, $name, $code, $clean, $tree;
}
#-------------------------------------------------------------------------------
sub f90_variations {
    my $version = 'f90';
    my ($name) = (caller(0))[3] =~ /:([^:]*$)/;
    my $code = <<'EOF';
subroutine dimssub(a)
implicit none
real(4) a,b,c,d,e
dimension::a(3,*),b(1,2,3,4,5),c(2:5,7:9)
dimension d(size(b),size(b,1)),e(size(c))
integer(4),dimension(:,:),pointer :: f
integer(4),dimension(:,:),allocatable :: g
end subroutine dimssub

subroutine intentsub(a,bcd,e,f,g,h)
implicit none
integer(4),dimension(:,:),intent(inout)::a,bcd,e
integer f,g,h
intent( out )   ::   f,   g
intent(in) h
f = 1
g = 1
end subroutine intentsub

subroutine optionalsub(a,b,c)
implicit none
integer, optional :: a
integer b,c
optional b
optional :: c
end subroutine optionalsub

subroutine externalsub(a,b,c,d)
implicit none
double precision, external :: a
real(8) b
external b

interface
    double precision function c(x)
        implicit none
        real x
    end function

    subroutine d(a,b)
        implicit none
        integer a,b
    end subroutine
end interface
    
end subroutine externalsub

subroutine typesub
implicit none
type really
    real ::x 
    integer :: y
    logical :: z
    integer*8 a
end type really
type::wrongly
    real x
end type wrongly
type(really) x
type(really),dimension(:),allocatable:: y
dimension x(3)
end subroutine typesub

character(len=*) function charsub(b,f,j,k,l,m)
implicit none
character(len=10)::a
character(len=*)::b
character(10) c
character::d
character*10::e
character*(*)::f
character g*10
character h*10(10)
character i(10)*10
character j*10(*)
character k(*)*10
character l(*)*(*)
character m*(*)(*)
charsub = 'hello'
end function charsub

subroutine paramsub
dimension f(3),h(3)
integer,parameter::va1=1,b_c=(3),d=123
parameter (e=1,f=(/3,3,4/))
parameter(nlist=3)
parameter g=1,h=(/3,3,4/)
end
EOF
    my $clean = [split /\n/, <<'EOF'];
subroutine dimssub(a)
implicit none
real(4) a,b,c,d,e
dimension::a(3,*),b(1,2,3,4,5),c(2:5,7:9)
dimension d(size(b),size(b,1)),e(size(c))
integer(4),dimension(:,:),pointer::f
integer(4),dimension(:,:),allocatable::g
end subroutine dimssub
subroutine intentsub(a,bcd,e,f,g,h)
implicit none
integer(4),dimension(:,:),intent(inout)::a,bcd,e
integer f,g,h
intent(out)::f,g
intent(in) h
f=1
g=1
end subroutine intentsub
subroutine optionalsub(a,b,c)
implicit none
integer,optional::a
integer b,c
optional b
optional::c
end subroutine optionalsub
subroutine externalsub(a,b,c,d)
implicit none
doubleprecision,external::a
real(8) b
external b
interface
doubleprecision function c(x)
implicit none
real x
end function
subroutine d(a,b)
implicit none
integer a,b
end subroutine
end interface
end subroutine externalsub
subroutine typesub
implicit none
type really
real::x
integer::y
logical::z
integer*8 a
end type really
type::wrongly
real x
end type wrongly
type(really) x
type(really),dimension(:),allocatable::y
dimension x(3)
end subroutine typesub
character(len=*) function charsub(b,f,j,k,l,m)
implicit none
character(len=10)::a
character(len=*)::b
character(10) c
character::d
character*10::e
character*(*)::f
character g*10
character h*10(10)
character i(10)*10
character j*10(*)
character k(*)*10
character l(*)*(*)
character m*(*)(*)
charsub={str}'hello'{/str}
end function charsub
subroutine paramsub
dimension f(3),h(3)
integer,parameter::va1=1,b_c=(3),d=123
parameter(e=1,f=(/3,3,4/))
parameter(nlist=3)
parameter g=1,h=(/3,3,4/)
end
EOF
    my $tree  = {
                _class=> '_root',
                dimssub => {
                            _args => [
                                        'a',
                                    ],
                            _mods => [],
                            _class=> 'subroutine',
                            _name => 'dimssub',
                            _implicit => undef,
                            a => {
                                    _class=> 'variable',
                                    _dims => [
                                            '3',
                                            '*',
                                        ],
                                    _name => 'a',
                                    _type => 'real(4)',
                                    _intent => 'inout',
                                },
                            b => {
                                    _class=> 'variable',
                                    _dims => [
                                            '1',
                                            '2',
                                            '3',
                                            '4',
                                            '5',
                                        ],
                                    _name => 'b',
                                    _type => 'real(4)',
                                },
                            c => {
                                    _class=> 'variable',
                                    _dims => [
                                            '2:5',
                                            '7:9',
                                        ],
                                    _name => 'c',
                                    _type => 'real(4)',
                                },
                            d => {
                                    _class=> 'variable',
                                    _dims => [
                                            'size(b)',
                                            'size(b,1)',
                                        ],
                                    _name => 'd',
                                    _type => 'real(4)',
                                },
                            e => {
                                    _class=> 'variable',
                                    _dims => [
                                            'size(c)',
                                        ],
                                    _name => 'e',
                                    _type => 'real(4)',
                                },
                            f => {
                                    _class=> 'variable',
                                    _dims => [
                                            ':',
                                            ':',
                                        ],
                                    _name => 'f',
                                    _type => 'integer(4)',
                                    _pointer => 1,
                                },
                            g => {
                                    _class=> 'variable',
                                    _dims => [
                                            ':',
                                            ':',
                                        ],
                                    _name => 'g',
                                    _type => 'integer(4)',
                                    _allocatable => 1,
                                },
                    },
                intentsub => {
                            _args => [
                                        'a',
                                        'bcd',
                                        'e',
                                        'f',
                                        'g',
                                        'h',
                                    ],
                            _mods => [],
                            _class=> 'subroutine',
                            _name => 'intentsub',
                            _implicit => undef,
                            a => {
                                    _class=> 'variable',
                                    _dims => [
                                            ':',
                                            ':',
                                        ],
                                    _name => 'a',
                                    _type => 'integer(4)',
                                    _intent => 'inout',
                                },
                            bcd => {
                                    _class=> 'variable',
                                    _dims => [
                                            ':',
                                            ':',
                                        ],
                                    _name => 'bcd',
                                    _type => 'integer(4)',
                                    _intent => 'inout',
                                },
                            e => {
                                    _class=> 'variable',
                                    _dims => [
                                            ':',
                                            ':',
                                        ],
                                    _name => 'e',
                                    _type => 'integer(4)',
                                    _intent => 'inout',
                                },
                            f => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'f',
                                    _type => 'integer',
                                    _intent => 'out',
                                },
                            g => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'g',
                                    _type => 'integer',
                                    _intent => 'out',
                                },
                            h => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'h',
                                    _type => 'integer',
                                    _intent => 'in',
                                },
                    },
                optionalsub => {
                            _args => [
                                        'a',
                                        'b',
                                        'c',
                                    ],
                            _mods => [],
                            _class=> 'subroutine',
                            _name => 'optionalsub',
                            _implicit => undef,
                            a => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'a',
                                    _type => 'integer',
                                    _intent => 'inout',
                                    _optional => 1,
                                },
                            b => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'b',
                                    _type => 'integer',
                                    _intent => 'inout',
                                    _optional => 1,
                                },
                            c => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'c',
                                    _type => 'integer',
                                    _intent => 'inout',
                                    _optional => 1,
                                },
                    },
                externalsub => {
                            _args => [
                                        'a',
                                        'b',
                                        'c',
                                        'd',
                                    ],
                            _mods => [],
                            _class=> 'subroutine',
                            _name => 'externalsub',
                            _implicit => undef,
                            _interface => {
                                    _class=> 'interface',
                            },
                            a => {
                                    _args => [],
                                    _mods => [
                                            'doubleprecision',
                                        ],
                                    _class=> 'function',
                                    _dims => [],
                                    _name => 'a',
                                    _intent => 'in',
                                    _external => 1,
                                    a => {
                                            _class=> 'variable',
                                            _dims => [],
                                            _name => 'a',
                                            _type => 'doubleprecision',
                                            _intent => 'out',
                                        },
                                },
                            b => {
                                    _args => [],
                                    _mods => [
                                            'real(8)',
                                        ],
                                    _class=> 'function',
                                    _dims => [],
                                    _name => 'b',
                                    _intent => 'in',
                                    _external => 1,
                                    b => {
                                            _class=> 'variable',
                                            _dims => [],
                                            _name => 'b',
                                            _type => 'real(8)',
                                            _intent => 'out',
                                        },
                                },
                            c => {
                                    _args => [
                                            'x',
                                        ],
                                    _mods => [
                                            'doubleprecision',
                                        ],
                                    _class=> 'function',
                                    _name => 'c',
                                    _implicit => undef,
                                    _intent => 'in',
                                    x => {
                                            _class=> 'variable',
                                            _dims => [],
                                            _name => 'x',
                                            _type => 'real',
                                            _intent => 'inout',
                                        },
                                    c => {
                                            _class=> 'variable',
                                            _dims => [],
                                            _name => 'c',
                                            _type => 'doubleprecision',
                                            _intent => 'out',
                                        },
                                },
                            d => {
                                    _args => [
                                            'a',
                                            'b',
                                        ],
                                    _mods => [],
                                    _class=> 'subroutine',
                                    _name => 'd',
                                    _implicit => undef,
                                    _intent => 'in',
                                    a => {
                                            _class=> 'variable',
                                            _dims => [],
                                            _name => 'a',
                                            _type => 'integer',
                                            _intent => 'inout',
                                        },
                                    b => {
                                            _class=> 'variable',
                                            _dims => [],
                                            _name => 'b',
                                            _type => 'integer',
                                            _intent => 'inout',
                                        },
                                },
                    },
                typesub => {
                            _args => [],
                            _mods => [],
                            _class=> 'subroutine',
                            _name => 'typesub',
                            _implicit => undef,
                            x => {
                                    _class=> 'variable',
                                    _dims => [
                                            3
                                        ],
                                    _name => 'x',
                                    _type => 'really',
                                },
                            y => {
                                    _class=> 'variable',
                                    _dims => [
                                            ':',
                                        ],
                                    _name => 'y',
                                    _type => 'really',
                                    _allocatable => 1,
                                },
                            really => {
                                    _class=> 'type',
                                    _name => 'really',
                                    a => {
                                            _class=> 'variable',
                                            _dims => [],
                                            _name => 'a',
                                            _type => 'integer*8',
                                        },
                                    x => {
                                            _class=> 'variable',
                                            _dims => [],
                                            _name => 'x',
                                            _type => 'real',
                                        },
                                    y => {
                                            _class=> 'variable',
                                            _dims => [],
                                            _name => 'y',
                                            _type => 'integer',
                                        },
                                    z => {
                                            _class=> 'variable',
                                            _dims => [],
                                            _name => 'z',
                                            _type => 'logical',
                                        },
                                },
                            wrongly => {
                                    _class=> 'type',
                                    _name => 'wrongly',
                                    x => {
                                            _class=> 'variable',
                                            _dims => [],
                                            _name => 'x',
                                            _type => 'real',
                                        },
                                },
                    },
                charsub => {
                            _args => [
                                    'b',
                                    'f',
                                    'j',
                                    'k',
                                    'l',
                                    'm',
                                ],
                            _mods => [
                                    'character(len=*)',
                                ],
                            _class=> 'function',
                            _name => 'charsub',
                            _implicit => undef,
                            a => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'a',
                                    _type => 'character',
                                    _len => '10',
                                },
                            b => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'b',
                                    _type => 'character',
                                    _len => '*',
                                    _intent => 'inout',
                                },
                            c => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'c',
                                    _type => 'character',
                                    _len => '10',
                                },
                            d => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'd',
                                    _type => 'character',
                                    _len => 1,
                                },
                            e => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'e',
                                    _type => 'character',
                                    _len => '10',
                                },
                            f => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'f',
                                    _type => 'character',
                                    _len => '*',
                                    _intent => 'inout',
                                },
                            g => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'g',
                                    _type => 'character',
                                    _len => '10',
                                },
                            h => {
                                    _class=> 'variable',
                                    _dims => [
                                            '10',
                                        ],
                                    _name => 'h',
                                    _type => 'character',
                                    _len => '10',
                                },
                            i => {
                                    _class=> 'variable',
                                    _dims => [
                                            '10',
                                        ],
                                    _name => 'i',
                                    _type => 'character',
                                    _len => '10',
                                },
                            j => {
                                    _class=> 'variable',
                                    _dims => [
                                            '*',
                                        ],
                                    _name => 'j',
                                    _type => 'character',
                                    _len => '10',
                                    _intent => 'inout',
                                },
                            k => {
                                    _class=> 'variable',
                                    _dims => [
                                            '*',
                                        ],
                                    _name => 'k',
                                    _type => 'character',
                                    _len => '10',
                                    _intent => 'inout',
                                },
                            l => {
                                    _class=> 'variable',
                                    _dims => [
                                            '*',
                                        ],
                                    _name => 'l',
                                    _type => 'character',
                                    _len => '*',
                                    _intent => 'inout',
                                },
                            m => {
                                    _class=> 'variable',
                                    _dims => [
                                            '*',
                                        ],
                                    _name => 'm',
                                    _type => 'character',
                                    _len => '*',
                                    _intent => 'inout',
                                },
                            charsub => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'charsub',
                                    _type => 'character',
                                    _len => '*',
                                    _intent => 'out',
                                },
                    },
                paramsub => {
                            _args => [],
                            _mods => [],
                            _class=> 'subroutine',
                            _name => 'paramsub',
                            _implicit => {
                                        a => 'real',
                                        b => 'real',
                                        c => 'real',
                                        d => 'real',
                                        e => 'real',
                                        f => 'real',
                                        g => 'real',
                                        h => 'real',
                                        i => 'integer',
                                        j => 'integer',
                                        k => 'integer',
                                        l => 'integer',
                                        m => 'integer',
                                        n => 'integer',
                                        o => 'real',
                                        p => 'real',
                                        q => 'real',
                                        r => 'real',
                                        s => 'real',
                                        t => 'real',
                                        u => 'real',
                                        v => 'real',
                                        w => 'real',
                                        x => 'real',
                                        y => 'real',
                                        z => 'real',
                                    },
                            va1 => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'va1',
                                    _type => 'integer',
                                    _value => '1',
                                    _parameter => 1,
                                },
                            b_c => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'b_c',
                                    _type => 'integer',
                                    _value => '(3)',
                                    _parameter => 1,
                                },
                            d => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'd',
                                    _type => 'integer',
                                    _value => '123',
                                    _parameter => 1,
                                },
                            e => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'e',
                                    _type => 'real',
                                    _value => '1',
                                    _parameter => 1,
                                },
                            f => {
                                    _class=> 'variable',
                                    _dims => [
                                            '3',
                                        ],
                                    _name => 'f',
                                    _type => 'real',
                                    _value => '(/3,3,4/)',
                                    _parameter => 1,
                                },
                            g => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'g',
                                    _type => 'real',
                                    _value => '1',
                                    _parameter => 1,
                                },
                            h => {
                                    _class=> 'variable',
                                    _dims => [
                                            '3',
                                        ],
                                    _name => 'h',
                                    _type => 'real',
                                    _value => '(/3,3,4/)',
                                    _parameter => 1,
                                },
                            nlist => {
                                    _class=> 'variable',
                                    _dims => [],
                                    _name => 'nlist',
                                    _type => 'integer',
                                    _value => '3',
                                    _parameter => 1,
                                },
                    },
                };
    $tree->{_subs}{dimssub}                     = $tree->{dimssub};
    $tree->{_subs}{intentsub}                   = $tree->{intentsub};
    $tree->{_subs}{optionalsub}                 = $tree->{optionalsub};
    $tree->{_subs}{externalsub}                 = $tree->{externalsub};
    $tree->{_subs}{typesub}                     = $tree->{typesub};
    $tree->{_subs}{charsub}                     = $tree->{charsub};
    $tree->{_subs}{paramsub}                    = $tree->{paramsub};
    $tree->{dimssub}{_vars}{a}                  = $tree->{dimssub}{a};
    $tree->{dimssub}{_vars}{b}                  = $tree->{dimssub}{b};
    $tree->{dimssub}{_vars}{c}                  = $tree->{dimssub}{c};
    $tree->{dimssub}{_vars}{d}                  = $tree->{dimssub}{d};
    $tree->{dimssub}{_vars}{e}                  = $tree->{dimssub}{e};
    $tree->{dimssub}{_vars}{f}                  = $tree->{dimssub}{f};
    $tree->{dimssub}{_vars}{g}                  = $tree->{dimssub}{g};
    $tree->{intentsub}{_vars}{a}                = $tree->{intentsub}{a};
    $tree->{intentsub}{_vars}{bcd}              = $tree->{intentsub}{bcd};
    $tree->{intentsub}{_vars}{e}                = $tree->{intentsub}{e};
    $tree->{intentsub}{_vars}{f}                = $tree->{intentsub}{f};
    $tree->{intentsub}{_vars}{g}                = $tree->{intentsub}{g};
    $tree->{intentsub}{_vars}{h}                = $tree->{intentsub}{h};
    $tree->{optionalsub}{_vars}{a}              = $tree->{optionalsub}{a};
    $tree->{optionalsub}{_vars}{b}              = $tree->{optionalsub}{b};
    $tree->{optionalsub}{_vars}{c}              = $tree->{optionalsub}{c};
    $tree->{externalsub}{_subs}{a}              = $tree->{externalsub}{a};
    $tree->{externalsub}{_subs}{b}              = $tree->{externalsub}{b};
    $tree->{externalsub}{_subs}{c}              = $tree->{externalsub}{c};
    $tree->{externalsub}{_subs}{d}              = $tree->{externalsub}{d};
    $tree->{externalsub}{_interface}{c}         = $tree->{externalsub}{c};
    $tree->{externalsub}{_interface}{d}         = $tree->{externalsub}{d};
    $tree->{externalsub}{_interface}{_subs}{c}  = $tree->{externalsub}{c};
    $tree->{externalsub}{_interface}{_subs}{d}  = $tree->{externalsub}{d};
    $tree->{externalsub}{a}{_vars}{a}           = $tree->{externalsub}{a}{a};
    $tree->{externalsub}{b}{_vars}{b}           = $tree->{externalsub}{b}{b};
    $tree->{externalsub}{c}{_vars}{c}           = $tree->{externalsub}{c}{c};
    $tree->{externalsub}{c}{_vars}{x}           = $tree->{externalsub}{c}{x};
    $tree->{externalsub}{d}{_vars}{a}           = $tree->{externalsub}{d}{a};
    $tree->{externalsub}{d}{_vars}{b}           = $tree->{externalsub}{d}{b};
    $tree->{typesub}{_vars}{x}                  = $tree->{typesub}{x};
    $tree->{typesub}{_vars}{y}                  = $tree->{typesub}{y};
    $tree->{typesub}{_type_def}{really}         = $tree->{typesub}{really};
    $tree->{typesub}{_type_def}{wrongly}        = $tree->{typesub}{wrongly};
    $tree->{typesub}{really}{_vars}{a}          = $tree->{typesub}{really}{a};
    $tree->{typesub}{really}{_vars}{x}          = $tree->{typesub}{really}{x};
    $tree->{typesub}{really}{_vars}{y}          = $tree->{typesub}{really}{y};
    $tree->{typesub}{really}{_vars}{z}          = $tree->{typesub}{really}{z};
    $tree->{typesub}{wrongly}{_vars}{x}         = $tree->{typesub}{wrongly}{x};
    $tree->{charsub}{_vars}{a}                  = $tree->{charsub}{a};
    $tree->{charsub}{_vars}{b}                  = $tree->{charsub}{b};
    $tree->{charsub}{_vars}{c}                  = $tree->{charsub}{c};
    $tree->{charsub}{_vars}{d}                  = $tree->{charsub}{d};
    $tree->{charsub}{_vars}{e}                  = $tree->{charsub}{e};
    $tree->{charsub}{_vars}{f}                  = $tree->{charsub}{f};
    $tree->{charsub}{_vars}{g}                  = $tree->{charsub}{g};
    $tree->{charsub}{_vars}{h}                  = $tree->{charsub}{h};
    $tree->{charsub}{_vars}{i}                  = $tree->{charsub}{i};
    $tree->{charsub}{_vars}{j}                  = $tree->{charsub}{j};
    $tree->{charsub}{_vars}{k}                  = $tree->{charsub}{k};
    $tree->{charsub}{_vars}{l}                  = $tree->{charsub}{l};
    $tree->{charsub}{_vars}{m}                  = $tree->{charsub}{m};
    $tree->{charsub}{_vars}{charsub}            = $tree->{charsub}{charsub};
    $tree->{paramsub}{_vars}{va1}               = $tree->{paramsub}{va1};
    $tree->{paramsub}{_vars}{b_c}               = $tree->{paramsub}{b_c};
    $tree->{paramsub}{_vars}{d}                 = $tree->{paramsub}{d};
    $tree->{paramsub}{_vars}{e}                 = $tree->{paramsub}{e};
    $tree->{paramsub}{_vars}{f}                 = $tree->{paramsub}{f};
    $tree->{paramsub}{_vars}{g}                 = $tree->{paramsub}{g};
    $tree->{paramsub}{_vars}{h}                 = $tree->{paramsub}{h};
    $tree->{paramsub}{_vars}{nlist}             = $tree->{paramsub}{nlist};
    
    return $version, $name, $code, $clean, $tree;
}