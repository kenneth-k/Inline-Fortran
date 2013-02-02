package Inline::Fortran::Parse;
use strict;
use warnings;
use Carp;

# TODO list
# Detect externals - note that they do not need to be declared in the called
# routine
#===============================================================================
# Package globals
#===============================================================================
our $VERSION = '0.01';
 
#my %type_regexes = (integer =>  'integer(?:(?:\([^()]+\))|(?:\*\d+))?',
#                    real =>     'real(?:(?:\([^()]+\))|(?:\*\d+))?',
#                    double =>   'double\s*precision',
#                    complex =>  'complex(?:(?:\([^()]+\))|(?:\*\d+))?',
#                    dblcmplx => 'double\s*complex',
#                    logical =>  'logical(?:(?:\([^()]+\))|(?:\*\d+))?',
#                    character =>'character(?:(?:\([^()]+\))|(?:\*(?:\d+|\(\*\))))?',
#                    custom =>   'type\([^()]+\)',
#                    );
#my %var_defaults = (_class       => 'variable',
#                    type        => undef,
#                    dims        => [],
#                    external    => 0,
#                    intent      => 'inout',
#                    pointer     => 0,
#                    target      => 0,
#                    optional    => 0,
#                    save        => 0,
#                    common      => undef,
#                    parameter   => 0,
#                    );
sub var_defaults {
    return
        _class       => 'variable',
        _type        => undef,
        _dims        => [],
}
sub implicit_default {
    my %result = ();
    $result{$_} = 'real'    for 'a' .. 'h';
    $result{$_} = 'integer' for 'i' .. 'n';
    $result{$_} = 'real'    for 'o' .. 'z';
    return \%result;
}
#my %implicit_default = ();
#$implicit_default{$_} = 'real'      for 'a' .. 'h';
#$implicit_default{$_} = 'integer'   for 'i' .. 'n';
#$implicit_default{$_} = 'real'      for 'o' .. 'z';

#===============================================================================
# Register Fortran::Parse as an Inline Language Support Module (ILSM)
#===============================================================================
sub register {
    return  {   extends => [qw(Fortran)],
                overrides => [qw(get_parser)],
            }
}

#===============================================================================
# Constructor
#===============================================================================
sub get_parser {
    return bless {}, 'Inline::Fortran::Parse';
}

#===============================================================================
# Main subroutine
#===============================================================================
sub code {
    my($self,$code) = @_;

    $self->{comments} = [];

    # Convert fixed/free format files into statements
    my @codes = $self->_clean($code); 
#-------------------------------------------------------------------------------
    # Parse the tree
    my $tree = $self->_parse(@codes); 
#-------------------------------------------------------------------------------
# Extract information for typing, etc.
    my %types = ();
    $self->{types} =     [map $types{$_}++ == 0 ? $_ : (),
                             map $_->{_type},
                             map values %$_,
                             values %{$self->{subs}}         ];
    $self->{externals} = [];
    foreach (values %{$self->{subs}}) {
        while (my ($key, $value) = each %$_) {
            push @{$self->{externals}}, $key if $value->{_external};
        }
    }
    $self->{overload} = [];
    while (my ($key, $value) = each %{$self->{subs}}) {
        push @{$self->{overload}}, $key if grep $_->{optional}, values %$value
    }
    
    return 1;
}
#-------------------------------------------------------------------------------
sub _clean {
    # Convert fixed/free format files into a universal format
    # 'F', 'f', 'F77' and 'f77' indicate a fixed format file; else free-format

    my($self,$code) = @_;
    
    
    my @line_labels = ();
    if ($self->{data}{language_id} =~ /^f(?:77)?$/i) {
        $code =~ s/^[*!Cc].*?^//mgs;            # Strip comments
        $code =~ s/^(.{0,5})\t/$1.' ' x 6/eg;   # Assume tabs pass continuation
        $code =~ s/\n[ ]{5}[^\s0]//g;           # Append continued lines
        @line_labels = $code =~ /^[ ]*(\d*)/mg; # Capture line numbering
        $code =~ s/^([ ]*\d*)/' ' x length $1/meg;  # Strip line numbering
    } else {
        # Strip comments
        $code =~ s/^((?:(?:\'[^'\n]*\')|(?:\"[^"\n]*\")|[^'"!\n])*)\!.*$/$1/mg;
        # Append continued lines to the previous line
        $code =~ s/^((?:(?:\'[^'\n]*\')|(?:\"[^"\n]*\")|[^'"&\n])*)\&\s*\n/$1/mg;
        # Split multistatement lines on ;
        while ($code =~ s/^((?:(?:\'[^'\n]*\')|(?:\"[^"\n]*\")|[^'";\n])*)\;\s+/$1\n/mg){};

        @line_labels = $code =~ /^[ \t]*(\d*)/mg;   # Capture line numbering, if used
        $code =~ s/^([ \t]*\d*)/' ' x length $1/meg;# Strip line numbering
    }
    
    # Capture and remove string constants
    my $string_count = -1;
    my @strings = ();
    while ($code =~ /^[^'"]*('[^'\n]*'|"[^"\n]*")/) {
        $strings[++$string_count] = $1;
        $code =~ s/^([^'"]*)('[^'\n]*'|"[^"\n]*")/$1\{string$string_count}/;
    }
    
    my @codes = split /\n/, $code;
    pop @line_labels while @line_labels > @codes;
    foreach (@codes) {
        tr/A-Z/a-z/;                                # Fortran is case-insensitive
        s/^\s*//;                                   # leading white space
        s/\s*$//;                                   # trailing white space
        s/(\s)\s+/$1/g;                             # redundant white space
        s/\bdouble\sprecision\b/doubleprecision/g;  # double precision
        s/\bdouble\scomplex\b/doublecomplex/g;      # double complex
        s/\s?([^a-z0-9_])\s?/$1/g;                  # whitespace around punc.
        s/(?<=\))(?=[a-z])/ /                       # put back when )[a-z]
    }

    # Reinsert line numbers
    foreach my $i (0 .. $#line_labels) {
        $codes[$i] = $line_labels[$i].' '.$codes[$i] if length $line_labels[$i];
    }
    
    # Reinsert strings
    foreach (@codes) {
        s/\{string(\d+)\}/\{str\}$strings[$1]\{\/str\}/g;
    }

    # Strip out any now blank lines
    @codes = grep ! /^\s*$/, @codes;

    $self->{_the_code_most_recently_parsed} = \@codes;  # Simplifies debugging.    
    return @codes;
}
#-------------------------------------------------------------------------------
sub _parse{
    # Takes lines of code and extracts a tree structure
    my($self,@codes) = @_;
    
    # Remove and store line labels
    my @line_labels = map {s/^(\d*)\s//; $1} @codes;
    # Remove and store comments
    my @strings = ();
    foreach (@codes) {
        while (/\{str\}(.*?)\{\/str\}/) {
            push @strings, $1;
            s/\{str\}(.*?)\{\/str\}/{string$#strings}/;
        }
    }

    # Initialize parsing structure
    my @stack = (); # Tracks control stack
    my $comments = $self->{comments} ||= []; # Add a comments array if called directly (testing)
    $self->{tree} = {_class => '_root'};
    push @stack, $self->{tree};
    CODE_LOOP: foreach (@codes) {
        
        # Store assignment statements in the body element
        if (/(?<!=|\/|<|>)=(?!=)/) {
            # Might still be an initialization statement or parameterized call
            my $depth = 0;
            my $is_assignment = 0;
            foreach (split //) {
                $depth++ if /\(/;
                $depth-- if /\)/;
                if (!$depth) {
                    next if /[a-z0-9_%]/; # Still possibly a variable name
                    $is_assignment = /=/; # We hit the equals
                    last;
                }
            }
            if ($is_assignment) {
                push @{$stack[-1]{_body}}, $_; # Save it for later
                next CODE_LOOP;
            }
        }

        # End of block, store in local tail and pop an element off the stack
        if (/^end/) {

            if (/^end./) { # Do some checking
                if (not /^end\s?$stack[-1]{_class}/) {
                    # Qualifiers don't match
                    if (defined $stack[-1]{_tail}) {
                        push @{$stack[-1]{_tail}}, $_; # Save it for later
                    } elsif (defined $stack[-1]{_body}) {
                        push @{$stack[-1]{_body}}, $_; # Save it for later
                    } else {
                        push @{$stack[-1]{_head}}, $_; # Save it for later
                    }
                    next CODE_LOOP;
                }
            }
            
            push @{$stack[-1]{_tail}}, $_; # Save it for later
            if (@stack == 0) {
                push @$comments, "Error: unbalanced stack in parse on $_";
                return {_class => '_root'}
            }
            
            # Transfer any accumulated code to the parent
            my @lines = ();
            push @lines, @{$stack[-1]{_head}} if defined $stack[-1]{_head};
            push @lines, @{$stack[-1]{_body}} if defined $stack[-1]{_body};
            push @lines, @{$stack[-1]{_tail}} if defined $stack[-1]{_tail};
            if (defined ($stack[-2]{_tail})) {
                push @{$stack[-2]{_tail}}, @lines;
            } elsif (defined ($stack[-2]{_body})) {
                push @{$stack[-2]{_body}}, @lines;
            } else { # head
                push @{$stack[-2]{_head}}, @lines;
            }

            pop @stack;
            next CODE_LOOP;
        }
        
        # Store executable statements that don't modify the stack
        if (/^(?:    allocate
                    |assign
                    |backspace
                    |call
                    |case
                    |close
                    |cycle
                    |deallocate
                    |endfile
                    |exit
                    |format
                    |^go\s?to
                    |inquire
                    |open
                    |pause
                    |read
                    |return
                    |rewind
                    |stop
                    |write      )\b(?!:)/x)
        {
            push @{$stack[-1]{_body}}, $_; # Save it for later
            next CODE_LOOP;
        }
        
        # Store control structures in the body element, add elements to the stack
        if (/^(?:([a-z0-9_]+)\:)? ( if
                                  |do
                                  |select
                                  |where
                                  |forall )\b/x)
        {
            my $name = $1 || "_none";
            my $type = $2;

            # Check for cases where stack should not be modified
            # Arithmetic IF or single-line IF
            if ($type eq 'if' and not /then$/) {
                push @{$stack[-1]{_body}}, $_; # Save it for later
                next CODE_LOOP;
            }
            # Label-terminated DO loop
            if ($type eq 'do' and /do\s[0-9]/) {
                push @{$stack[-1]{_body}}, $_; # Save it for later
                next CODE_LOOP;
            }
            # Single-line WHERE or FORALL statement
            if ($type eq 'where' or $type eq 'forall') {
                my $depth = 0;
                my $left_mask = 0;
                foreach (split //) {
                    $depth++ if /\(/;
                    if (/\)/) {
                        $depth--;
                        $left_mask++ if $depth == 0;
                        next
                    }
                    if ($left_mask) {
                        # Can only get here if the statement continues past the
                        # original set of parentheses
                        push @{$stack[-1]{_body}}, $_; # Save it for later
                        next CODE_LOOP;
                    }
                }
            }

            # Add a dummy element to the stack for control structures
            my $control = {_name => $name,
                           _class=> 'control',
                           _type => $type,
                           };
            push @stack, $control;
            push @{$stack[-2]{_body}}, (); # Make sure hierarchy is recorded
            push @{$stack[-1]{_body}}, $_; # Save it for later
            next CODE_LOOP;
        }
        
        # A program statement
        if (/^program\b/) {
            my (undef,$name) = split /\s/;
            my $prog = {_name => $name,
                        _class=> 'program',
                        };
            if (exists $stack[-1]{_implicit}) {
                if (defined $stack[-1]{_implicit}) {
                    $prog->{_implicit} = {%{$stack[-1]{_implicit}}};
                } else {
                    $prog->{_implicit} = undef;
                }
            } else {
                $prog->{_implicit} = implicit_default;
            }

            $stack[-1]{$name} = $prog;
            $stack[-1]{_prog}{$name} = $prog;
            push @stack, $prog;

            push @{$stack[-2]{_tail}}, (); # Make sure hierarchy is recorded
            push @{$stack[-1]{_head}}, $_; # Save it for later
            next CODE_LOOP;
        }
        
        if (    /^module\b/ # Module statement
            and (not /^module procedure/
                 or $stack[-1]{_class} ne "interface" # A module named procedure
                 )
            )
        {
            my ($type, $name) = /^(module)\s([a-z0-9_]+)$/;

            my $mod = {_name => $name,
                       _class=> 'module',
                       _private_default => 0,
                       };
            if (exists $stack[-1]{_implicit}) {
                if (defined $stack[-1]{_implicit}) {
                    $mod->{_implicit} = {%{$stack[-1]{_implicit}}};
                } else {
                    $mod->{_implicit} = undef;
                }
            } else {
                $mod->{_implicit} = implicit_default;
            }
            $stack[-1]{_modules}{$name} = $mod;
            $stack[-1]{$name} = $mod;
            push @stack, $mod;

            push @{$stack[-2]{_tail}}, (); # Make sure hierarchy is recorded
            push @{$stack[-1]{_head}}, $_; # Save it for later
            next CODE_LOOP;
        }

        if (/^block\sdata\b/) {# Block Data statement
            my ($name) = /^block\sdata\b\s?(.*)?/;
            $name ||= "_none";

            my $data= {_name => $name,
                       _class => "block data",
                       };
            if (exists $stack[-1]{_implicit}) {
                if (defined $stack[-1]{_implicit}) {
                    $data->{_implicit} = {%{$stack[-1]{_implicit}}};
                } else {
                    $data->{_implicit} = undef;
                }
            } else {
                $data->{_implicit} = implicit_default;
            }

            $stack[-1]{"block data"} = $data;
            push @stack, $data;

            push @{$stack[-2]{_tail}}, (); # Make sure hierarchy is recorded
            push @{$stack[-1]{_head}}, $_; # Save it for later
            next CODE_LOOP;
        }

        if (/^module procedure/) {# module procedure declaration (overloading)
            # Note the degenerate case of a module named procedure is handled in
            # /^module\b/ case
            #module procedure sgamma, dgamma
            my ($names) = /^module procedure\s(.*)$/;
            my @names = split /,/, $names;
            
            foreach my $name (@names) {
                my $sub = {_name => $name,
                           _class => "module procedure",
                           };
                $stack[-1]{_subs}{$name} = $sub;
                $stack[-1]{$name} = $sub;
            }

            push @{$stack[-1]{_tail}}, $_; # Save it for later
            next CODE_LOOP;
        }

        if (/^(?:public|private)/) {
            my ($visibility, $names) = split /\:\:|\s/;
            if ($names) { # Giving a list
                push @{$stack[-1]{"_${visibility}s"}}, split /,/, $names;
            } else { # Setting default
                $stack[-1]{_private_default} = /private/ ? 1 : 0;
            }

            push @{$stack[-1]{_head}}, $_; # Save it for later
            next CODE_LOOP;
        }

        if (/(?<!end\s)\b(?:subroutine|function)\b/) {
            # Subroutine/function
            # Note that this test/capture must be performed before variable resolution
            #real function f(x)
            #elemental function f(x)
            #write(*,*) "I like function a lot!"
            #write(*,*) 'I like function, too!'
            #x = 10.0
            #end function
            my ($mods, $class, $name, $args)
                = /^(.*)\b(subroutine|function)\s([a-z0-9_]+)(?:\(([^()]*)\))?$/;
            my @mods = grep $_, split /\s+/, $mods;
            my @args = grep $_, defined $args ? split /\,/, $args  : ();

            my $sub = {_name => $name,
                       _class=> $class,
                       _mods => \@mods,
                       _args => \@args,
                       };
            if (exists $stack[-1]{_implicit}) {
                if (defined $stack[-1]{_implicit}) {
                    $sub->{_implicit} = {%{$stack[-1]{_implicit}}};
                } else {
                    $sub->{_implicit} = undef;
                }
            } else {
                $sub->{_implicit} = implicit_default;
            }
            $stack[-1]{_subs}{$name} = $sub;
            $stack[-1]{$name} = $sub;
            push @stack, $sub;

            push @{$stack[-2]{_tail}}, (); # Make sure hierarchy is recorded
            push @{$stack[-1]{_head}}, $_; # Save it for later
            next CODE_LOOP;
        }

        if (/^use\b/) { # Use statement
            my ($name, $only, $args) = /use\s([a-z0-9_]+)\,?(only\:)?(.*)$/;
            my @args = split /\,/, $args;
            my $module = {_name => $name,
                          _class=> 'use',
                          _only => $only?1:0,
                          _rename => {map {/=>/?split /=>/:($_,$_)} @args},
                          };
            $stack[-1]{_use}{$name} = $module;
            $stack[-1]{$name} = $module;

            push @{$stack[-1]{_head}}, $_; # Save it for later
            next CODE_LOOP;
        }

        if (/^implicit\b/) {
            # Rules for implicit typing
            #implicit complex*8(z),real(8)(a-b,c-d),integer(e-l)
            if (/implicit\snone/) {
                undef $stack[-1]{_implicit};
                next CODE_LOOP;
            }
            my @terms = /((?:[^,()\s]|\([^()]*\))+)/g;
            shift @terms; # Drop 'implicit'
            foreach my $term (@terms) {
                my ($type,$ranges) = $term =~ /^(.*)\(([^()]*)\)$/;
                foreach my $range (split /\,/, $ranges) {
                    if ($range =~ /\-/) {
                        my ($from,$to) = split /\-/, $range;
                        foreach my $letter ($from .. $to) {
                            $stack[-1]{_implicit}{$letter} = $type;
                        }
                    } else {
                        my $letter = /([a-z])/;
                        $stack[-1]{_implicit}{$letter} = $type;
                    }
                }
            }
            
            push @{$stack[-1]{_head}}, $_; # Save it for later
            next CODE_LOOP;
        }

        if (/^interface\b/) {# Interface statement
            my $interface;
            if (my ($name) = /^interface\s?(.+)$/) {
                $stack[-1]{$name} = {_name => $name,
                                     _class=> "interface",
                                     }
                    if not defined $stack[-1]{$name};
                $interface = $stack[-1]{$name};
                $stack[-1]{_overload}{$name} = $interface;
            } else {
                $stack[-1]{_interface} = {_class => "interface",
                                          }
                    if not defined $stack[-1]{_interface};
                $interface = $stack[-1]{_interface};
            }
            push @stack, $interface;

            push @{$stack[-2]{_head}}, (); # Make sure hierarchy is recorded
            push @{$stack[-1]{_head}}, $_; # Save it for later
            next CODE_LOOP;
        }
        
        if (/^type\b[^(]/) {# Type definition statement
            #type::person
            #integer::i
            #type(place)::j
            #end type person
            my ($name) = /^type[^a-z]*([a-z0-9_]+)$/;
            my $type = {_name => $name,
                        _class => "type",
                        };
            $stack[-1]{_type_def}{$name} = $type;
            $stack[-1]{$name} = $type;
            push @stack, $type;

            push @{$stack[-2]{_head}}, (); # Make sure hierarchy is recorded
            push @{$stack[-1]{_head}}, $_; # Save it for later
            next CODE_LOOP;
        }
        
        if (    /^(?: integer
                     |real
                     |doubleprecision
                     |complex
                     |doublecomplex
                     |logical         )\b/x
            )
        { # Non-character built-in type declaration
            # integer*8 x
            # integer(8)::x
            # integer::x
            # integer x
            # integer(I4),dimension(:,:),intent(inout)::a,bcd,e
            # integer a(10),bcd(9,*),e
            my ($mods, $names) = split /\:\:|\s/;
            my @mods = _split_on_lvl0_commas($mods);
            my @names = _split_on_lvl0_commas($names);
            my $type = shift @mods;

            foreach my $name (@names) {
                ($name,my $value) = split /\=/, $name, 2;
                my @dims = _split_on_lvl0_commas($name =~/\((.*)\)/);
                #my @dims = $name =~ /(?:\(|,)([^(),]+)/g;   # Follows ( or ,
                $name =~ s/\(.*$//;                         # Strip ( to the end

                if (not defined $stack[-1]{$name}) {
                    $stack[-1]{$name} = {var_defaults,
                                         _name => $name,
                                         };
                    $stack[-1]{_vars}{$name} = $stack[-1]{$name};
                }
                my $var = $stack[-1]{$name};

                $var->{_type} = $type;
                $var->{_dims} = \@dims if @dims;
                $var->{_value} = $value if defined $value;
                # Set visibility if this is a module variable
                foreach my $mod (@mods) {
                    if ($mod =~ /^dimension/) {
                        my @dims = _split_on_lvl0_commas($mod =~/\((.*)\)/);
                        $var->{_dims} = \@dims;
                    } elsif ($mod =~ /^intent/) {
                        my ($intent) = $mod =~ /\(([^()]+)\)/;
                        $var->{_intent} = $intent;
                    } elsif ($mod =~ /^public/) {
                        $var->{_private} = 0;
                    } else {
                        $var->{"_$mod"} = 1;
                    }
                }
                if (@dims) {
                    $var->{_dims} = \@dims;
                }
            }

            push @{$stack[-1]{_head}}, $_; # Save it for later
            next CODE_LOOP;
        }

        if (/^character\b/) {# Character declaration
            # Character declaration
            #character(len=10)::var
            #character(len=*)::var
            #character(10) var
            #character::var
            #character*10::var
            #character*(*)::var
            #character var*10
            #character var*10(10)
            #character var(10)*10
            #character var*10(*)
            #character var(*)*10
            #character var(*)*(*)
            #character var*(*)(*)
            #character(len=2,kind=1) :: var
            #character(kind=1,len=2) :: var
            #character(2,kind=1) :: var
            #character(len=2,1) :: var
            #character(2,1) :: var
            #character(kind=1) :: var
            #character(input) :: var

            my ($mods, $names) = split /\:\:|\s/;
            my @mods = _split_on_lvl0_commas($mods);
            my @names = _split_on_lvl0_commas($names);
            my $type = shift @mods;
            my ($outer_len) = $type =~ /^[^(*]*\*([0-9]*)$/;
            my $kind;
            if ($type =~ /\(/) {
                my ($parens) = $type =~ /^[^(]*\((.*)\)/;
                my @terms = _split_on_lvl0_commas($parens);
                if ($terms[0] =~ s/^kind=//) {
                    $kind = $terms[0];
                    if (defined $terms[1]) {
                        if ($terms[1] =~ s/^len=//) {
                            $outer_len = $terms[1];
                        }
                    }
                } else {
                    $terms[0] =~ s/^len=//;
                    $outer_len = $terms[0];
                    if (defined $terms[1]) {
                        $terms[1] =~ s/^kind=//;
                        $kind = $terms[1];
                    }
                }
            }
            $outer_len ||= 1;
            $type =~ s/[^a-z].*//;

            foreach my $name (@names) {
                # Check for initial value
                my ($value) = $name =~ /=(.*)$/;
                $name =~ s/=.*$//;
                my @dims = _split_on_lvl0_commas($names =~ /(?<!\*)\((.*)(?<!\*\(\*)\)/);
                my ($len) = grep $_, $name =~ /  ^[^()*]*\*([^()*]+)
                                                |^[^()*]*\*\((\*)\)
                                                |        \*([^()*]+)$
                                                |        \*\((\*)\)$    /x;
                
                $len ||= $outer_len;
                $name =~ s/(?:\(|\*).*$//;                  # Strip (,* to end

                if (not defined $stack[-1]{$name}) {
                    $stack[-1]{$name} = {var_defaults,
                                         _name => $name,
                                         _len => $len,
                                         };
                    $stack[-1]{$name}{_kind} = $kind if defined $kind;
                    $stack[-1]{_vars}{$name} = $stack[-1]{$name};
                }
                my $var = $stack[-1]{$name};
                
                $var->{_value} = $value if defined $value;
                $var->{_type} = $type;
                $var->{_dims} = \@dims if @dims;
                foreach my $mod (@mods) {
                    if ($mod =~ /^dimension/) {
                        my @dims = _split_on_lvl0_commas($mod =~/\((.*)\)/);
                        $var->{_dims} = \@dims;
                    } elsif ($mod =~ /^intent/) {
                        my ($intent) = $mod =~ /\(([^()]+)\)/;
                        $var->{_intent} = $intent;
                    } elsif ($mod =~ /^public/) {
                        $var->{_private} = 0;
                    } else {
                        $var->{"_$mod"} = 1;
                    }
                }
                if (@dims) {
                    $var->{_dims} = \@dims;
                }
            }

            push @{$stack[-1]{_head}}, $_; # Save it for later
            next CODE_LOOP;
        }
        
        if (/^type\(/) {# Custom type declaration
            #type::person
            #integer::i
            #type(place)::j
            #end type person
            my ($mods, $names) = split /\:\:|\s/;
            my @mods = _split_on_lvl0_commas($mods);
            my @names = _split_on_lvl0_commas($names);
            #my @names = $names =~ /((?:[^(),]|\([^()]*\))+)/g;
            my $type = shift @mods;
            $type =~ s/^type\(//;
            $type =~ s/\)//;

            foreach my $name (@names) {
                my ($value) = $name =~ /=(.*)$/;
                $name =~ s/=.*$//;               
                my @dims = _split_on_lvl0_commas($name =~/\((.*)\)/);
                $name =~ s/\(.*$//;                         # Strip ( to the end

                if (not defined $stack[-1]{$name}) {
                    $stack[-1]{$name} = {var_defaults,
                                         _name => $name,
                                         };
                    $stack[-1]{_vars}{$name} = $stack[-1]{$name};
                }
                my $var = $stack[-1]{$name};

                $var->{_value} = $value if defined $value;
                $var->{_type} = $type;
                $var->{_dims} = \@dims if @dims;
                foreach my $mod (@mods) {
                    if ($mod =~ /^dimension/) {
                        my @dims = _split_on_lvl0_commas($mod =~/\((.*)\)/);
                        $var->{_dims} = \@dims;
                    } elsif ($mod =~ /^intent/) {
                        my ($intent) = $mod =~ /\(([^()]+)\)/;
                        $var->{_intent} = $intent;
                    } elsif ($mod =~ /^public/) {
                        $var->{_private} = 0;
                    } else {
                        $var->{"_$mod"} = 1;
                    }
                }
                if (@dims) {
                    $var->{_dims} = \@dims;
                }
            }

            push @{$stack[-1]{_head}}, $_; # Save it for later
            next CODE_LOOP;
        }
        
        if (/^dimension\b/) {# Dimension statement
            #dimension::var1(10),var_two(1,2,3,4,5),var3(2:5,7:9)
            #dimension var1(10)
            my ($terms) = /(?:\s|\:\:)(.*)$/;
            my @terms = _split_on_lvl0_commas($terms);
            foreach my $term (@terms) {
                my ($name, $dims) = $term =~ /^(.*?)\((.*)\)$/;
                
                if (not defined $stack[-1]{$name}) {
                    $stack[-1]{$name} = {var_defaults,
                                         _name => $name,
                                         };
                    $stack[-1]{_vars}{$name} = $stack[-1]{$name};
                }
                my $var = $stack[-1]{$name};
                $var->{_dims} = [_split_on_lvl0_commas($dims)];
            }
            
            push @{$stack[-1]{_head}}, $_; # Save it for later
            next CODE_LOOP;
        }

        if (/^intent\b/) {# Intent statement
            #intent(out)::var1,var_two,var3
            #intent(in) var1
            my ($mod,$names) = split /\s|\:\:/;
            my ($intent) = $mod =~ /\(([a-z]*)\)/;
            my @names = split /\,/, $names;
            foreach my $name (@names) {
                if (not defined $stack[-1]{$name}) {
                    $stack[-1]{$name} = {var_defaults,
                                         _name => $name,
                                         };
                    $stack[-1]{_vars}{$name} = $stack[-1]{$name};
                }
                my $var = $stack[-1]{$name};
                $var->{_intent} = $intent;
            }
            
            push @{$stack[-1]{_head}}, $_; # Save it for later
            next CODE_LOOP;
        }

        if (/^parameter\b/) {# Parameter statement
            #parameter::var1=1,var_two=(3),var3=123
            #parameter (var1=1,var2=(/3,3,4/))
            #parameter(nlist=3)
            #parameter var1=1,var2=(/3,3,4/)
            my ($names) = grep $_, /(?:\s|\:\:|(?=\())(?:([^(].*)|\((.*)\))$/;
            my @names = split /,(?=[^,]*\=)/, $names;
            foreach (@names) {
                my ($name, $value) = split /\=/;
                if (not defined $stack[-1]{$name}) {
                    $stack[-1]{$name} = {var_defaults,
                                         _name => $name,
                                         };
                    $stack[-1]{_vars}{$name} = $stack[-1]{$name};
                }
                my $var = $stack[-1]{$name};
                $var->{_parameter} = 1;
                $var->{_value} = $value;
            }
            
            push @{$stack[-1]{_head}}, $_; # Save it for later
            next CODE_LOOP;
        }

        if (/^common\b/) {# Common block declaration statement
            #common/mccons/vers,prec(-3:3,-3:3),kerr,leni,lenr
            my ($common,$names) = /^common(?:\/([a-z0-9]*)\/|\s)?(.*)$/;
            my @names = _split_on_lvl0_commas($names);
            $common = '_anon' if not defined $common;
            $stack[-1]{_commons}{$common} = {_name => $common,
                                            _class=> 'common',
                                            _vars => \@names
                                           };

            push @{$stack[-1]{_head}}, $_; # Save it for later
            next CODE_LOOP;
        }

        if (/^data\b/) {# Data statement
            #data var1/1,2,3/,var2/4,5,6/,var3,var4,var5/3*7/
            s/^data(?:\:\:|\s)//;
            my @groups = split /(?<=\/),/;
            push @{$stack[-1]{_data}}, @groups;
            foreach my $group (@groups) {
                my ($names) = split /\//, $group;
                my @names = _split_on_lvl0_commas($names);
                s/\(.*\)// foreach @names;
                
                foreach my $name (@names) {
                    if (not defined $stack[-1]{$name}) {
                        $stack[-1]{$name} = {var_defaults,
                                             _name => $name,
                                             };
                        $stack[-1]{_vars}{$name} = $stack[-1]{$name};
                    }
                    my $var = $stack[-1]{$name};
                    $var->{_data} = $group;
                }
            }

            push @{$stack[-1]{_head}}, $_; # Save it for later
            next CODE_LOOP;
        }

        if (    /^(?: pointer
                     |target
                     |external
                     |optional
                     |save      )\b/x
            )
        {# Pointer/Target/External/Optional/Save statement
            my ($mod,$names) = split /\s|\:\:/;
            my @names = _split_on_lvl0_commas($names);
            foreach my $name (@names) {
                if (not defined $stack[-1]{$name}) {
                    $stack[-1]{$name} = {var_defaults,
                                         _name => $name,
                                         };
                    $stack[-1]{_vars}{$name} = $stack[-1]{$name};
                }
                my $var = $stack[-1]{$name};
                $var->{"_$mod"} = 1;
            }
            
            push @{$stack[-1]{_head}}, $_; # Save it for later
            next CODE_LOOP;
        }
        
        if (/^contains$/) {
            push @{$stack[-1]{_tail}}, $_; # Save it for later
            next CODE_LOOP;
        }
        
        # If we got here, an unrecognized line was encountered, and so we
        # should fail
        push @$comments, "Error: parse failure on $_";
        return {_class => '_root'};
    }
    if ($#stack) {
        push @$comments, "Error: unbalanced stack after parse $#stack";
        return {_class => '_root'};
    }
    
## FIXME handle error
    #---------------------------------------------------------------------------
    # Crawl the tree, completing mappings (defined implicits, commons...)
    _define_external_from_interface($self->{tree})
        or push @$comments, "Error: defining externals from interfaces failed";
    _move_external_to_subs($self->{tree})
        or push @$comments, "Error: redefining externals as subs failed";
    _define_arguments($self->{tree})
        or push @$comments, "Error: mapping argument lists failed";
    _define_function_returns($self->{tree})
        or push @$comments, "Error: mapping function returns failed";
    _define_commons($self->{tree})
        or push @$comments, "Error: mapping common block variables failed";
    _set_module_visibility($self->{tree})
        or push @$comments, "Error: setting module visibility failed";
    _copy_module_procedure($self->{tree})
        or push @$comments, "Error: defining implicit typing failed";
    _define_implicits($self->{tree})
        or push @$comments, "Error: defining implicit typing failed";
    _sub_in_strings($self->{tree},\@strings)
        or push @$comments, "Error: resubstituting strings failed";

    return $self->{tree};
    
}
#-------------------------------------------------------------------------------
sub _split_on_lvl0_commas{
    # Splits the passed string on commas not surrounded by parentheses
    my ($input) = @_;
    return () if not defined $input;
    my @array = (q{});
    my $depth = 0;
    foreach(split //, $input){
        $depth++ if /\(/;
        $depth-- if /\)/;
        if (/,/ and not $depth){
            push @array, q{};
        } else {
            $array[-1].=$_;
        }
    }
    return @array;
}
#-------------------------------------------------------------------------------
sub _define_function_returns{
    # Creates the return variable for a function if necessary and formats it
    # appropriately (intent(out), type if known...)
    my ($node) = @_;
    my $success = 1;
    if ($node->{_class} eq 'function') {
        my $name = $node->{_name};
        my $var;
        if (not defined $node->{$name}) {
            # Not previously declared
            $var = {
                        var_defaults,
                        _name => $name,
                    };
            $node->{$name} = $var;
            $node->{_vars}{$name} = $var;
            
        } else {
            $var = $node->{$name};
        }

        # Check mod list for type declaration
        my ($type) = grep /  ^integer
                            |^real
                            |^doubleprecision
                            |^complex
                            |^doublecomplex
                            |^logical
                            |^character
                            |^type              /x, @{$node->{_mods}};
        if (defined $type) {
            $var->{_type} = $type;
            if ($type =~ /^character/) {
                my ($len) =
                    grep $_,
                        $type =~ /\*(?:\((\*)\)|([^()*\s]*))|\((?:len=)?([^()]*)\)/;
                $var->{_len} = $len || 1;
                $var->{_type} = "character";
            }
        }

        $node->{$name}{_intent} = 'out'; # All functions are pure returns
    }
    
    # And recurse
    while (my($key,$value) = each %$node) {
        if ($key !~ /^_/ and ref $value eq 'HASH') {
            $success &&= _define_function_returns($value);
        }
    }
    return $success;
}
#-------------------------------------------------------------------------------
sub _define_arguments{
    # Extracts variables from the argument list
    my ($node) = @_;
    my $success = 1;

    if (defined $node->{_args}) {
        foreach my $name (@{$node->{_args}}) {
            my $var;
            if (not defined $node->{$name}) {
                # Not previously declared
                $var = {
                            var_defaults,
                            _name => $name,
                        };
                $node->{$name} = $var;
                $node->{_vars}{$name} = $var;
            } else {
                $var = $node->{$name};
            }
            if ($var->{_class} eq 'variable') {
                $var->{_intent} = 'inout' if not defined $var->{_intent};
            } else {
                $var->{_intent} = 'in';
            }
        }
    }

    # And recurse
    while (my($key,$value) = each %$node) {
        if ($key !~ /^_/ and ref $value eq 'HASH') {
            $success &&= _define_arguments($value);
        }
    }
    return $success;
}
#-------------------------------------------------------------------------------
sub _set_module_visibility{
    # Applies public/private labels to module objects
    my ($node) = @_;
    my $success = 1;

    if ($node->{_class} eq 'module') {
        if (defined $node->{_publics}) {
            foreach my $name (@{$node->{_publics}}) {
                if (defined $node->{$name}) {
                    $node->{$name}{_private} = 0;
                }
            }
        }
        if (defined $node->{_privates}) {
            foreach my $name (@{$node->{_privates}}) {
                if (defined $node->{$name}) {
                    $node->{$name}{_private} = 1;
                }
            }
        }
        foreach my $name (keys %$node) {
            next if $name =~ /^_/;
            next if exists $node->{$name}{_private};
            $node->{$name}{_private} = $node->{_private_default};
        }
    }

    # And recurse
    while (my($key,$value) = each %$node) {
        if ($key !~ /^_/ and ref $value eq 'HASH') {
            $success &&= _set_module_visibility($value);
        }
    }
    return $success;
}
#-------------------------------------------------------------------------------
sub _copy_module_procedure{
    # Overwrites module procedure elements w/ actual subroutine/function
    my ($node) = @_;
    my $success = 1;

    if ($node->{_class} eq 'module') {
        if (defined $node->{_overload}) {
            foreach my $overload (keys %{$node->{_overload}}) {
                foreach my $name (keys %{$node->{_overload}{$overload}}) {
                    next if $name =~ /^_/;
                    if ($node->{_overload}{$overload}{$name}{_class} eq 'module procedure') {
                        $node->{_overload}{$overload}{$name} = $node->{$name};
                    }
                }
            }
        }
    }

    # And recurse
    while (my($key,$value) = each %$node) {
        if ($key !~ /^_/ and ref $value eq 'HASH') {
            $success &&= _copy_module_procedure($value);
        }
    }
    return $success;
}
#-------------------------------------------------------------------------------
sub _define_commons{
    # Extracts variables from the common block
    my ($node) = @_;
    my $success = 1;

    if (defined $node->{_commons}) {
        foreach my $common (values %{$node->{_commons}}) {
            foreach my $name (@{$common->{_vars}}) {
                my $var;
                if (not defined $node->{$name}) {
                    # Not previously declared
                    $var = {
                                var_defaults,
                                _name => $name,
                            };
                    $node->{$name} = $var;
                    $node->{_vars}{$name} = $var;
                    
                } else {
                    $var = $node->{$name};
                }
                
                $var->{_common} = $common->{_name};
            }
        }
    }

    # And recurse
    while (my($key,$value) = each %$node) {
        if ($key !~ /^_/ and ref $value eq 'HASH') {
            $success &&= _define_commons($value);
        }
    }
    return $success;
}
#-------------------------------------------------------------------------------
sub _define_external_from_interface{
    # Extracts external functions and subroutines from interface statements
    my ($node) = @_;
    my $success = 1;
    if (defined $node->{_interface}) {
        foreach my $ext (values %{$node->{_interface}}) {
            next unless ref $ext eq 'HASH' and defined $ext->{_name};
            my $name = $ext->{_name};
            $node->{$name} = $ext;
            $node->{_subs}{$name} = $ext;
        }
    }    
    
    # And recurse
    while (my($key,$value) = each %$node) {
        if ($key !~ /^_/ and ref $value eq 'HASH') {
            $success &&= _define_external_from_interface($value);
        }
    }
    return $success;
}
#-------------------------------------------------------------------------------
sub _move_external_to_subs{
    # Extracts external functions and subroutines from interface statements
    my ($node) = @_;
    my $success = 1;
    if (defined $node->{_vars}) {
        foreach my $var (values %{$node->{_vars}}) {
            my $name = $var->{_name};
            if (defined $var->{_external}) {
                # Move it to the subs and delete it from vars
                $node->{_subs}{$name} = $var;
                delete $node->{_vars}{$name};
                delete $node->{_vars} if not keys %{$node->{_vars}};
                
                if (defined $var->{_type}) {
                    # If it was explicitly typed, it must be a function
                    $var->{_class} = 'function';
                    $var->{_mods} = [delete $var->{_type}];
                    $var->{_args} = [];
                } else {
                    # Default to assuming it should be a subroutine
                    $var->{_class} = 'subroutine';
                    $var->{_mods} = [];
                    $var->{_args} = [];
                }
            }
        }
    }    
    
    # And recurse
    while (my($key,$value) = each %$node) {
        if ($key !~ /^_/ and ref $value eq 'HASH') {
            $success &&= _move_external_to_subs($value);
        }
    }
    return $success;
}
#-------------------------------------------------------------------------------
sub _define_implicits{
    # Substitutes types in forimplicitly-typed variables
    my ($node) = @_;
    my $success = 1;
    if (defined $node->{_vars} and defined $node->{_implicit}) {
        my %implicit = %{$node->{_implicit}};
        foreach my $var (values %{$node->{_vars}}) {
            my $type;
            if (defined $var->{_type}) {
                next;
            } else {
                $type = $implicit{substr $var->{_name},0,1};
            }
            if (not defined $type) {
                $success = 0;
                next;
            }
            $var->{_type} = $type;
            if ($type =~ /^character/) {
                my ($len) =
                    grep $_,
                        $type =~ /\*(?:\((\*)\)|([^()*\s]*))|\((?:len=)?([^()]*)\)/;
                $var->{_len} = $len || 1;
            }
            
        }
    }
    
    # And recurse
    while (my($key,$value) = each %$node) {
        if ($key !~ /^_/ and ref $value eq 'HASH') {
            $success &&= _define_implicits($value);
        }
    }
    return $success;
}
#-------------------------------------------------------------------------------
sub _sub_in_strings{
    # Substitute string values back into assignment statements
    my ($node, $strings_ref) = @_;
    my $success = 1;
    if ($node->{_class} eq 'variable') {
        foreach (qw(_value _len _kind)) {
            if (defined $node->{$_}) {
                $node->{$_} =~ s/{string(\d+)}/$strings_ref->[$1]/g;
            }
        }
    }
    # Substitute strings back into _head, _body and _tail elements
    foreach (qw(_head _body _tail)) {
        if (defined $node->{$_}) {
            foreach (@{$node->{$_}}) {
                s/{string(\d+)}/$strings_ref->[$1]/g
            }
        }
    }
    
    # And recurse
    while (my($key,$value) = each %$node) {
        if ($key !~ /^_/ and ref $value eq 'HASH') {
            $success &&= _sub_in_strings($value, $strings_ref);
        }
    }
    return $success;
}
#-------------------------------------------------------------------------------
1;
__DATA__

=head1 NAME

Inline::Fortran::Parse - Parses elements of Fortran code

=head1 SYNOPSIS

    use Inline Fortran => DATA =>
               USING => Parse;

=head1 DESCRIPTION

This modules is designed to read Fortran code and return a tree containing the
subroutine, function, module and variable structure

=head1 NOTE REGARDING SYNTAX

The syntactic forms used for development of this parser were based on those
presented in "Fortran 90/95 for Scientists and Engineers" by Stephen J. Chapman
(c) 1998 as well as trial and error exploration of acceptable syntax to some
common compilers (Sun, Intel and Gnu).  As such, no guarentee can be made
regarding the rigor.  If a user has a piece of that successfully compiles and
runs but fails to be parsed correctly, please file a detailed bug report.

=head1 AUTHOR

Kenneth Kroenlein <kennethk@cpan.org>

=head1 COPYRIGHT

This software is released under the terms of the Artistic license.

Copyright (c) 2009. Kenneth Kroenlein. All rights reserved.

See http://www.perl.com/perl/misc/Artistic.html

=cut