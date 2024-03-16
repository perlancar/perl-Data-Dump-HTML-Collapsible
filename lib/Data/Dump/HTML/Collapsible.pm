## no critic: Modules::ProhibitAutomaticExportation

package Data::Dump::HTML::Collapsible;

use 5.010001;
use strict;
use warnings;

use Exporter qw(import);
use HTML::Entities qw(encode_entities);
use Scalar::Util qw(looks_like_number blessed reftype refaddr);

# AUTHORITY
# DATE
# DIST
# VERSION

our @EXPORT = qw(dd);
our @EXPORT_OK = qw(dump);

# for when dealing with circular refs
our %_seen_refaddrs;
our %_subscripts;
our @_fixups;

our $OPT_PERL_VERSION = "5.010";
our $OPT_REMOVE_PRAGMAS = 0;
our $OPT_DEPARSE = 1;
our $OPT_STRINGIFY_NUMBERS = 0;

# BEGIN COPY PASTE FROM Data::Dump
my %esc = (
    "\a" => "\\a",
    "\b" => "\\b",
    "\t" => "\\t",
    "\n" => "\\n",
    "\f" => "\\f",
    "\r" => "\\r",
    "\e" => "\\e",
);

# put a string value in double quotes
sub _double_quote {
    local($_) = $_[0];

    # If there are many '"' we might want to use qq() instead
    s/([\\\"\@\$])/\\$1/g;
    return qq("$_") unless /[^\040-\176]/;  # fast exit

    s/([\a\b\t\n\f\r\e])/$esc{$1}/g;

    # no need for 3 digits in escape for these
    s/([\0-\037])(?!\d)/sprintf('\\%o',ord($1))/eg;

    s/([\0-\037\177-\377])/sprintf('\\x%02X',ord($1))/eg;
    s/([^\040-\176])/sprintf('\\x{%X}',ord($1))/eg;

    return qq("$_");
}
# END COPY PASTE FROM Data::Dump

# BEGIN COPY PASTE FROM String::PerlQuote
sub _single_quote {
    local($_) = $_[0];
    s/([\\'])/\\$1/g;
    return qq('$_');
}
# END COPY PASTE FROM String::PerlQuote

sub _dump_code {
    my $code = shift;

    state $deparse = do {
        require B::Deparse;
        B::Deparse->new("-l"); # -i option doesn't have any effect?
    };

    my $res = $deparse->coderef2text($code);

    my ($res_before_first_line, $res_after_first_line) =
        $res =~ /(.+?)^(#line .+)/ms;

    if ($OPT_REMOVE_PRAGMAS) {
        $res_before_first_line = "{";
    } elsif ($OPT_PERL_VERSION < 5.016) {
        # older perls' feature.pm doesn't yet support q{no feature ':all';}
        # so we replace it with q{no feature}.
        $res_before_first_line =~ s/no feature ':all';/no feature;/m;
    }
    $res_after_first_line =~ s/^#line .+//gm;

    $res = "sub" . $res_before_first_line . $res_after_first_line;
    $res =~ s/^\s+//gm;
    $res =~ s/\n+//g;
    $res =~ s/;\}\z/}/;
    $res;
}

sub _quote_key {
    $_[0] =~ /\A-?[A-Za-z_][A-Za-z0-9_]*\z/ ||
        $_[0] =~ /\A-?[1-9][0-9]{0,8}\z/ ? $_[0] : _double_quote($_[0]);
}

sub _dump {
    my ($val, $subscript, $depth) = @_;

    my $ref = ref($val);
    if ($ref eq '') {
        if (!defined($val)) {
            return "undef";
        } elsif (looks_like_number($val) && !$OPT_STRINGIFY_NUMBERS &&
                     # perl does several normalizations to number literal, e.g.
                     # "+1" becomes 1, 0123 is octal literal, etc. make sure we
                     # only leave out quote when the number is not normalized
                     $val eq $val+0 &&
                     # perl also doesn't recognize Inf and NaN as numeric
                     # literals (ref: perldata) so these unquoted literals will
                     # choke under 'use strict "subs"
                     $val !~ /\A-?(?:inf(?:inity)?|nan)\z/i
                 ) {
            return $val;
        } else {
            return _double_quote($val);
        }
    }
    my $refaddr = sprintf("%x", refaddr($val));
    $_subscripts{$refaddr} //= $subscript;
    if ($_seen_refaddrs{$refaddr}++) {
        my $target = "\$var" .
            ($_subscripts{$refaddr} ? "->$_subscripts{$refaddr}" : "");
        push @_fixups, "\$var->$subscript = $target;\n";
        return "<a href=#r$refaddr>"._single_quote($target)."</a>";
    }

    my $class;

    if ($ref eq 'Regexp' || $ref eq 'REGEXP') {
        require Regexp::Stringify;
        return Regexp::Stringify::stringify_regexp(
            regexp=>$val, with_qr=>1, plver=>$OPT_PERL_VERSION);
    }

    if (blessed $val) {
        $class = $ref;
        $ref = reftype($val);
    }

    my $res = "";
    $res .= "<a name=r$refaddr></a>";
    if ($ref eq 'ARRAY') {
        $res .= "<details><summary># ARRAY(0x$refaddr) elems=".(scalar @$val)."</summary>[";
        my $i = 0;
        for (@$val) {
            $res .= ",   # ".("." x $depth)."[".($i-1)."]\n" if $i;
            $res .= _dump($_, "$subscript\[$i]", $depth+1);
            $i++;
        }
        $res .= "]</details>";
    } elsif ($ref eq 'HASH') {
        $res .= "<details><summary># HASH(0x$refaddr) keys=".(scalar keys %$val)."</summary>{";
        my $i = 0;
        for (sort keys %$val) {
            $res .= ",   # ".("." x $depth)."{".($i-1)."}\n" if $i;
            my $k = _quote_key($_);
            my $v = _dump($val->{$_}, "$subscript\{$k}", $depth+1);
            $res .= "$k =&gt; $v";
            $i++;
        }
        $res .= "}</details>";
    } elsif ($ref eq 'SCALAR') {
        if (defined $class) {
            $res .= "do { my \$o="._dump($$val, $subscript)."; \\\$o}";
        } else {
            $res .= "\\"._dump($$val, $subscript);
        }
    } elsif ($ref eq 'REF') {
        $res .= "\\"._dump($$val, $subscript);
    } elsif ($ref eq 'CODE') {
        $res .= $OPT_DEPARSE ? _dump_code($val) : 'sub{"DUMMY"}';
    } else {
        die "Sorry, I can't dump $val (ref=$ref) yet";
    }

    $res = "bless($res,"._double_quote($class).")" if defined($class);
    $res;
}

our $_is_dd;
sub _dd_or_dump {
    local %_seen_refaddrs;
    local %_subscripts;
    local @_fixups;

    my $res;
    if (@_ > 1) {
        die "Currently multiple arguments are not supported, please only pass 1 argument";
        #$res = "(" . join(",\n", map {_dump($_, '', 0)} @_) . ")";
    } else {
        $res = _dump($_[0], '', 0);
    }
    if (@_fixups) {
        $res = "do { my \$var = $res;\n" . join("", @_fixups) . "\$var }";
    }

    $res = "<style>details {  margin-left: 1em; } summary { margin-left: -1em; }</style><pre>$res</pre>";
    if ($_is_dd) {
        say $res;
        return wantarray() || @_ > 1 ? @_ : $_[0];
    } else {
        return $res;
    }
}

sub dd { local $_is_dd=1; _dd_or_dump(@_) } # goto &sub doesn't work with local
sub dump { goto &_dd_or_dump }

1;
# ABSTRACT: Dump Perl data structures as HTML document with collapsible sections

=head1 SYNOPSIS

 use Data::Dump::HTML::Collapsible; # exports dd(), can export dump()
 dd [1, 2, 3];


=head1 DESCRIPTION

This module is a L<Data::Dump> variant that dumps Perl data structure to HTML
document where you can expand and collapse nodes to drill down and roll up your
data. It currently uses the C<< <summary> >> and C<< <details> >> HTML elements.

There are other collapsible HTML dumper modules on CPN (see L</SEE ALSO>). This
module tries to make its text output valid Perl while adding some visual cues
to let human reader track the nodes more easily.


=head1 FUNCTIONS

=head2 dd

=head2 dump


=head1 VARIABLES

=head2 $Data::Dump::HTML::Collapsible::OPT_PERL_VERSION

String, default: 5.010.

Set target Perl version. If you set this to, say C<5.010>, then the dumped code
will keep compatibility with Perl 5.10.0. This is used in the following ways:

=over

=item * passed to L<Regexp::Stringify>

=item * when dumping code references

For example, in perls earlier than 5.016, feature.pm does not understand:

 no feature ':all';

so we replace it with:

 no feature;

=back

=head2 $Data::Dump::HTML::Collapsible::OPT_REMOVE_PRAGMAS

Bool, default: 0.

If set to 1, then pragmas at the start of coderef dump will be removed. Coderef
dump is produced by L<B::Deparse> and is of the form like:

 sub { use feature 'current_sub', 'evalbytes', 'fc', 'say', 'state', 'switch', 'unicode_strings', 'unicode_eval'; $a <=> $b }

If you want to dump short coderefs, the pragmas might be distracting. You can
turn turn on this option which will make the above dump become:

 sub { $a <=> $b }

Note that without the pragmas, the dump might be incorrect.

=head2 $Data::Dump::HTML::Collapsible::::OPT_DEPARSE

Bool, default: 1.

Can be set to 0 to skip deparsing code. Coderefs will be dumped as
C<sub{"DUMMY"}> instead, like in Data::Dump.

=head2 $Data::Dump::HTML::Collapsible::::OPT_STRINGIFY_NUMBERS

Bool, default: 0.

If set to true, will dump numbers as quoted string, e.g. 123 as "123" instead of
123. This might be helpful if you want to compute the hash of or get a canonical
representation of data structure.


=head1 SEE ALSO

Other data structure dumpers to (collapsible) tree: L<Data::HTML::TreeDumper>
(also uses C<< <details> >> and C<< <summary> >> HTML elements, doesn't handle
recursion), L<Data::TreeDumper> (L<Data::TreeDumper::Renderer::DHTML>,
L<Data::TreeDumper::Renderer::GTK>), L<Data::Dumper::GUI>.

Other data structure dumpers that outputs to HTML: L<Data::HTMLDumper>,
L<Data::Dumper::HTML>, L<Data::Format::Pretty::HTML>.

Other data structure dumpers, among others: L<Data::Dumper>, L<Data::Dump>,
L<Data::Dump::Color>, L<Data::Dmp>, L<Data::Printer>.

=cut
