#!/usr/bin/perl
use Marpa::R2;

my $marpa = <<'EOF';
:default ::= action => [values]

statement ::= name action => do_run
           || window area action => do_move
            | area action => do_move1
            | empty action => do_nothing
            | window action => do_select
            | sexp action => do_sexp
            | '/' digits action => do_layout_switch
            | '/' action => do_layout_switch
           || statement ( ';' ) statement action => do_progn assoc => left
           || name ( ':' ) statement action => do_define

area ::= digits action => area_digit
       | area ( '+' ) area action => area_plus
       | area ( '-' ) area action => area_minus

name ::= letter letters action => do_name

letters ::= letter + action => do_letters

window ::= letter action => window_letter
       | ucletter action => window_baptize
       | '@' digits action => window_digits
       | '@' digits ucletter action => window_baptize_2

empty ::=

sexp ::= '(' balanceds ')' action => do_sexp
balanceds ::= balanced * action => do_sexp
balanced ::= sexp action => ::first | balancedchar action => ::first | digits action => ::first

balancedchar ~ [^()0-9]

digits ~ digit +
digit ~ [\d]
letter ~ [a-z]
ucletter ~ [A-Z]
EOF

my $grammar = Marpa::R2::Scanless::G->new({source => \$marpa});

my %statements;

sub do_layout_switch {
    my (undef, $slash) = @_;

    return "(cli-layout-switch)";
}

sub do_sexp {
    my (undef, @s) = @_;

    return join("", @s);
}

sub do_name {
    my (undef, $letter, $letters) = @_;

    return $letter . $letters;
}

sub do_letters {
    my (undef, @letters) = @_;

    return join("", @letters);
}

sub do_repeat {
    my (undef) = @_;

    push @history, $history[1];

    return $history[0];
}

sub do_run {
    my (undef, $name) = @_;

    push @history, $statements{$name};

    return $statements{$name};
}

sub do_define {
    my (undef, $name, $statement) = @_;

    $statements{$name} = $statement;

    return "()";
}

sub do_select {
    my (undef, $window) = @_;

    return "(popup-window ${window})";
}

sub do_move {
    my (undef, $window, $area) = @_;

    return "(progn (cli-move-window ${window} '${area} (pointer-head)) " . do_select(undef, $window) . ")";
}

sub do_move1 {
    my (undef, $area) = @_;

    return do_move(undef, "(cli-current-window)", $area);
}

sub do_progn {
    my (undef, $a, $b) = @_;

    return "(progn ${a} ${b})";
}

sub do_nothing {
}

sub area_digit {
    my (undef, $digit) = @_;

    return "(get-area ${digit})";
}

sub area_plus {
    my (undef, $a1, $a2) = @_;

    return "(area-union ${a1} ${a2})";
}

sub area_minus {
    my (undef, $a1, $a2) = @_;

    return "(area-minus ${a1} ${a2})";
}

sub window_letter {
    my (undef, $l) = @_;

    return "(window-by-cli-tag '${l})";
}

sub window_digits {
    my (undef, $at, $id) = @_;

    return "(nth ${id} (stacking-order))";
}

sub window_baptize {
    my (undef, $l) = @_;
    $l = lc($l);

    return "(let ((w (cli-current-window))) (window-put w 'cli-tag '${l}) w)";
}

sub window_baptize_2 {
    my (undef, $at, $id, $l) = @_;
    $l = lc($l);

    return "(let ((w (nth ${id} (stacking-order)))) (window-put w 'cli-tag '${l}) w)";
}

use IPC::Run;

sub run {
    my ($cmd, $silent) = (@_);
    my $in = "";
    my $out;

    IPC::Run::run(["/usr/bin/sawfish-client", "-e", "$cmd"], \$in, \$out);
    warn "$cmd => $out"
        unless $silent;

    return $out;
}

my $winid = hex shift;
my $cycle = undef;
my @cycle;
my $cycle_index = 0;

sub fire {
    my ($s) = @_;
    if (defined($s)) {
        my $command = $grammar->parse(\$s, 'main');

        run("(setq focus-ignore-pointer-events t)");
        run($$command);
        if (defined $cycle) {
            $cycle = undef;
            run("(require 'sawfish.wm.util.display-wininfo)");
            run("(display-wininfo (window-by-id nil))");
            run("(setq focus-ignore-pointer-events nil)");
            run("(popup-window (window-by-id $cycle[0]))");
        }
        run("(lower-window (window-by-id ${winid}))");
        run("(setq focus-ignore-pointer-events nil)");
    }
}

sub cycle {
    my ($winid, $step) = @_;

    unless (defined($cycle)) {
        $cycle = run("(mapcar window-id (filter-windows (lambda (w) t)))");
        $cycle =~ s/^\(//;
        $cycle =~ s/\)$//;
        @cycle = split / /, $cycle;
        run("(setq focus-ignore-pointer-events t)");
    }
    run("(lower-window (window-by-id $cycle[${cycle_index}]))");
    $cycle_index += $step;
    $cycle_index = 0 if $cycle_index == scalar(@cycle);
    $cycle_index = $#cycle if $cycle_index == -1;
    run("(popup-window (window-by-id $cycle[${cycle_index}]))");
    run("(require 'sawfish.wm.util.display-wininfo)");
    run("(display-wininfo (window-by-id $cycle[${cycle_index}]))");
    run("(popup-window (window-by-id $winid))");
    my $list = run("(window-list)");
    $list =~ s/\\012/\n/msg;
    $list =~ s/\A\"//;
    $list =~ s/\"\Z//;
    warn $list;
}

sub preview {
    my ($s, $winid) = @_;

    while ($s =~ s/^ //) {
        cycle($winid, 1);
    }
    while ($s =~ s/^\177//) {
        cycle($winid, -1);
    }
    # warn "command $s";
    return $s;
}

my $in;
my $s = undef;

fire("ee:l;r;e");
fire("ea:a;r;e");
fire("(window-put (match-best (get-area 4) (windows-by-regexp \"^Emacs\")) 'cli-tag 'l)");
fire("l4");
fire("(window-put (match-best (get-area 6) (windows-by-regexp \"^Emacs\")) 'cli-tag 'r)");
fire("r6");
fire("(window-put (match-best (get-area 5) (windows-by-regexp \"^Emacs\")) 'cli-tag 'e)");
fire("e5");
fire("(window-put (match-best (get-area 6) (windows-by-regexp \".*Nightly\$\")) 'cli-tag 'b)");
fire("b6");
fire("(window-put (match-best (get-area 55) (windows-by-regexp \".*Nightly\$\")) 'cli-tag 'a)");
fire("a55");
fire("(window-put (match-best (get-area 6) (windows-by-regexp \"^Screen\")) 'cli-tag 'z)");
fire("z6");
fire("(window-put (match-best (get-area 5) (windows-by-regexp \"^Screen\")) 'cli-tag 'y)");
fire("y5");
fire("(window-put (match-best (get-area 4) (windows-by-regexp \"^Screen\")) 'cli-tag 'x)");
fire("x4");

while(read(STDIN, $in, 1) > 0) {
    run("(pop-under (window-by-id ${winid}))", 1);
    if ($in =~ /^\377$/) {
        $s = undef;
        my $list = run("(window-list)");
        $list =~ s/\\012/\n/msg;
        $list =~ s/\A\"//;
        $list =~ s/\"\Z//;
        warn $list;
    } elsif ($in =~ /^\0$/) {
        fire($s);
        $s = undef;
    } elsif ($in =~ /^[\n]$/) {
        fire($s);
        $s = undef;
    } else {
        $s .= $in;
        $s = preview($s, $winid);
    }
}
die "done";
