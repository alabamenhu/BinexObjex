# Actions 2 is where I am building up each stage of the actions
unit class BinexActions;
use Binex::Classes;

method metachar:sym<( )> ($/) {
    my $qast := $<nibbler>.made;

    if $qast.isa(BXConcat) {
        $qast := BXCapture.new(:children($qast.children) );
    }else{
        $qast := BXCapture.new( :node($qast.node), :children($qast) );
    }

    make $qast;
}

method metachar:sym<[ ]> ($/) {
    my $qast := $<nibbler>.made;
    make $qast;
}


method TOP ($/) {
    my $qast := $<nibbler>.made;
    # Once finalized, check the positional elements.
    # I'm not sure how regular regex does this, but this is simple enough
    walk-pos $qast;
    make $qast;
}
method nibbler ($/) {
    my $qast := $<termseq>.made;
    make $qast;
}
method termseq ($/) {
    my $qast := $<termaltseq>.made;
    make $qast;

}
method termaltseq ($/) {
    my $qast := $<termconjseq>[0].made;

    if $<termconjseq>.elems > 1 {
        $qast := BXAltSeq.new(:node($/), :children($<termconjseq>.list>>.made))
    }
    make $qast;
}

method termconjseq ($/) {
    my $qast := $<termalt>[0].made;
    if $<termalt>.elems > 1 {
        $qast := BXConjSeq.new(:node($/), :children($<termalt>.list>>.made))
    }
    make $qast;
}
method termconj ($/) {
    my $qast := $<termish>[0].made;
    if $<termish>.elems > 1 {
        $qast := BXConj.new(:node($/), :children($<termish>.list>>.made))
    }
    make $qast;
}
method termalt ($/) {
    my $qast := $<termconj>[0].made;
    if $<termconj>.elems > 1 {
        $qast := BXAlt.new(:node($/), :children($<termconj>.list>>.made))
    }
    make $qast;
}


# Termish itself doesn't really do anything, it helps for catching errors
method termish ($/) {
    my $qast := BXConcat.new(:node($/));
    # Below is optimization, making BXLiteral --> BXScoured
#`<<<    my $lastlit := 0;
    my $last_noun;
    for $<noun> { # $<noun> is a qualified-atom
        my $ast := $_.made;
        if $ast {
            # This is where the literals are merged
            # TODO align with Binex conditions
            if $lastlit && $ast.bxtype eq 'literal'
                    && !QAST::Node.ACCEPTS($ast[0])
                    && $lastlit.subtype eq $ast.subtype {
                $lastlit[0] := $lastlit[0] ~ $ast[0];
            }
            else {
                $qast.push($_.made);
                $lastlit := $ast.bxtype eq 'literal'
                        && !QAST::Node.ACCEPTS($ast[0])
                        ?? $ast !! 0;
            }
        }
        $last_noun := $_;
    }>>>
    $qast.children.push($_.made) for $<noun><>;
    make $qast;
}

method quantifier:sym<*>($/) {
    my $qast := BXQuant.new(:min(0), :max(-1), :node($/), :$*scoured);
    make $qast;
    #make backmod($qast, $<backmod>);
}

method quantifier:sym<+>($/) {
    my $qast := BXQuant.new(:min(0), :max(-1), :node($/), :$*scoured);
    make $qast;

    #my $qast := QAST::Regex.new( :bxtype<quant>, :min(1), :max(-1), :node($/) );
    #make backmod($qast, $<backmod>);
}

method quantifier:sym<?>($/) {
    my $qast := BXQuant.new(:min(0), :max(1), :node($/), :$*scoured);
    make $qast;
    #my $qast := QAST::Regex.new( :bxtype<quant>, :subtype<item>, :min(0), :max(1), :node($/) );
    #make backmod($qast, $<backmod>);
}


method codeblock ($/) {
    my $qast := BXCodeBlock.new(:code($<block>.Str), :node($/));
    make $qast;
}

method quantifier:sym<**>($/) {
    my $qast;
    if $<codeblock> {
        $qast = BXDynQuant.new( :codeblock($<codeblock>.made), :node($/) );
        #`<<< Normally, we would grab the Raku parse tree and put it directly here.
              That's not possible in module space, so instad we hack the above
        $qast := QAST::Regex.new( :rxtype<dynquant>, :node($/),
                QAST::Op.new( :op('callmethod'), :name('!DYNQUANT_LIMITS'),
                        QAST::Var.new( :name('$¢'), :scope('lexical') ),
                        $<codeblock>.ast
                        ),
                );
        >>>
    }
    else {
        my $min := 0;
        if $<min> { $min := $<min>.parse-base(10) }

        my $max := -1;
        #my $upto := $<upto>;

        if $<from> eq '^' { ++$min }

        if ! $<max> {
            $max := $min
        }
        elsif $<max> ne '*' {
            $max := $<max>.parse-base(10);
            if $<upto> eq '^' {
                --$max;
            }
            $/.panic("Empty range") if $min > $max;
        }
        $qast := QAST::Binex.new( :rxtype<quant>, :min($min), :max($max), :node($/) );
    }
    make $qast;
    #make backmod($qast, $<backmod>);
}


method quantified_atom($/) {
    my $qast := $<atom>.made;

    if $<quantifier> {
        $/.panic('Quantifier quantifies nothing')
            unless $qast;
        #my str $bxtype = $qast.bxtype;
        $/.throw_non_quantifiable()
            if #`($bxtype eq 'qastnode' ||) $qast.isa(BXAnchor);
        my $ast := $<quantifier>.made;

        $ast.children.unshift($qast);
        $qast := $ast;
    }

    if $<separator> {
        if !$qast.isa(BXQuant) && !$qast.isa(BXQuant) #`( ne 'dynquant') {
            $/.panic("'" ~ $<separator><septype> ~
                    "' may only be used immediately following a quantifier")
        }

        $qast.children.push($<separator>.made);

        if $<separator><septype> eq '%%' {
            $qast := BXConcat.new( :children(
                $qast,
                BXQuant.new(:0min, :1max, :children($<separator>.made))
            ));

        }
    }

    # set ratcheting here
   # if $qast {
    #    $qast.backtrack('r') if !$qast.backtrack && ($<backmod> ?? (~$<backmod> eq ':') !! %*BX<r>);
    #    $qast.node($/);
    #}
    make $qast;
}


method atom($/) {
    # There are some metachars, for instance, ^ or $ in Binex
    if $<metachar> {
        make $<metachar>.ast;
    } else {
        make $<word>.made if $<word>;
        # TODO: apply literal modifiers from above

        #my $qast := QAST::Regex.new( ~$/, :rxtype<literal>, :node($/));
        #make $qast;
        #make self.apply_literal_modifiers($qast); Binex: don't think modifiers are needed here but TODO check
    }
}

method word ($/) {
    #  $*bits [passed in]
    my $*base = $<base>.Str;
    my $qast := BXLiteral.new($<char>, :node($/), :$*scoured);
    make $qast;
}

method metachar:sym<$> ($/) {
    my $qast := BXAnchor.new(:!start, :node($/));
    make $qast;
}
method metachar:sym<^> ($/) {
    my $qast := BXAnchor.new(:start, :node($/));
    make $qast;
}

method metachar:sym<{*}> ($/) {
    my $qast := BXProtoIndicator.new;
    make $qast;
}

method separator ($/) {
    make $<quantified_atom>.ast;
}



method metachar:sym<var>($/) {
    my $qast;
    # Is it a named match or a numeric match?
    my $name := $<pos> ?? $<pos>.parse-base(10) !! ~$<name>;


    if $<quantified_atom> {
        $qast := $<quantified_atom>[0].ast;
        if ($qast.rxtype eq 'quant' || $qast.rxtype eq 'dynquant') && $qast[0].rxtype eq 'subrule' {
            self.subrule_alias($qast[0], $name);
        }
        elsif $qast.rxtype eq 'subrule' {
            self.subrule_alias($qast, $name);
            $qast := QAST::Regex.new( :rxtype<quant>, :min(1), :max(1), $qast) if $<wantarray>;
        }
        else {
            $qast := QAST::Regex.new( $qast, :name($name),
                    :rxtype<subcapture>, :node($/) );
        }
    }
    else {
        $qast := QAST::Regex.new( :rxtype<subrule>, :subtype<method>, :node($/),
                QAST::NodeList.new(
                        QAST::SVal.new( :value('!BACKREF') ),
                        QAST::SVal.new( :value($name) ) ) );
    }
    make $qast;
}





method metachar:sym<assert>($/) {
    make $<assertion>.ast;
}

method assertion ($/) {

}




#| Walks through a Match tree to determine what
#| the index of the positional captures are
sub walk-pos(Binex \b, \c = 0, $outer = True) is export {
    return 0 if b.isa(BXLiteral);
    return 0 if b.isa(BXAnchor);

    # Outer ensures we don't get caught the second time around
    if b.isa(BXCapture) && $outer {
        b.position = c;
        samewith b, 0, False;
        return c + 1
    }

    my $count = c;
    $count = samewith $_, $count, True
        for b.children;
    return $count;
}
