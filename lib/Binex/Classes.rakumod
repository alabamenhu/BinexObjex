# Classes 2 is where I have the AST Binex objects

# todo convert this to a capture
class BXMatch is Positional is Associative {
    has Blob $.orig;
    has int  $.from;
    has int  $.to;
    has int  $.pos;
    has      @.positional = Empty; # can be a list OR a BinexMatch
    has      %.named      = Empty; # can be a list OR a BinexMatch
    method scoured { False }
    multi method gist (BXMatch:D:) { 'BXMatch[' ~ $!from ~ '…' ~ $!to ~ ']' }
    method Blob { $!orig.subbuf: $!from, $!to }
    method list { @!positional }
    method hash { %!named      }
}
subset BXMatchResult where BXMatch | Slip;


class BXMatchScoured is BXMatch {
    has Blob $.scoured-blob;
    method scoured { True }
    # Better logic is required here, because
    # we'll need to peak inward to other matches
    # It is TBD when (both in development, and in execution) that should happen
    method Blob { $!scoured-blob }
}

role Binex {
    has $.scoured = False;
    # TODO: standard Regex also accepts positionals and associatives
    #proto method ACCEPTS($) { * }
    #`(multi) method ACCEPTS (Blob \orig) {
        # Older rakudo accidentally included Backtraces' frames.
        my $comp = ($*RAKU // $*PERL).compiler; # $*PERL is deprecated
        my $skip-frames = $comp.name eq 'rakudo' && $comp.version < v2020.02.1.109.g7c3681647
                ?? 3
                !! 1;

        my \match = self.MATCH(orig, 0, orig.elems);
        if Backtrace.new($skip-frames).list.head.subname eq 'infix:<~~>' {
            $CALLER::CALLER::CALLER::('/') = match;
        }else{
            $CALLER::CALLER::('/') = match;
        }
        return match
    }
    proto method MATCH($, int $, int $? --> BXMatch) { * }
    # ^^ MATCH can always assume a *BX-CTX variable
}



class BXLiteral {...}
class BXQuant {...}
# These roles require their attributes to be rw
# This is due to the fact (bug?) that native types are considered immutable
# even when referenced with $!foo.
#       sadj  base  shift  place[0b....]
# bin:    1     2     7      1  [  0001]
# oct:    2     4     6      3  [  0011]
# hex:    4     8     4      7  [  0111]

role BXType[8] {
    has uint8 $!match = 0;
    has uint8 $!mask  = 0;

    method setup($chars, $bits) {
        my uint8 $s-adj = $*base eq 'b' ?? 1 !! $*base eq 'o' ?? 2 !! 4; # number of bits we shift
        my uint8 $base  = $s-adj * 2;
        my uint8 $shift = 8 - $s-adj;
        my uint8 $place = $base - 1;
        for $chars<> {
            NEXT     { $shift  -= $s-adj }
            when '.' { $!mask  +|=                $place  +< $shift }
            default  { $!match +|= parse-base(~$_, $base) +< $shift }
        }
        $!mask = +^$!mask;
    }
    method MATCH(Blob \orig, int \from, $) {
        # This is kind of silly, but the assignment is necessary to avoid unboxing
        my uint8 $filter = $!mask +& (orig[from] +^ $!match);
        return Nil unless $filter == 0;
        BXMatch.new: :orig(orig), :from(from), :to(from + 1);
    }
}

role BXType[16] {

}

role BXScouredType[8] {
    has uint8 $!match = 0;
    has uint8 $!mask = 0;
    has uint8 $!zero = 0;
    has uint8 $!rshift = 0;

    method setup($chars, $bits) {
        my uint8 $s-adj  = $*base eq 'b' ?? 1 !! $*base eq 'o' ?? 2 !! 4; # number of bits we shift
        my uint8 $base   = $s-adj * 2;
        my uint8 $shift  = 8 - $s-adj;
        my uint8 $place  = $base - 1;

        for $chars<> {
            NEXT     { $shift   -= $s-adj }
            when '.' { $!mask  +|= $place +< $shift }
            when 'X' { $!mask  +|= $place +< $shift; $!rshift++ }
            when 'Z' { $!mask  +|= $place +< $shift; $!zero +|= $place +< $shift }
            default  { $!match +|= parse-base(~$_, $base) +< $shift }
        }
        $!mask = +^$!mask;
        $!zero = +^$!zero;
    }
    method MATCH(Blob \orig, int \from, $ #`[would be $to, but on lit doesn't matter] ) {
        return Nil if $!mask +& (orig[from] +^ $!match);
        BXMatchScoured.new: :orig(orig), :from(from), :to(from + 1),
                            :scoured-blob(blob8.new: orig[from] +& $!zero +> $!rshift)
    }
}

class BXLiteral does Binex        {
    has  uint $.bit;     #= comes from role
    has       $.node;    #= I guess this is important (copying from Regex);
    #has ---- $.match;   #= the bits we must match (native-type specific, in role)
    #has ---- $.mask;    #= the bits we care about (native-type specific, in role)

    #| Creates a new BXLiteral.
    #|   Requires $*bits and $*base dynamic vars
    method new ($char, :$node, :$scoured) {
        # $*bits and $*base are available

        my \blessed := self.bless(:$*bit, :$node, :$scoured);

        $scoured
            ?? blessed does BXScouredType[$*bit]
            !! blessed does BXType[$*bit];

        blessed.setup($char, $*base);
        blessed;
    }

    # We *can* distinguish different types of blobs though.
    # This method will be run when there is a type mismatch.
    #multi method ACCEPTS($bad, $) {
    #    die "Binex data type mismatch: required {self.bit}-bit, but got {$bad.^name}";
        #return Nil if $!mask +& (orig[from] +^ $!match);
        #BXMatch.new:
        #    orig => orig,
        #    from => from,
        #    to => from + 1,
        #    positional-captures => Empty,
        #    named-captures => Empty
    #}

    multi method gist(BXLiteral:D:) { "BXLit{'X' if $.scoured}[" ~ self.node<char>.join ~ "]" }
    multi method gist(BXLiteral:U:) { "BXLiteral" }
}

# simply concatenates each entry
class BXConcat does Binex {
    has @.children;

    multi method gist { 'BXConcat' }
    multi method MATCH(Blob \orig, \from = 0, \to = orig.elems --> BXMatch) {
        my $pos = from;
        # Scoured needs to be handled separately
        # so that we don't slow down regular matches
        if $.scoured {
            my @matches;
            for @!children -> \child {
                my $match := child.MATCH(orig, $pos, to);
                return Nil unless $match;
                @matches.push: $match;
                $pos = $match.to;
            }
            return BXMatchScoured.new:
                    orig => orig,
                    from => from,
                    to => $pos,
                    scoured-blob => [~] @matches>>.Blob;
        } else {
            my \matches = self!step(0, orig, from, to);
            return Nil unless matches;
            return BXMatch.new: orig => orig, from => from, to => matches.tail.to;
        }
    }

    method !step(\index, Blob \orig, int \from, int \to) {
        # The logic of this is fairly simple.
        # Each child node is processed.  If it's a non-ratcheting quant
        # then we set a first run to 0, and then reattempt the match with
        # quant until there's a successful match chain, or the possibilities
        # are exhausted.

        return Empty if index == @!children.elems;
        my \child := @!children[index];

        # determine if we need to allow back tracking
        # If we are the final element, ratching is on, or it's not a quant, we don't
        if child.isa(BXQuant) && !child.ratcheting && (index + 1 != @!children.elems) {
            my $*last-quant;
            # each iteration will [in|de]crement the match count.
            # exhausted when Nil
            while my \match := child.MATCH(orig, from, to) {

                # if it matches, check and see if the rest of the concat works
                my \rest = self!step(index+1,orig,match.to, to);

                # if not, bail
                next unless rest; # use Nil, because Empty is falsy
                # success, so return it
                return match, |rest;
            }
            # bail because we exhausted
            return Nil;
        } else {
            # If quants ratchet or is final, then it runs once with default settings
            my $*last-quant;
            my \match := child.MATCH(orig, from, to);
            return Nil unless match;

            # We are the tail, so return now
            return (match,) if index + 1 == @!children.elems;

            # Still more, so check it
            my \rest := self!step(index+1, orig, match.to, to);
            return Nil unless rest;

            # merge and return (todo: maybe dyn variable?)
            return match, |rest;
        }
    }
}

# wonderfully, here we don't have to worry about
# the bitness
class BXQuant does Binex {
    has int $.min;
    has int $.max; #-1 = inf
    has @.children;
    has $.greedy = True;
    has $.ratcheting = False;

    multi method gist (BXQuant:D:) { 'BXQuant[' ~ $!min ~ '…' ~ $!max ~ (':' if $!ratcheting) ~ ('?' unless $!greedy) ~ ('X' if $.scoured) ~ ']'}
    multi method gist (BXQuant:U:) { 'BXQuant' }

    multi method MATCH(Blob \orig, \from = 0, \to = orig.elems --> BXMatchResult) {
        my $goal  := $*last-quant
                      ?? $!greedy
                          ?? ($*last-quant - 1)
                          !! ($*last-quant + 1)
                      !! ($!greedy || $!ratcheting) # $ratcheting always assumes greedy
                          ?? $!max
                          !! $!min;

        # This return can probably be removed, as the calling container
        # should know better, but it doesn't take a lot of extra cycles for now.
        return Nil if $!min > $goal && $goal != -1;
        return Nil if $goal > $!max && $!max != -1;

        my $pos   = from;
        my $found = 0;
        # Only make a new match context if there isn't one.
        # This should be made already by outer capture groups.
        #my $*BX-CTX = $*BX-CTX // BXMatch.new(orig => orig, from => from, pos => $pos);

        # These keep us from going further than we need to
        # Future optimizations might include restarting at the same place.
        # But that will require passing more cached data around.
        my $max := $goal > $!max ?? $goal !! $!max;


        my @matches;
        my \a = @!children[0];
        @matches[0] := a.MATCH(orig, $pos, to);
        $found++;

        # If it doesn't match, we can still "match" if it's not required
        unless @matches[0] {
            return Nil if $!min > 0;
            return Empty; #$*scoured
               # ?? BXMatch\      .new(:orig(orig), :from(from), :to(from))
               # !! BXMatchScoured.new(:orig(orig), :from(from), :to(from), :scoured-blob(@matches[0].Blob))
        }

        # greedy backtracking will eventually fail here
        return Nil unless $goal > 0 || $goal == -1;

        $pos = @matches[0].to;

        # Check if we have a separator
        if my \b := @!children[1] {
            say "There is a separator!";
            while (my \sep := b.MATCH: orig, $pos, to)
               && (to != ($pos = sep.to))
               && (my \cor := a.MATCH: orig, sep.to, to)
               && (@matches.elems != $goal) {

                @matches.push: sep;
                @matches.push: cor;
                $pos = cor.to;
                last if $pos == to;
                $found++;
            }
        }else{
            # no separator
            while $found != $goal && (my \match := a.MATCH(orig, $pos, to)) {
                @matches.push: match;
                $pos = match.to;
                last if $pos == to;
                $found++;
            }
        }

        # finally, check if we meet conditions:
        if $found ≥ $!min && ($found ≤ $!max || $!max == -1) {
            $*last-quant = $found;
            return BXMatch.new: :orig(orig), :from(from), :to(@matches.tail.to);
        } else {
            return Nil;
        }
    }

}

class BXConjSeq does Binex {
    has @.children;
    multi method gist(BXConjSeq:D:) { 'BXConjSeq·' ~ @!children.elems }
    multi method gist(BXConjSeq:U:) { 'BXConjSeq' }
    multi method MATCH(Blob \orig, \from = 0, \to = orig.elems --> BXMatch) {
        my @matches;
        for @!children -> \child {
            if my \match = child.MATCH(orig, from, to) {
                @matches.push: match;
            }else{
                return Nil
            }
            # TODO: insert better code
            die "I don't know how to handle & when either side isn't exactly the same...yet.  Sorry :-("
                unless @matches[0].to == @matches.all.to;
            return @matches;
        }
    }
}
class BXConj does Binex {
    has @.children;
    multi method gist(BXConj:D:) { 'BXConj·' ~ @!children.elems }
    multi method gist(BXConj:U:) { 'BXConj' }
    multi method MATCH(Blob \orig, \from = 0, \to = orig.elems --> BXMatch) {
        my \matches := @!children>>.MATCH(orig, from, to);
        return Nil unless matches.all;
        # TODO: insert better tie-breaking code
        die "I don't know how to handle & when either side isn't exactly the same...yet.  Sorry :-("
            unless matches[0].to == matches.all.to;
        return matches;
    }
}
class BXAltSeq does Binex {
    has @.children;
    multi method gist(BXAltSeq:D:) { 'BXAltSeq·' ~ @!children.elems }
    multi method gist(BXAltSeq:U:) { 'BXAltSeq' }
    multi method MATCH(Blob \orig, \from = 0, \to = orig.elems --> BXMatch) {
        for @!children -> \child {
            my \match := child.MATCH(orig, from, to);
            next unless match;
            return match;
        }
        # none worked
        return Nil;
    }
}
class BXAlt does Binex {
    has @.children;
    multi method gist(BXAlt:D:) { 'BXAlt·' ~ @!children.elems }
    multi method gist(BXAlt:U:) { 'BXAlt' }
    multi method MATCH(Blob \orig, \from = 0, \to = orig.elems --> BXMatch) {
        my @matches := @!children>>.MATCH(orig, from, to);
        # TODO: insert better tie-breaking code
        my \match := @matches.grep(*.defined).sort(*.to).tail;
        return Nil unless match;
        return match;
    }
}

class BXAnchor does Binex {
    has $.start;
    multi method gist(BXAnchor:D:) { 'BXAnchor' ~ ($!start ?? '^' !! '$' ) }
    multi method gist(BXAnchor:U:) { 'BXAnchor' }
    multi method MATCH(Blob \orig, \from, $ --> BXMatch) {
        return Nil
            unless from == $!start
                            ?? 0
                            !! orig.elems;
        return BXMatch.new: orig => orig, from => from, to => from;
    }
}

class BXProtoIndicator does Binex {
    multi method gist(BXAnchor:) { 'BXProtoIndicator' }
    multi method MATCH($,$,$) {
        die 'BXProto token {*} called.  This should never actually happen.';
    }
}

class BXCapture is BXConcat {
    has $.name = '';
    has $.position = '';
    method MATCH(\orig, \from, \to) {
        {
            my $*BX-CAPTURE = Match.new(orig, from, to);

            my \match = samewith(orig, from, to);
            return Nil unless match;

            match.positional = $*BX-CAPTURE.positional;
            match.named      = $*BX-CAPTURE.named;
        }
        if $!name eq '' {
            $*BX-CAPTURE.add_positional
        }
    }
}

class BXCaptureContext {
    has @!positional;
    has %!named;
    method add_named (\match, \name, :$force-array) {
        if %!named{name} {
            if %!named{name}.isa(BXMatch) {
                %!named{name} := Array.new: %!named{name},match
            } else {
                %!named{name}.push: match
            }
        } else {
            %!named{name} := $force-array ?? match !! Array.new(match)
        }
    }
    method add_positional (\match, \pos = @!positional.elems, :$force-array) {
        if @!positional[pos] {
            if @!positional[pos].isa(BXMatch) {
                @!positional[pos] := Array.new: @!positional[pos],match
            }else{
                @!positional.push: match
            }
        } else {
            if $force-array {
                @!positional[pos] := Array.new: match
            } else {
                @!positional[pos] := match
            }
        }
    }
    method list { @!positional }
    method hash { @!positional }}


class BXLiteralBlob does Binex {

}



    #`<<<method ACCEPTS (Blob[uint8] \orig, int \from --> BinexMatch) {
        return Nil if $!mask +& (orig[from] +^ $!match);
        #orig[from] +& $!zero +> $!rshift;
        BinexMatchScoured.new:
                orig => orig,
                from => from,
                to => from + 1,
                positional-captures => Empty,
                named-captures => Empty
    }>>>





sub as-bin(byte \b) { b == 0 ?? '0b00000000' !! sprintf("%#.8b",b) }
sub as-oct(byte \b) { b == 0 ?? '0o0000'     !! sprintf("%#.4o",b) }
sub as-hex(byte \b) { b == 0 ?? '0x00'       !! sprintf("%#.2x",b) }

multi sub dump(BXLiteral $b, $indent = 0) is export {
    say (' ' x $indent) ~ $b.gist;
}
multi sub dump(BXAnchor $b, $indent = 0) is export {
    say (' ' x $indent) ~ $b.gist;
}
multi sub dump(BXQuant $b, $indent = 0) is export  {
    say (' ' x $indent) ~ $b.gist;
    dump($_, $indent + 2) for $b.children;
}
multi sub dump(BXAlt $b, $indent = 0) is export  {
    say (' ' x $indent) ~ $b.gist;
    dump($_, $indent + 2) for $b.children;
}
multi sub dump(BXAltSeq $b, $indent = 0) is export  {
    say (' ' x $indent) ~ $b.gist;
    dump($_, $indent + 2) for $b.children;
}
multi sub dump(BXConjSeq $b, $indent = 0) is export  {
    say (' ' x $indent) ~ $b.gist;
    dump($_, $indent + 2) for $b.children;
}
multi sub dump(BXConj $b, $indent = 0) is export  {
    say (' ' x $indent) ~ $b.gist;
    dump($_, $indent + 2) for $b.children;
}

multi sub dump(BXConcat $b, $indent = 0) is export  {
    say (' ' x $indent) ~ $b.gist;
    dump($_, $indent + 2) for $b.children;
}

