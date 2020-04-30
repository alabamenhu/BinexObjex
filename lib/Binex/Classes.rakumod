# Classes 2 is where I have the AST Binex objects

# todo convert this to a capture
class BXMatch is Positional is Associative {
    has Blob $.orig;
    has int  $.from = 0;
    has int  $.to is rw = $!from ;
    has int  $.pos is rw = $!from;
    has      @.positional; # can be a list OR a BinexMatch
    has      %.named;      # can be a list OR a BinexMatch
    method scoured { False }
    multi method gist (BXMatch:D:) {
        "BXMatch"
                ~ "\x001b[38;5;242m" ~ "[" ~ "\x001b[0m"
                ~ $!from
                ~ "\x001b[38;5;242m" ~ "…" ~ "\x001b[0m"
                ~ $!to
                ~ "\x001b[38;5;242m" ~ "@" ~ "\x001b[0m"
                ~ "\x001b[34m" ~ $!pos ~ "\x001b[0m"
                ~ "\x001b[38;5;242m" ~ "]" ~ "\x001b[0m"
    }
    method Blob { $!orig.subbuf: $!from, $!to }
    method list { @!positional }
    method hash { %!named      }
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
    method add_positional (\match, \pos = @!positional.elems, :$force-array = False) {
        if @!positional[pos]:exists {
            if @!positional[pos].isa(BXMatch) {
                @!positional[pos] := Array.new: @!positional[pos],match
            }else{
                @!positional.push: match
            }
        } else {
            if $force-array {
                #say "Adding a match we've never seen before, single item as array";
                @!positional[pos] := Array.new: match
            } else {
                #say "Adding a match we've never seen before, single item";
                @!positional[pos] := match
            }
        }
    }

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

        # Init some values before matching
        my $*BX-QUANTIFIED = False;
        my $*BX-CAPTURE    := (self.scoured ?? BXMatchScoured !! BXMatch).new(orig => orig,:0from);
        $¢ = $*BX-CAPTURE; # this should propogate down the caller chain, but it doesn't.

        my \result = self.MATCH(orig, 0, orig.elems);

        # The original $/ may be two or three callers up, depending on
        # whether it was called with ~~ or .ACCEPTS directly.
        if Backtrace.new($skip-frames).list.head.subname eq 'infix:<~~>' {
            $CALLER::CALLER::CALLER::('/') = result;
        }else{
            $CALLER::CALLER::('/') = result;
        }

        return result
    }
    proto method MATCH($, int $, int $? --> BXMatch) { * }
    # ^^ MATCH can always assume a *BX-CAPTURE variable
}



class BXLiteral {...}
class BXQuant {...}
class BXDynQuant {...}
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
    method MATCH(Blob \orig, int \from) {
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
    method MATCH(Blob \orig, int \from) {
        return Nil if $!mask +& (orig[from] +^ $!match);
        BXMatchScoured.new: :orig(orig), :from(from), :to(from + 1), :pos(from + 1),
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
        my \match = ($.scoured ?? BXMatchScoured !! BXMatch).new:
                orig => orig,
                from => from,
                pos => from;

        my $*BX-CAPTURE = $CALLERS::('*BX-CAPTURE') // match;
        # Scoured requires regularly concatenating a blob
        # so it's avoided in regular matches.
        # These subs will restore old pos/to values
        if $.scoured {
            #my $scoured-blob := match.scoured-blob;
            return match if self!step-scoured(0,match);
            return Nil;
        } else {
            return match if self!step(0,match);
            return Nil
        }
    }

    method !step(\index, \match) {
        # This is a recursive method.  DO NOT CHANGE WITHOUT ALSO CHANGING SCOURED
        # The "to" attribute is only set when the match is made.
        # A match is successful if we reach this point (the end of the chain).
        if index == @!children.elems {
            match.to = match.pos;
            return True;
        }

        my \child := @!children[index];

        child.set_min_max if child.isa(BXDynQuant);

        # determine if we need to allow back tracking
        # If we are the final element, ratching is on, or it's not a quant, we don't
        if child.isa(BXQuant) && !child.ratcheting && (index + 1 != @!children.elems) {
            my $*last-quant;

            # Each iteration will [in|de]crement the match count via $*last-quant,
            # which is reset in the BXQuant Match method.
            # Backtracking exhausted when Nil
            while my \current := child.MATCH(match.orig, match.pos) {
                # If it matches, check and see if the rest of the concat works
                my $old-pos = match.pos;
                match.pos = current.to;
                my \end := self!step(index+1,match);

                # End is True if the rest of the the trail matched
                if end {
                    return True;
                } else {
                    # Restore old POS, because we're just passing around the original
                    match.pos = $old-pos
                }
            }
            # We only reach this point if all attempts to get a valid
            # match with the quantified atom fail, so we return Nil.
            return False;

        } else {
            # If quants ratchet or is final, then it runs once with default settings
            my $*last-quant;
            my \current := child.MATCH(match.orig, match.pos);
            return False unless current;

            # Match forward
            match.pos = current.to;
            my \end := self!step(index+1, match);
            return end;
        }
    }

    method !step-scoured(\index, \match) {
        # This is a recursive method.  THIS METHOD SHOULD BE IDENTICAL TO !STEP
        #   except that it also regularly updates the blob which requires more
        #   resource allocation.
        # The "to" attribute is only set when the match is made.
        # A match is successful if we reach this point (the end of the chain).
        if index == @!children.elems {
            match.to = match.pos;
            return True;
        }

        my \child := @!children[index];

        # determine if we need to allow back tracking
        # If we are the final element, ratching is on, or it's not a quant, we don't
        if child.isa(BXQuant) && !child.ratcheting && (index + 1 != @!children.elems) {
            my $*last-quant;
            # Each iteration will [in|de]crement the match count via $*last-quant,
            # which is reset in the BXQuant Match method.
            # Backtracking exhausted when Nil
            while my \current := child.MATCH(match.orig, match.pos) {
                # If it matches, check and see if the rest of the concat works
                my $old-pos  = match.pos;
                my $old-blob = match.scoured-blob;
                match.pos           = current.to;
                match.scoured-blob ~= current.scoured-blob;

                my \end := self!step(index+1,match);

                # End is True if the rest of the the trail matched
                if end {
                    return True;
                } else {
                    # Restore old POS, because we're just passing around the original
                    match.pos          = $old-pos;
                    match.scoured-blob = $old-blob;
                }
            }
            # We only reach this point if all attempts to get a valid
            # match with the quantified atom fail, so we return Nil.
            return False;

        } else {
            # If quants ratchet or is final, then it runs once with default settings
            my $*last-quant;
            my \current := child.MATCH(match.orig, match.pos);
            return False unless current;

            # Match forward
            match.pos          = current.to;
            match.scoured-blob = current.scoured-blob;

            # --> True (OK) / False (failed match)
            return self!step(index+1, match);
        }
    }

}

# wonderfully, here we don't have to worry about
# the bitness
class BXQuant does Binex {
    has int $.min is rw;
    has int $.max is rw; #-1 = inf
    has @.children;
    has $.greedy = True;
    has $.ratcheting = False;

    multi method gist (BXQuant:D:) { 'BXQuant[' ~ $!min ~ '…' ~ $!max ~ (':' if $!ratcheting) ~ ('?' unless $!greedy) ~ ('X' if $.scoured) ~ ']'}
    multi method gist (BXQuant:U:) { 'BXQuant' }

    multi method MATCH(Blob \orig, \from = 0 --> BXMatchResult) {
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
        my \to    = orig.elems; # fix, but at the moment, establishes a max pos
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
        @matches[0] := a.MATCH(orig, $pos);
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

            while (my \sep := b.MATCH: orig, $pos, to)
               && (to != ($pos = sep.to) !>= orig.elems)
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
            while $found != $goal && (my \match := a.MATCH(orig, $pos)) {
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

class BXCodeBlock is Binex {
    has $.code = "";
    method EVAL {
        use MONKEY-SEE-NO-EVAL;
        # This activates $/, and the newline ensures ends lining in comments are correctly parsed
        ('$/ = $*BX-CONTEXT; { ' ~ $!code ~ "\n } ").EVAL
    }
    method gist { 'BXCodeBlock{' ~ .code ~ '}' }
    method Str { $!code }
    method MATCH(\orig, \from) {
        return BXMatch.new: orig => orig, from => from, to => from;
    }
}

class BXDynQuant is BXQuant {
    has $.codeblock;
    method gist { 'BXDynQuant{' ~ $!codeblock.Str ~ '}' }
    method set_min_max {
        my $range = $!codeblock.EVAL;
        if $range ~~ Range {
            ($.min, $.max) = $range.int-bounds;
            die "Can't use a negative index" if $.min < 0;
            $.max = -1 if $.max ~~ Inf;
        } else {
            $.min = $.max = $range.Int;
        }
    }
}
class BXCapture is BXConcat {
    has $.name             = '';
    has $.position   is rw = -1;
    has $.force-list       = False;
    method gist {
        'BXCapture'
                ~ "\x001b[32m"
                ~ ($!name ne ''
                    ?? "<$!name>"
                    !! ($!position > -1
                        ?? "[$!position]"
                        !! "[?]"))
                ~ "\x001b[0m"
    }

    method MATCH(\orig, \from, \to = orig.elems) {

        # Save the outer context, if it exists
        my \outer := $CALLERS::('*BX-CAPTURE') // Match.new(orig => orig, from => from, to => to);

        # Kill the current context, so that concat
        # creates a new capturing context
        my $*BX-CAPTURE = Nil;

        return Nil
            unless my \match = self.BXConcat::MATCH: orig, from, to;

        # TODO: better determine if we're in a quant
        #       ... maybe via a $*BX-QUANTIFIED var that gets reset above?
        if $!name {
            outer.add_named: match;
        } else {
            outer.add_positional: match;
        }

        return match;
    }
}

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

subset BXEndNode of Binex where .isa(BXLiteral) || .isa(BXCodeBlock) || .isa(BXAnchor);
subset BXParentNode of Binex where !.isa(BXLiteral) || !.isa(BXCodeBlock) || !.isa(BXAnchor);

multi sub dump(BXEndNode $b, $indent = 0) is export {
    say (' ' x $indent) ~ $b.gist;
}
multi sub dump(BXParentNode $b, $indent = 0) is export  {
    say (' ' x $indent) ~ $b.gist;
    dump($_, $indent + 2) for $b.children;
}
