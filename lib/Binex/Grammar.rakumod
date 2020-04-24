unit grammar BinexGrammar;
    use nqp; # allow compile, but eventually remove;

    method obs ($old, $new, $when = ' in Perl 6') {
        self.panic('Unsupported use of ' ~ ~$old ~ ';'
                ~ ~$when ~ ' please use ' ~ ~$new);
    }

    # errors are reported through methods, so that subclasses like Rakudo's
    # Perl6::RegexGrammar can override them, and throw language-specific
    # exceptions
    method throw_unrecognized_metachar ($char) {
        self.panic('Unrecognized regex metacharacter ' ~ $char ~ ' (must be quoted to match literally)');
    }

    method throw_malformed_range() {
        self.panic('Malformed range.');
    }

    method throw_confused() {
        self.panic('Confused.');
    }

    method throw_unspace($char) {
        self.panic: "No unspace allowed in regex; " ~
                " if you meant to match the literal character," ~
                " please enclose in single quotes ('"
                ~ $char ~ "') or use a backslashed form like \\x"
                ~ nqp::sprintf('%02x', [nqp::ord($char)]);
    }

    method throw_regex_not_terminated() {
        self.panic('Regex not terminated.');
    }

    method throw_spaces_in_bare_range() {
        self.panic('Spaces not allowed in bare range.');
    }

    method throw_unecessary_upto_inf() {
        self.panic('Unecessary use of "** ^*" quantifier. Did you mean to use the "*" quantifier');
    }

    method throw_solitary_quantifier() {
        self.panic('Quantifier quantifies nothing.');
    }

    method throw_non_quantifiable() {
        self.panic('Can only quantify a construct that produces a match');
    }

    method throw_solitary_backtrack_control() {
        self.panic("Backtrack control ':' does not seem to have a preceding atom to control");
    }

    method throw_null_pattern() {
        self.panic('Null regex not allowed');
    }

    method worry(*@args) {
        note(nqp::join('', @args) ~ "\n");
    }


    # BINEX MODIFICATION NOTE:
    # This token is fine and can be used as is
    token ws { [ \s | '#' \N* ]* }

    # BINEX MODIFICATION NOTE:
    # This token is fine and can be used as is
    token normspace { <?[\s#]> <.ws> }

    # BINEX MODIFICATION NOTE:
    # This token is fine and can be used as is
    token identifier { <.ident> [ <[\-']> <.ident> ]* }

    # BINEX MODIFICATION NOTE:
    # This token is fine and can be used as is
    token arg {
        [
        | <?[']> <quote_EXPR: ':q'>
        | <?["]> <quote_EXPR: ':qq'>
        | $<val>=[\d+]
        ]
    }

    # BINEX MODIFICATION NOTE:
    # This token is fine and can be used as is
    rule arglist { '' <arg> +% [',' ] }

    # BINEX MODIFICATION NOTE:
    # This token should import instead QRegex::Binex::World
    my $cur_handle := 0;
    token TOP {
        #:my %*BX;
        #:my $handle := '__QREGEX_BINEX__' ~ $cur_handle++;
        #:my $*W := QRegex::P6Regex::World.new(:$handle);

        <nibbler>
        [ $ || <.throw_confused> ]
    }

    # BINEX MODIFICATION NOTE:
    # I'm still not entirely certain what the nibbler does
    # but since it has no references to terms, it should be okay.
    token nibbler {
        #:my $OLDRX := nqp::getlexdyn('%*RX');
        #:my %*RX;
        #:my $*SEQ := 0;
        #{
        #    for $OLDRX { %*RX{$_.key} := $_.value; }
        #}
        <.ws>
        <termseq>
# For some reason, in testing this falls apart
#        [
#        || <?infixstopper>
#        || $$ <.throw_regex_not_terminated>
#        || (\W) { self.throw_unrecognized_metachar: ~$/[0] }
#        || <.throw_regex_not_terminated>
#        ]
    }

    # BINEX MODIFICATION NOTE:
    # This token is fine and can be used as is
    regex infixstopper {
        :dba('infix stopper')
        [
        | <?before <.[\) \} \]]> >
        | <?before '>' <.-[>]> >
        | <?bxstopper>
        ]
    }

    # BINEX MODIFICATION NOTE:
    # These tokens are all fine as is at the moment, though should be updated
    # if regex makes the noted changes
    token bxstopper { $ }

    # XXX Eventually squish termseq and termish and
    # get < || && | & > infixes using by EXPR in nibbler
    token termseq {
        <termaltseq>
    }

    token termaltseq {
        [ <!bxstopper> '||' #`({ $*SEQ := 1; }) <.ws> ]?
        <termconjseq>
        [ <!infixstopper> '||' <.ws> #`({ $*SEQ := 1; }) <termconjseq> ]*
    }

    token termconjseq {
        [ <!bxstopper> '&&' #`({ $*SEQ := 0; }) <.ws> ]?
        <termalt>
        [ <!infixstopper> '&&' <.ws> #`({ $*SEQ := 0; }) <termalt> ]*
    }

    token termalt {
        [ <!bxstopper> '|' <.ws> ]?
        <termconj>
        [ <!infixstopper> '|' <![|]> <.ws> { #`($*SEQ := 0;) } <termconj> ]*
    }

    token termconj {
        [ <!bxstopper> '&' <.ws> ]?
        <termish>
        [ <!infixstopper> '&' <![&]> <.ws> { #`($*SEQ := 0;) } <termish> ]*
    }

    # BINEX MODIFICATION NOTE:
    # The only modification that we need here is to provide an awesome error
    # message if there is a non-atom that we find.
    token termish {
        :my $*SIGOK  := 0;
        :my $*VARDEF := 0;
        [
        || <noun=.quantified_atom>+
        || <?before <.bxstopper> | <.[&|~]> > <.throw_null_pattern>
        || <?before <.infixstopper> >         <.throw_null_pattern> # XXX Check if unmatched bracket
        || $$                                 <.throw_regex_not_terminated>
        || (\W)                          { self.throw_unrecognized_metachar: ~$/[0] }
        ||                                    <.throw_regex_not_terminated>
        ]
    }

    # BINEX MODIFICATION NOTE:
    # We don't worry about significant space at all.  All SIGOKs can probably be deleted
    # method SIGOK() { $*SIGOK := %*RX<s>; self }

    # BINEX MODIFICATION NOTE:
    # Nothing should need to be adjusted here, as important things are in <atom>
    token quantified_atom {
        <!bxstopper>
        <atom>
        [
        ||  <.normspace>?
            [
            | <!bxstopper> <quantifier>
            | <?[:]> <backmod> <!alnum>
            ]
            [ <!{$*VARDEF}> #`«« <.SIGOK> <sigfinal=.sigmaybe> »» ]?
            [ <.ws> <separator> ]?
        ||  [ <!{$*VARDEF}> #`«« <sigfinal=.sigmaybe> »»  ]?
        ]
        <.ws>
        # { $*SIGOK := 0 } SIGOK can be deleted
    }

    # BINEX MODIFICATION NOTE:
    # Nothing should need to be adjusted here
    rule separator {
        { say "Exploring a separator" }
        $<septype>=['%''%'?]
        :my $*VARDEF := 0;
        #:my $*SIGOK  := 0;
        <quantified_atom>
    }

    # BINEX MODIFICATION NOTE:
    # The atom for us is, in effect, a byte/word, rather than a word character.
    # The following changes were made:
    # 1. \w --> <word>
    # 2. the [<?before ' ' ...]? block was removed, as significant space isn't an issue for us
    # 3. dba is commented out, as I don't know what it does yet but I think it's debug
    token atom {
        # :dba('regex atom')
        [
        | <word>
        | <metachar>
        ]
    }

    # BINEX MODIFICATION NOTE:
    # These don't need changing.  Sigmaybe defines two different types of whitespaces
    # for later processing
    proto token sigmaybe { <...> }

    token sigmaybe:sym<normspace> {
        <!{$*SIGOK}> <normspace>
    }

    token sigmaybe:sym<sigwhite> {
        <?{$*SIGOK}> <normspace>
    }

    # BINEX MODIFICATION NOTE:
    # Quantifiers should be fine and can be used as is
    proto token quantifier { <...> }
    token quantifier:sym<%> {
        ('%''%'?) {
            $/.panic("Missing quantifier on the left argument of " ~ $/[0]);
        }
    }
    token quantifier:sym<*> { <sym> <backmod> }
    token quantifier:sym<+> { <sym> <backmod> }
    token quantifier:sym<?> { <sym> <backmod> }
    token quantifier:sym<{N,M}> { {} '{' (\d+) (','?) (\d*) '}'
        <.obs: '{N,M} as general quantifier', '** N..M (or ** N..*)'>
    }

    # BINEX MODIFICATION NOTE:
    # This token should be fine and can be used as is
    token quantifier:sym<**> {
        # 10 | 1..10 | 1^..10 | 1^..^10 | 1..^10 | ^10 | 1..* | 1^..*
        <sym> <.normspace>? <backmod> <.normspace>?
        [
        | <min=.integer> \s+ '..' <.throw_spaces_in_bare_range>
        | '^' '*' <.throw_unecessary_upto_inf>
        | $<upto>='^' <max=.integer>
        | [
        | <min=.integer>
        [
        [
        | $<from>='^' [ '..' | <.throw_malformed_range> ]
        | '..'
        ]
        [
        | $<upto>='^'? <max=.integer> {
            $/.panic("Negative numbers are not allowed as quantifiers") if nqp::radix(10, $<max>, 0, 0)[0] < 0;
        }
        | $<max>=['*']
        | <.throw_malformed_range>
        ]
        ]?
        ]
        { $/.panic("Negative numbers are not allowed as quantifiers") if nqp::radix(10, $<min>, 0, 0)[0] < 0 }
        | <?[{]> <codeblock>
        ]
    }

    # BINEX MODIFICATION NOTE:
    # This token is fine and can be used as is
    token codeblock {
        <block=.LANG('MAIN','pblock')>
    }

    # BINEX MODIFICATION NOTE:
    # This token is fine and can be used as is
    token backmod { ':'? [ '?' | '!' | <!before ':'> ] }

    # BINEX MODIFICATION NOTE:
    # Most of these make sense, but a few don't:
    # 1. Quotes removed because we don't need them
    # 2. . removed, because confusing.  TODO: redefine as <any> instead
    # 3. ^^ and $$ removed, because lines aren't a thing
    # 4. lwb and rwb make no sense either
    # 5. from/to kept, but only on byte/word boundaries
    # 6. bs makes no sense, \foo or \\ would be invalid anyways
    # 7. TODO reconsider <mod>?
    proto token metachar { <...> }
    token metachar:sym<[ ]> { '[' ~ ']' <nibbler> #`(<.SIGOK>) }
    token metachar:sym<( )> { '(' ~ ')' <nibbler> <.SIGOK> }
#    token metachar:sym<'> { <?['‘‚]> <quote_EXPR: ':q'>  <.SIGOK> }
#    token metachar:sym<"> { <?["“„]> <quote_EXPR: ':qq'> <.SIGOK> }
#    token metachar:sym<.> { <sym> <.SIGOK> }
    token metachar:sym<^> { <sym> #`(<.SIGOK>) }
#    token metachar:sym<^^> { <sym> <.SIGOK> }
    token metachar:sym<$> { <sym> #`(<.SIGOK>) }
#    token metachar:sym<$$> { <sym> <.SIGOK> }
    token metachar:sym<:::> { <sym> <.panic: '::: not yet implemented'> }
    token metachar:sym<::> { <sym> <.panic: ':: not yet implemented'> }
#    token metachar:sym<lwb> { $<sym>=['<<'|'«'] <.SIGOK> }
#    token metachar:sym<rwb> { $<sym>=['>>'|'»'] <.SIGOK> }
    token metachar:sym<from> { '<(' <.SIGOK> }
    token metachar:sym<to>   { ')>' <.SIGOK> }
#    token metachar:sym<bs> { \\ <backslash> <.SIGOK> }
    token metachar:sym<mod> { <mod_internal> }
    token metachar:sym<quantifier> {
        <!bxstopper> <quantifier> <.throw_solitary_quantifier>
    }

    # BINEX MODIFICATION NOTE:
    # This token is fine and can be used as is
    # we cheat here, really should be regex_infix:sym<~>
    token metachar:sym<~> {
        :my $*HAS_GOAL := 1;
        <sym>
        <.ws> <GOAL=.quantified_atom>
        <.ws> <EXPR=.quantified_atom>
    }

    # BINEX MODIFICATION NOTE:
    # This token is fine and can be used as is
    token metachar:sym<{*}> {
        <sym>
        [ \h* '#= ' \h* $<key>=[\S+ [\h+ \S+]*] ]**0..1
    }

    # BINEX MODIFICATION NOTE:
    # This token is fine and can be used as is
    token metachar:sym<assert> {
        '<' ~ '>' <assertion> <.SIGOK>
    }

    # BINEX MODIFICATION NOTE:
    # This token is fine and can be used as is
    token sigil { <[$@%&]> }

    # BINEX MODIFICATION NOTE:
    # This token is fine and can be used as is
    token metachar:sym<var> {
        [
        | $<wantarray>=['@'] '<' $<name>=[<-[>]>+] '>'
        | '$<' $<name>=[<-[>]>+] '>'
        | '$' $<pos>=[\d+]
        ]

        [
        <.ws> '=' <.ws>
        { $*VARDEF := 1 }
        <quantified_atom>
        { $*VARDEF := 0 }
        ]**0..1
        <.SIGOK>
    }

    # BINEX MODIFICATION NOTE:
    # This token is fine and can be used as is
    token metachar:sym<:> {
        <sym> <?before \s> <.throw_solitary_backtrack_control>
    }

    # BINEX MODIFICATION NOTE:
    # Really none of these are going to be needed, because they deal with
    # special character/classes that don't pertain to binary data
#`<<
    proto token backslash { <...> }
    token backslash:sym<s> { $<sym>=[<[dDnNsSwW]>] }
    token backslash:sym<e> { $<sym>=[<[eE]>] }
    token backslash:sym<f> { $<sym>=[<[fF]>] }
    token backslash:sym<h> { $<sym>=[<[hH]>] }
    token backslash:sym<r> { $<sym>=[<[rR]>] }
    token backslash:sym<t> { $<sym>=[<[tT]>] }
    token backslash:sym<v> { $<sym>=[<[vV]>] }
    token backslash:sym<o> { $<sym>=[<[oO]>] [ <octint> | '[' <octints> ']' ] }
    token backslash:sym<x> { $<sym>=[<[xX]>] [ <hexint> | '[' <hexints> ']' ] }
    token backslash:sym<c> { $<sym>=[<[cC]>] <charspec> }
    token backslash:sym<0> { $<sym>=['0'] }
    token backslash:sym<B> { 'B' <.obs:
    '\\B', '<!|w> for negated word boundary. If you meant a negated'
            ~ ' backspace character, use it in a negated character class (<-[\b]>).'
            > }
    token backslash:sym<b> { 'b' <.obs:
    '\\b', '<|w> for word boundary (or « and » for left/right boundaries).'
            ~ ' If you meant the backspace character, quote it ("\b") or use it as'
            ~ ' inside a character class (<[\b]>)'
            >}
    token backslash:sym<K> { 'K' <.obs:
    '\\K', '<( for discarding text before the capture marker or )> for discarding text after.'
            >}
    token backslash:sym<A> { 'A' <.obs: '\\A as beginning-of-string matcher', '^'> }
    token backslash:sym<z> { 'z' <.obs: '\\z as end-of-string matcher', '$'> }
    token backslash:sym<Z> { 'Z' <.obs: '\\Z as end-of-string matcher', '\\n?$'> }
    token backslash:sym<Q> { 'Q' <.obs: '\\Q as quotemeta', 'quotes or literal variable match'> }
    token backslash:sym<unrec> { {} (\w) { self.throw_unrecog_backslash_seq: $/[0].Str } }
    token backslash:sym<unsp> {
        \s {}
        <.throw_unspace(~$/)>
    }
    token backslash:sym<misc> { \W }
>>
    # BINEX MODIFICATION NOTE:
    # Most of these tokens won't be used and can be deleted (I think)
#`<<
    proto token cclass_backslash { <...> }
    token cclass_backslash:sym<s> { $<sym>=[<[dDnNsSwW]>] }
    token cclass_backslash:sym<b> { $<sym>=[<[bB]>] }
    token cclass_backslash:sym<e> { $<sym>=[<[eE]>] }
    token cclass_backslash:sym<f> { $<sym>=[<[fF]>] }
    token cclass_backslash:sym<h> { $<sym>=[<[hH]>] }
    token cclass_backslash:sym<n> { $<sym>=[<[nN]>] }
    token cclass_backslash:sym<r> { $<sym>=[<[rR]>] }
    token cclass_backslash:sym<t> { $<sym>=[<[tT]>] }
    token cclass_backslash:sym<v> { $<sym>=[<[vV]>] }
    token cclass_backslash:sym<o> { $<sym>=[<[oO]>] [ <octint> | '[' <octints> ']' ] }
    token cclass_backslash:sym<x> { $<sym>=[<[xX]>] [ <hexint> | '[' <hexints> ']' ] }
    token cclass_backslash:sym<c> { $<sym>=[<[cC]>] <charspec> }
    token cclass_backslash:sym<0> { $<sym>=['0'] }
    token cclass_backslash:sym<any> { . }
>>

    # BINEX MODIFICATION NOTE:
    # These tokens are fine and can be used as is
    proto token assertion { <...> }

    token assertion:sym<?> { '?' [ <?before '>' > | <assertion> ] }
    token assertion:sym<!> { '!' [ <?before '>' > | <assertion> ] }
    token assertion:sym<|> { '|' <identifier> }

    token assertion:sym<method> {
        '.' <assertion>
    }

    # BINEX MODIFICATION NOTE:
    # This token is fine and can be used as is
    token assertion:sym<name> {
        <longname=.identifier>
        [
        | <?before '>'>
        | '=' <assertion>
        | ':' <arglist>
        | '(' <arglist> ')'
        | <.normspace> <nibbler>
        ]?
    }


    # BINEX MODIFICATION NOTE:
    # This token is fine but the next one needs to be modified…
    token assertion:sym<[> { <?before '['|'+'|'-'|':'> <cclass_elem>+ }

    # BINEX MODIFICATION NOTE:
    # This token has been modified in the following was:
    # 1. Changed capture from $<charspec> to $<bytespec>
    # 2. Removed alternation, the :Fo classes don't make sense here
    # 3. Removed escape characters, they don't make sense here
    # 4. Shifted the \s* from inside $<bytespec> to outside for symmetry
    token cclass_elem {

        $<sign>=['+'|'-'|<?>]

        <.normspace>?

        '[' \s*

        $<bytespec>=( <word> [\s* '..' \s* <word>] ** 0..1 )*

        \s* ']'

        <.normspace>?
    }

token mod_internal {
    ':'
    [
    | <?before '!'> $<n>=('!')**1  <mod_ident> »
    | <?before \d>  $<n>=(\d+)**1  <mod_ident> »
    | <mod_ident>
    [
    '('
    [
    | $<n>=[\d+]
    | <?[']> <quote_EXPR: ':q'>
    | <?["]> <quote_EXPR: ':qq'>
    ]
    ')'
    ]**0..1
    ]
    {
        if !$<quote_EXPR> {
            my $n := $<n>[0] gt '' ?? ($<n>[0] eq '!' ?? 0 !! +$<n>[0]) !! 1;
            %*BX{ ~$<mod_ident><sym> } := $n;
        }
    }
}

proto token mod_ident { <...> }
token mod_ident:sym<ignorecase> { $<sym>=[i] 'gnorecase'? » }
token mod_ident:sym<ignoremark> {
    [
    | $<sym>=[m]
    | 'ignore' $<sym>=[m] 'ark'
    ] »
}
token mod_ident:sym<ratchet>    { $<sym>=[r] 'atchet'? » }
token mod_ident:sym<sigspace>   { $<sym>=[s] 'igspace'? » }
token mod_ident:sym<dba>        { <sym> » }
token mod_ident:sym<oops>       { {} (\w+) { self.throw_unrecognized_regex_modifier($/[0].Str) } }

method throw_unrecognized_regex_modifier($mod) {
    self.panic('Unrecognized regex modifier :' ~ $mod);
}


token word {
    # Determine the base
    $<base>=<[box]>

    # Expect a certain number of digits
    :my int $digits;
    {
        $digits = do given $<base> {
            when 'b' { $*bit       }
            when 'o' { $*bit div 2 }
            when 'x' { $*bit div 4 }
        }
    }

    # digit spacer always allowed here
    <.digit-spacer>

    [
      $<char>=(
              | <.bdigit: $<base>>
              | <?{$*scoured}> <.zero>
              )
      ** { 1 .. $*bit }
      %  <.digit-spacer>
    ]

    [ '_' { self.throw_underscore if $<char>.elems == $digits} ]?

    # R-shifting is only possible under two conditions
    # (1) scouring is enabled
    # (2) digits haven't been exhausted
    [
      <?{ $*scoured && $<char>.elems < $digits}>
      <.digit-spacer>
      [
        $<char>=<.rshift>
        ** { $digits - $<char>.elems }
        %  <.digit-spacer>
      ]
    ]?

    # Try to provide helpful advice if there's an malformed word
    [
      <?{$<char>.elems != $digits}>
      (.) {}
      {
        self.throw_scouring_not_allowed($¢[0]) if $¢[0] eq 'X'|'Z' && !$*scoured;
        self.throw_insufficient_bits($¢[0]);
      }
    ||
      '_' <.throw_underscore>
    ]?
}

method throw_insufficient-bits($) {
    self.panic('You don’t have enough bits here.');
}
method throw_underscore {
    self.panic('Underscores are only allowed inside of a token');
}
method throw_scouring_not_allowed($char) {
    self.panic("The scouring character ‘{$char}’ is not allowed here.\nPerhaps you meant to mark the token with the trait ‘is scoured’?");
}

method panic($msg) {
    die $msg;
}

token digit-spacer { '_' * }

proto token bdigit($) { * }
token bdigit:x('x') { <[0..9a..fA..F.]> }
token bdigit:o('o') { <[0..8.]>         }
token bdigit:o('b') { <[01.]>           }
token rshift { 'X' }
token zero   { 'Z' }

