# The making of Binex

While this is partly an exploration of how to create a binary grammar engine for Raku, it is also an exploration of Raku's regex engine.
It is written as much to explore the beauty of Raku as a language, as it is to aid others who may be interested in contributing to the regex (or binex or objex) code.
While it goes in depth on the grammars and actions, it does not describe the core regex code, as that is compiled directly to VM code and a bit more impenetrable.
My initial Binex code, written entirely in Raku, will be nowhere near as efficient (but maybe if you're reading this in 2030 it will be) but hopefully illustrative of how much work has gone into making regex so functional in Raku.

## Introduction 
### What is regex?

Regex, from *regular expressions*, have come a long way from their origin.
Early versions had lots of limitations, such as not allowing back references, inlining code, or being recursive.
Perl, Raku’s sister language, held the crown for a very long time as the most advanced and common regex engine, to the point that its syntax was adopted by most other engines.
Raku changed a lot of the syntax to avoid the line noise that regex quickly developed (anyone who did regex to capture sequences of slashes knows how bad it can be).

### What is binex?

Although Raku’s regex is awesome, it is not great for parsing binary data.
The main reason is that Raku assumes that we're working with textual data (in fact, internally, it will convert anything that *isn't* a string into one before processing it).

But sometimes we actually *want* to process binary data.
There are workarounds (manually decoding the data into a string avoid Unicode issues) but they're clunky, and not optimal.

Enter Binex, which aims to allow the same ease of development for binary formats as Regex does for textual ones.

## Part 1: What do we want?

I created a proposal that describes a lot of the decisions here.
For the rest of the document, suffice it to say that there are three major changes from regex to binex:

  1. Matches are done on binary data in 8-, 16-, 32-, or 64-bit increments, expressed as `b0001_1011`, or `o0123`, or `x36` (for 8-bits; internal underscores are optional).
  2. The characters `.` (match anything), `X` (right shift), and `Z` (zero out) have special meaning.
     - The latter two are opted in, as they modify the match value.
  3. Strings and string-y metas (`\w`) are disallowed, because they would have no meaning.
  
Otherwise, everything else -- quantifiers, groups, captures, etc -- works basically works as expected.
For the most part, adding binary grammars is about as simple as what it would seem by defining it here: only minor changes to the core definition of tokens, and that's mostly right.

## Part 2: $binex = $regex

Because the goal is to make binex as close to regex as possible, the best thing to do is to start from Raku's core regex definition.
This is easier said than done: regex stuff is spread across both Raku (the core) and Rakudo (the compiler).
The main stuff we want to look at now, though, is in [`QRegex/P6Regex/Grammar.nqp`](https://github.com/Raku/nqp/blob/master/src/QRegex/P6Regex/Grammar.nqp).
There are two main sections to this file. 
First we have the `HLL::World` subclass which we'll conveniently ignore for now.
Next, however, is the big gigantic grammar.

There are a *lot* of methods and tokens with [corresponding actions](https://github.com/Raku/nqp/blob/master/src/QRegex/P6Regex/Grammar.nqp), and it's not useful to go through them all.
Let's focus instead on the most basic core element: just like the optimal approach for any other grammar.

## Part 3: Going nuclear

The *core* of the regex grammar is a token called `atom`.  It has two main components, a grammar and an action, which we'll work with here.

### The grammar for \<atom\>

Let's take a look at the original definition:

```raku
    token atom {
        # :dba('regex atom')
        [
        | \w
          [ <?before ' ' \w <!before <.quantifier>>  > <!{ %*RX<s> || $*HAS_GOAL }> <.worry("Space is not significant here; please use quotes or :s (:sigspace) modifier (or, to suppress this warning, omit the space, or otherwise change the spacing)")> ]?
          <.SIGOK>
        | <metachar>
        ]
    }
```

If you've used regex a lot, this should make a lot of sense to you.  
In between `//`, you can immediately type in word characters (`\w`) or you can do other, multi-character things like quotes, character classes, embedded blocks, etc, which will all be matched/handled in the `<metachar>` token.
Although it's not required at all, we try to be nice.  When some does `/a dog/`, we remind them that this is equivalent to `/adog/`. 
The `<.SIGOK>` token is used to adjust a dynamic variable that indicates whether space should currently be considered significant or not.
The method call `:dba()` (doing business as) can also be ignored here, as it's for debugging purposes.

This is token is where the core of binex will be working, but I'll eventually get to the other tokens later for those more interested in regex generally.
The original regex definition here gets to cheat with `\w`: we've determine that the core of binex will be a format defining bytes or words. 
On the other hand, we have it easier in that we don't need to keep track of significant space: it is *never* significant for binex. 
So we can whittle down the definition to:

```raku
    token atom {
        [
        | <word>
        | <metachar>
        ]
    }
```

Simple, but now we need to define a word.
That is a bit more complicated than it may look.
We have to expect an exact number of digits, but that number is based on the base (binary, octal, or hex) and the word length (8-, 16-, 32-, or 64-bit).
This is the word definition I came up with:

```raku
token word {
    # Determine the base
    $<base>=<[box]>

    # Expect a certain number of digits
    :my int $digits;
    {
        $digits = do given $<base> {
            when 'b' { $*bits     }
            when 'o' { $*bits / 2 }
            when 'x' { $*bits / 4 }
        }
    }

    # digit spacer always allowed here
    <.digit-spacer>

    [
      $<char>=[
              | <.digit($<base>)>
              | <?{$*scoured}> <.zero>
              ]
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

    # Final checks: must have enough digits, and no trailing underscores
    { self.throw_insufficient_bits($<char>.join) if $<char>.elems != $digits }
    [ '_' { self.throw_underscore } ]?
}

token digit-spacer { '_' * }

proto token digit($) { * }
token digit('x') { <[0..9a..fA..F.]> }
token digit('o') { <[0..8.]>         }
token digit('b') { <[01.]>           }

token rshift { 'X' }
token zero   { 'Z' }
```

I like to comment a lot, as you can see, but I hope it helps you to follow the code better.
One \[dis\]?advantage of dynamic variables is sometimes you get into methods and you don't see where a variable is defined.
In this case, it's `$*bit`, which a token can only know if it's passed in from above.

### The action for \<atom\>
There are two next steps. 
 
We need an action for `atom` and `word` and we need to generate something with them.  

## Quantum mechanics

Although `<atom>` also has a `<metachar>` option, we'll leave that one behind for now, and progress up the chain to `<quantified-atom>`.
This will let us match using options such as `+`, `*`, `?`, and `** min [.. max]`.
The original definition in the grammar is

```raku
    token quantified_atom {
        <!rxstopper>
        <atom>
        [
        ||  <sigmaybe>?
            [
            | <!bxstopper> <quantifier>
            | <?[:]> <backmod> <!alnum>
            ]
            [ <!{$*VARDEF}> <.SIGOK> <sigfinal=.sigmaybe> ]?
            [ <.ws> <separator> ]?
        ||  [ <!{$*VARDEF}> <sigfinal=.sigmaybe> ]?
        ]
        { $*SIGOK := 0 }
    }
```

The `<!rxstopper>` allows syntax like `m!foo!`, and represents the character that will terminate a regex.
Here we see some evidence of the significant-spacing concerns that complicate the way that regex syntax is parsed.
After 

To convert this token to an appropriate binex token, we can strip out a lot of the spacing code and just allow regular `<normspace>` (which handles comments too):

```raku 
    token quantified_atom {
        <!rxstopper>
        <atom>
        [
        ||  <.normspace>?
            [
            | <!bxstopper> <quantifier>
            | <?[:]> <backmod> <!alnum>
            ]
            [ <!{$*VARDEF}> <.SIGOK> <sigfinal=.sigmaybe> ]?
            [ <.ws> <separator> ]?
        ||  [ <!{$*VARDEF}> <sigfinal=.sigmaybe> ]?
        ]
        { $*SIGOK := 0 }
    }
```

The quantifier, of course, will be what determines how many we need, and we'll deal with shortly.
Next we have the `<backmod>` which handles things like ratcheting (`<quantier>` also includes `<backmod>`).
`$*VARDEF` is set if we're in the middle of defining a regex as a different type, for example, in saying `$<foo>=<bar>`.

The more interesting bit comes in the actions method.
That is where we get to see the first bit of interesting regex building. 
Let's explore the original regex in piecese

```raku
method quantified_atom($/) {
    my $qast := $<atom>.made;
    ...
}
```

The main building block is the original atom.
Next we get to some spacing stuff:

```raku 
    my $sigmaybe := $<sigmaybe>.ast if $<sigmaybe>;
    $qast := QAST::Regex.new(:rxtype<concat>, $qast, $sigmaybe) if $sigmaybe;
```

For binex, we won't need any of this.
But for understanding the internals of the regex engine, it is interesting: 
The regex type 'concat' takes a sequence of regexes.
In this case, if the `<sigmaybe>` token was found, it (itself of type *subrule* which will be explained later) gets included along with the the original literal. 

```raku
    if $<quantifier> {
        $/.panic('Quantifier quantifies nothing')
            unless $qast;
        my str $bxtype := $qast.bxtype;
        $/.throw_non_quantifiable()
            if $bxtype eq 'qastnode' || $bxtype eq 'anchor';
        my $ast := $<quantifier>.made;
        $ast.unshift($qast);
        $qast := $ast;
    }
```

The next section deals with the quantifier.
If for some reason we got a spare quantifier without an atom, we check it.
Next we check if the type isn't quantifiable and throw a helpful message (you can't have multiple anchors, of course!)
Lastly, we get `<quantifier>`'s regex node, and unshift our atom into.
All regex nodes that contain other nodes can have them added with the "standard" methods of `unshift` or `push` (that is not unique to regex, it is a behavior of the more generic role `QAST::Children`).

Of course, if there isn't a quantifier, nothing changes at all. 
Also, at this point you might be wondering about the coding style. 
This is extremely hot code, and so allocations are designed to be minimized through binding and reuse of containers.
It doesn't look super Raku-ish (and technically it's NQP, not Raku), but I digress.

Next we deal with something that we don't normally think of as being a quantifier:

```raku
    if $<separator> {
        if $qast.rxtype ne 'quant' && $qast.rxtype ne 'dynquant' {
            $/.panic("'" ~ $<separator><septype> ~
                    "' may only be used immediately following a quantifier")
        }
        $qast.push($<separator>.made);
        if $<separator><septype> eq '%%' {
            $qast := QAST::Regex.new( :rxtype<concat>, $qast,
                    QAST::Regex.new( :rxtype<quant>, :min(0), :max(1), $<separator>.made ));
        }
    }
```
We don't normally think of the `%` (and `%%`) operator as being quantifiers, but they can only work in conjunction with them.
For this reason, they were included in the grammar as a part of the `quantified_atom`
The `concat` Regex will alternate back and forth when it detects a separator.
Personally, I think it would have made more sense to rewrite something of the sort `a* % b` as `[a [[b a]*]?` and `a* %% b` as `[a [b a]* b?]?`, but that might be handled in the optimization phase, but there might be a performance reason I'm not aware of (\*awaits jnthn's explanation\*).

In any case, whereas one might think that writing the quantifier's code would be easy, it's actually surprisingly complex.
Whereas with literal element we could live in our own little bubble, the quantifier becomes more complex:

  - The quantified token might not be literal (wait, we can't handle those yet!)
  - The separator token might not be literal (wait, we can't handle those yet!)
  - The quantified or separator tokens might need to capture (wait, when did we talk about captures!?)
  - The quantifier might be greedy or frugal, and ratcheting or not. (did we bite off more than we can chew?).
  - A binex of type concat will be necessary (this feels like it might get a bit circular...).
    
This requires us to solve quite a few different issues, all at once.

The first three should be solved easily once we handle non-literal tokens.
The greedy/frugal/ratching question will also need to wait.
Let's focus on beginning the concatenated binex first (and it will also show where Binex, normally simpler, will complicate things a bit).
We can do a super naïve version with literals fairly easily:

```raku
class BXConcat does Binex {
    has @.children;
    multi method MATCH(Blob \orig, int \from = 0, int \to = orig.elems --> BXMatch) {
        my $pos = from;
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
            for @!children -> \child {
                my $match := child.MATCH(orig, $pos, to);
                return Nil unless $match;
                $pos = $match.to;
            }
            return BXMatch.new: orig => orig, from => from, to => $pos;
        }
    }
}
```
  
Basically, checking each child, and advance.
Fail with `Nil` if any child doesn't match.
If we are scoured, our match object must be scoured, and will include any scoured data as well.
Per my proposal, scoured data will trickle up so long as each item declares itself scoured.
The reason here should be obvious: a standard match need only pass around two integers (`from`/`to`) and the reference to `orig` stays in tact and never changes.
But for a scoured match, we must allocate a brand new Blob for *each* match object, and if each token contains two more tokens which contain two more, we might end up using far more reasons.

Now that we have the initial concatenation type done (we'll be back), we can tackle the `<quantified-atom>` again.
This binex type will repeat some number of times.
The easiest way to handle it is to assume it's ratcheting:




# Regex Internal Quick Reference

Regex types:
  - **literal**: matches actual text.  Single payload
  - **concat**: matches several regexen in a row
  - **quant**: matches an item a number of times.  If payload has more than one, alternates, but must start/end with the first.
  - **dynquant**: See *quant*, but number to be determined at run time
  - **subrule**: calls something else, either a method (**subtype\<method\>**) or another regex (**subtype\<regex\>**)
  - **anchor**: non capturing, defined by the start or end of a string (or of a line).

Tokens
  - **\<atom\>**: base element, either literal or a special meta 
  - **\<quantified_atom>**: handles both quantification (`?`, `!`, `+`, `**`) *and* the separators (`%`, `%%`)
  - **\<sigmaybe\>**: inserts a `<ws>` token (for `rule`s)