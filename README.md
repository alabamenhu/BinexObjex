# Binex & Objex

Binex is a first start at creating a binary grammar system for Raku that’s as powerful for blobs as regex is for strings.

To try it out, simply do:
 
```raku
use Binex;
 
my $binex = bx 'b...._...1';
my $blob = blob8.new: 1,3,5,7,9,10,11,13; 
$blob ~~ $binex;

say $/.Blob;
# Blob[uint8]:0x<01 03 05 07 09 08 00>
 ```
Right now it's at least moderately usable. 
Currently supported:
  - 8-bit blobs/tokens
  - Non-capturing grouping `[ ]`
  - Quantifiers `+`, `?`, `*` 
    - Works with `%` and `%%`)
  - Backtracking (yay)
  - Option `:s[couring]` will allow in-line changes to data
    - Use `Z` to *zero out* the given bit/crumb/nibble
    - Use `X` to *right shift* the given bit/crumb/nibble out.
    - Conjunctions `&` and `&&` not available due to ambiguity.
    - Only partially supported: 
      - Complex expressions might not scour correctly because each node has to be configured to supported it at the moment, and only the basic ones do.
    

Todo list:
  - Make code not ugly (please don't judge!)
  - Add support for captures
  - Make it work for 16+ bit (assumes 8-bit always for now)
    - Make it understand endianness
  - Add support for inline code
  - Improve support for `|`, `&`, and `&&`
    - `|` currently chooses the longest match, rather than longest-token-match.  If two options have the same length, result is non-deterministic
    - `&` and `&&` currently die if, run separately, they do not have the same length (quantifiers should rerun to find a best-fit match).
  - Add support for character classes
  - Add support for method calls and embedded tokens
  - Make the interface look identical to regex.
  - Optimize, optimize, optimize

## Version history

 - 0.1.0: Initial release