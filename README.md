# Prototype for doc comment parsing

This is a minimum working example tinkering with the doc comment parsing idea originally described at https://github.com/mrkkrp/megaparsec/issues/428 ,

As explored with this repository, it's found that Megaparsec's
trailing-space-consuming idiom is much nicer than leading-space-consuming.

And an followup issue https://github.com/mrkkrp/megaparsec/issues/429 that
how doc comment at eof can be gracefully dropped as white spaces. It is solved
with help from Olaf.

Used to reproduce issues:

- https://stackoverflow.com/questions/64522568/why-optional-in-a-parser-can-err-out
- https://stackoverflow.com/questions/64524585/how-to-encode-nomatch-after-consumed-some-input-in-a-parser
