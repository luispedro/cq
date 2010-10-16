* Markup Library

This is MIT Licensed

** Instalation

Just use "make". It needs ghc, but only uses standard Haskell libraries (I
believe that Parsec is shipped with ghc). It was tested on stock Ubuntu 10.10.

** Running it

   ./markup FILENAME

** Library Interface

The main structures are these. Usage is through pattern matching

   data Text = RawText String
       | LinkWKey String String -- link key
       | Link String -- link
       | InlineTag String [Text] -- tag str
       | BlockTag String [Element] -- tag str
       | Sequence [Text]

   data Element = Paragraph Text
       | Verbatim String
       | UList [Element]
       | OList [Element]
       | UListElement [Element]
       | OListElement [Element]
       | Block [Element]
       | Header Integer String
       | LinkDef String String

Parse.hs has a single entrypoint:

   parseMarkup :: String -> Either ParseError Document

** Implementation

See Parse.hs for a discussion.

** Notes

This passes all the tests, but it is not really finished (I learned about the
Challenge only this week).

My main issue (which took me about 24 hours, several of those coding) was
realising that it could all be done very easily if we had saved the indentation
level and the nesting level on the parser state. I had toyed with a two level
parsing scheme which was line-based where lines knew there indentation and a
second parser would then parse those lines, but it was becoming a mess.

Also, putting some of the dumber processing (eol normalisation &c) as a
separate function which takes advantage of Haskell's laziness made of the rest
of the code much easier.

Finally, this was my first larger piece of code in Haskell (I started learning
it a few weeks back by working on Project Euler), so I was still struggling
with it: for a while I hadn't realised that my unit tests weren't running
because I wasn't forcing evaluation! So it all passed even when the main
programme seemed broken.

According to my time tracker, I spent exactly 7 hours on this project over the
last 3 days. This does not count the time when I was thinking about this on the
way to lunch and finally realised that the two parser solution was idiotic.

** Author

Luis Pedro Coelho <luis@luispedro.org>
http://luispedro.org

