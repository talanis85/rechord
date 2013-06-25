![Demo](https://raw.github.com/talanis85/rechord/master/gfx/demo.png)

reChord
=======

reChord is a command line tool to produce pretty-printed chord sheets from
ChordPro-like text files.

Usage:

`rechord <inputfile.cho> <outputfile.pdf>`

Build instructions
------------------

`cabal configure`  
`cabal build`

Input format
------------

### Lyrics and Chords

The input format is a variant of the ChordPro format. Essentially, it
looks like this:

`[F]Raindrops keep falling on my [Fmaj7]head.`  
`And [F7]just like the guy whose feet are [Bb]too big for his [Am]head`

So, basically, it's just lines of lyrics with chords written in brackets.

### Annotations

Annotations are denoted with angle brackets. They can be placed anywhere
in the song and will be printed in a distinct style. Example:

`<BRIDGE>`  
`But there's one [F]thing I [Fmaj7]know`  
`The [Bb]blues they send to [C]meet me won't de[Am]feat me`  
`It won't be long till [D7]happiness steps [Gm7]up to greet me <Instrumental Break>`

### Directives

The only directive supported for now defines the title of the song:

`{t:Raindrops Keep Falling On My Head}`

Unlike ChordPro, reChord expects directives only at the beginning of an
input file.

### EBNF

Song ::= { Directive }, Paragraphs

Paragraphs ::= Paragraph, { "\n", Paragraphs }

Directive ::= "{", Key, ":", Value, "}", Newline

Paragraph ::= Line, { Line }

Line ::= ( Lyrics | Annotation | Chord ), { ( Lyrics | Annotation | Chord ) }

Lyrics ::= String

Annotation ::= "<", String, ">"

Chord ::= "[", ChordName, "]"

ChordName ::= ( "A" | "B" | "C" | "D" | "E" | "F" | "G" ), [ ( "#", "b" ) ], [ Modifier ]

Modifier ::= String

Roadmap
-------

* Configurable style
* Automatic transposition
