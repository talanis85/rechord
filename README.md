![Demo](https://raw.github.com/talanis85/rechord/master/gfx/demo.png)

reChord
=======

reChord is a command line tool to produce pretty-printed chord sheets from
ChordPro-like text files.

Usage:

`rechord render [-k <key>] [-t <transposition>] <inputfile.crd> <outputfile.pdf>`

Build instructions
------------------

`stack build`

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

The only directives supported for now are:

- Song title
    `{t:Raindrops Keep Falling On My Head}`
- Song key
    `{key:F}`

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

Chord ::= AbsoluteChord | RelativeChord

AbsoluteChord ::= "[", ChordName, { "/", Pitch } "]"

RelativeChord ::= "[", Degree, [ Type ], { "/", Degree } "]"

Degree ::= DegreeBase, ( Accidental )*

ChordName ::= Pitch, [ Type ]

Pitch ::= ( "A" | "B" | "C" | "D" | "E" | "F" | "G" ), ( Accidental )*

Accidental ::= "#" | "b"

Type ::= "m" | "m7" | "maj7" | etc...

Roadmap
-------

* Configurable style
