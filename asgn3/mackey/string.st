#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
"$Id: string.st,v 1.5 2015-11-13 14:42:01-08 - - $"

nl := Character nl.

s := 'Hello world'.
stdout << s << nl.

(1 to: s size) do: [:i|
   |c ci|
   ci := s byteAt: i.
   cc := ci asCharacter.
   stdout << i << ' ' << ci << '=' << cc << ';' << nl.
   (cc = $o) ifTrue: [ stdout << 'True' << nl. ].
]

