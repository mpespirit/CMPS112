#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
"$Id: char.st,v 1.2 2015-11-13 14:51:58-08 - - $"

nl := Character nl.

(0 to: 255) do: [:i|
   |c|
   c := i asCharacter.
   "stdout nextPut: c."
   stdout nextPutByte: i.
]
