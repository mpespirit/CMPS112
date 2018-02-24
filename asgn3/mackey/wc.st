#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
"$Id: wc.st,v 1.5 2016-11-14 16:31:24-08 - - $"

chars := 0.
lines := 0.

[stdin atEnd not] whileTrue: [
   |byte|
   byte := stdin next.
   chars := chars + 1.
   (byte = Character nl) ifTrue: [ lines := lines + 1 ].
].

stdout << lines << ' ' << chars << Character nl.

