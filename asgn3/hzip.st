#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
"$Id: bitstream.st,v 1.6 2016-10-13 13:08:26-07 - - $"
execname := '$RCSfile: bitstream.st,v $' =~ '.*: (.*),v.*' at: 1.

nl := Character nl.

Character extend [
   isGraph [
      ^ (Character space < self) & (self <= $~)
   ]
   visible [
      self isGraph ifTrue: [^ '$', self asString]
                   ifFalse: [^ self asInteger printStringRadix: 16]
   ]
]

Object subclass: BitStream [
   |bitIndex byte myStream|
   BitStream class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   init: fileStream [
      myStream := fileStream.
      bitIndex := 1.
   ]
   nextBit [
      bitIndex = 1 ifTrue: [
         byte := myStream next.
         bitIndex := 9.
      ].
      bitIndex := bitIndex - 1.
      ^byte value bitAt: bitIndex
   ]
   atEnd [
      ^bitIndex = 1 and: [myStream atEnd]
   ]
   currByte [
      ^byte
   ]
]

"
nl := Character nl.
stdinBits := BitStream new: stdin.
count := 0.
[stdinBits atEnd] whileFalse: [
   stdout << stdinBits nextBit.
   count := count + 1.
   count = 4 ifTrue: [stdout << ' '].
   count = 8 ifTrue: [
      stdout << ' ' << stdinBits currByte visible << nl.
      count := 0
   ].
]
"

"TEST: echo hello world | ./bitstream.st 2>&1"

nl := Character nl.

Object subclass: Leaf [
   |char count|
   char [ ^ char ]
   count [ ^ count ]

   Leaf class >> new [
      self shouldNotImplement
   ]

   Leaf class >> new: aChar count: aCount [
      |result|
      result := super new.
      result setChar: aChar andCount: aCount.
      ^result
   ]

   setChar: aChar andCount: aCount [
      char := aChar.
      count := aCount.
   ]

   <= other [
      ^ (count < other count)
      | ((count = other count) & (char <= other char))
   ]

   printBase: aStream [
      ^ aStream << self class << '(' << char << ',' << count
   ]

   printOn: aStream [
      (self printBase: aStream) << ')'.
   ]

   inorder: visitor prefix: string [
      visitor value: char value: string.
   ]

]

Leaf subclass: Tree [
   |left right|

   Tree class >> new: aChar count: aCount [
      self shouldNotImplement
   ]

   Tree class >> new: aChar count: aCount left: aLeft right: aRight [
      |result|
      result := super new: aChar count: aCount.
      result setLeft: aLeft andRight: aRight.
      ^ result
   ]

   setLeft: aLeft andRight: aRight [
      left := aLeft.
      right := aRight.
   ]

   printOn: aStream [
      (self printBase: aStream) << ',' << left << ',' << right << ')'.
   ]

   inorder: visitor prefix: string [
      left inorder: visitor prefix: string, '0'.
      right inorder: visitor prefix: string, '1'.
   ]

]

"
a := Leaf new: $a count: 10.
b := Leaf new: $b count: 20.
c := Leaf new: $c count: 15.
t := Tree new: $t count: 30 left: a right: b.
u := Tree new: $u count: 50 left: t right: c.
x := Leaf new: $x count: 20.
z := Tree new: $z count: 80 left: u right: x.

sortcol := SortedCollection new.
sortcol add: t; add: u; add: a; add: b; add: x; add: z; inspect.

stdout << nl << 'Before vising z Tree' << nl.
z inorder: [:char :string |
   stdout << '[' << char << ']=' << string << nl.
] prefix: ''.

stdout << nl << 'Before sortcol do: loop' << nl.
sortcol do: [:item |
   stdout << item << nl.
].

stdout << nl << 'Before remove loop.' << nl.
[sortcol notEmpty] whileTrue: [
   |first|
   first := sortcol removeFirst.
   stdout << first << nl.
]
"
"TEST: tree.st"

"Count frequency of input characters"

Character extend [
   isPrint [
      ^ (Character space <= self) & (self <= $~)
   ]
   visible [
      self isPrint ifTrue: [^ '$', self asString]
                   ifFalse: [^ self asInteger printStringRadix: 8]
   ]
]

Object subclass: ZeroArray [
   |theArray|

   ZeroArray class >> new [
      self shouldNotImplement.
   ]

   ZeroArray class >> new: size [
      |result|
      result := super new.
      result init: size.
      ^ result
   ]

   init: size [
      theArray := Array new: size.
   ]

   size [
      ^ theArray size.
   ]

   at: index [
      ^ theArray at: index + 1.
   ]

   at: index put: value [
      ^ theArray at: index + 1 put: value.
   ]

   incr: index [
      (self at: index) isNil
         ifTrue: [ self at: index put: 0].
      self at: index put: (self at: index) + 1.
   ]

   keysAndValuesDo: aBlock [
      (0 to: (self size) - 1) do: [:index |
         aBlock value: index value: (self at: index).
      ]
   ]

]

"freqtable := ZeroArray new: 256.

[stdin atEnd not] whileTrue: [
   |ordChar|
   ordChar := stdin next asInteger.
   freqtable incr: ordChar.
].

freqtable keysAndValuesDo: [:key :value |
   (value notNil and: [value > 0]) ifTrue: [
      stdout << 'freq[' << key asCharacter visible << ']=' << value
             << nl.
   ]
].
"

"TEST: echo aaaaaaaaaa bbbbb ccc | charfreq.st"

Smalltalk arguments: '-c -t -d'
do: [:opt :arg | 
    "Compress File"
    (opt = $c) ifTrue: [
        |infile outfile table sortcol|
        '-c command' displayNl.
        "Parse command line files"
        filename  := Smalltalk getArgv: 2.
        outname := Smalltalk getArgv: 3.

        "No file specified, print to standard out"
        (outname isNil) ifTrue: [outname := stdout].

        inFile  := FileStream open: filename mode: FileStream read.
        "outFile := FileStream open: outname."
        
        Transcript show: 'iterating through file'.
        Transcript cr. 

        "Count frequency by iterating through file"
        table := ZeroArray new: 257.
        [inFile atEnd not] whileTrue: [
            |charIn|
            charIn := inFile next asInteger.
            table incr: charIn.
        ].
        table at: 256 put: 1. "EOF character" 

        "Generate leaf nodes for non-zero values"
        sortcol := SortedCollection new.
        table keysAndValuesDo: [:key :value | 
            |node|  
            (value notNil and: [value > 0]) ifTrue: [
                node := Leaf new: (key asCharacter) count: value.
                sortcol add: node. 
            ]
        ].        
        "Build tree based on frequency"
        [sortcol size > 1] whileTrue: [
            |first second tChar tCount|
            first  := sortcol removeFirst.
            second := sortcol removeFirst.
            tChar  := first char.
            tCount := first count + second count + 1.
            sortcol add: 
                (Tree new: tChar count: tCount left: first right: second).
        ].

        Transcript show: 'built tree'. 
        Transcript cr.

        "Place tree into encoded table" 
        tree := sortcol first.
        encoded := ZeroArray new: 257.
        tree inorder: [:char :string |
            index := char asInteger.
            encoded at: index put: string.
        ] prefix: ' '.

        outBits := BitStream new: outFile.

        Transcript show: 'encoding tree'.
        Transcript cr.

        "Encoding stuff here"

        Transcript show: 'exiting'.
        Transcript cr. 
"
        inFile close.
        outFile close.        
"
    ].

    "Print Tree"
    (opt = $t) ifTrue: [
        
    ].

    "Uncompress File"
    (opt = $u) ifTrue: [
        
    ].
]
ifError: [
    'Error: invalid option' displayN1.
]
