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
   |bitIndex currentByte myStream|
   BitStream class >> new [
      self shouldNotImplement.
   ]
   BitStream class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   clearByte [
      bitIndex := 8.
      currentByte := 0.
   ]
   init: fileStream [
      myStream := fileStream.
      self clearByte.
   ]
   flushByte [
      bitIndex = 8 ifFalse: [
         myStream nextPutByte: currentByte.
         self clearByte.
      ]
   ]
   writeBit: bit [ 
      Transcript show: 'here'. Transcript cr.
      currentByte := currentByte bitAt: bitIndex put: bit.
      bitIndex := bitIndex - 1.
      bitIndex = 0 ifTrue: [self flushByte].
   ]

   writeString: string [
      string do: [:index |
          index asString = '0'
             ifTrue: [self writeBit: 0]
             ifFalse: [self writeBit: 1].
      ].
   ] 
]

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

   binTreeDump: myStream [
      |num|
      myStream writeBit: 0.
      num := char asciiValue.
      8 to: 1 by: -1 do: [:index |
           |current|
           current := num bitAt: index.
           myStream writeBit: current.
      ].
      "Placeholder for EOF char"
      num = 256 ifTrue: [ 
          myStream writeBit: 1.
      ].
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

   left [^ left]
   right [^ right]

   binTreeDump: myStream [
      left binTreeDump: myStream.
      right binTreeDump: myStream.
      myStream writeBit: 1.
   ]
]

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

Smalltalk arguments: '-c -t -d -u'
do: [:opt :arg | 
    "Compress File"
    (opt = $c) | (opt = $t) ifTrue: [
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
 
        " ------DEBUG---------  
          Print Sorted Collection: Expand to make -t opt"     
        "sortcol keysAndValuesDo: [:key :value | 
           |bits keyChar|
           bits := key printString: 2.
           keyChar := key asCharacter.
           stdout << keyChar << ' ' << (value count) << ' ' 
                  << bits << nl.
        ]."        

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
        
        Transcript show: 'option to print tree'.
        Transcript cr.

        (opt = $t) ifTrue: [
            "Check encoded table's stuff
            This can be extended into being full -t option"
            encoded keysAndValuesDo: [:key :value | 
                |bits count lt127 gt32 empty|
                count := table at: key.
                "The number of digits in count will determine the
                spacing between columns"
                i := count printString size. 
                lt127 := key < 127.
                gt32 := key > 32.
                empty := count isNil. 
                (key == 256) ifTrue: [
                    stdout << 'EOF      ' << ' ' << count << ' '
                    << value << nl.
                ] ifFalse: [
                    (gt32 & lt127 & empty not) ifTrue: [
                        keyChar := key asCharacter.
                        stdout << ' ' << keyChar. 
                        1 to: (9-i) do: [ :x |
                            stdout << ' '. 
                        ]. 
                        stdout << count << ' ' << value << nl.
                    ]. 
                    ( gt32 not | lt127 not & empty not ) ifTrue: [
                        key1 := ((key asInteger printStringRadix: 16) copyFrom: 4).
                        stdout << 'x' << key1.
                        (key1 size == 1) ifTrue: [
                            stdout << '0'.
                        ].
                        "((key asInteger printStringRadix: 16) copyFrom: 4)." 
                        1 to: (8-i) do: [ :x |
                            stdout << ' '. 
                        ].                    
                        stdout << count << ' ' << value << nl.
                    ].
                ].
            ].
        ].
        (opt = $c) ifTrue: [
        outBits := BitStream new: outFile. 
        tree binTreeDump: outBits. 

        Transcript show: 'passed dump'. Transcript cr. 
        outBits writeBit: 1.

        Transcript show: 'compressing'.
        Transcript cr.

        inFile reset. 
        [inFile atEnd not] whileTrue: [
            |ordChar|
            ordChar := inFile next asInteger.
            str := encoded at: ordChar.
            outBits writeString: str.
        ].

        Transcript show: 'exiting'.
        Transcript cr. 

        eof := encoded at: 256.
        outBits writeString: eof.
        outBits clear. 
        inFile close.
        outFile close.      
        ].  
    ].
]
ifError: [
    'Error: invalid option' displayN1.
]
