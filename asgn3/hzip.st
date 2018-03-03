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

Object subclass: OutBits [
   |bitIndex currentByte myStream|
   OutBits class >> new [
      self shouldNotImplement.
   ]
   OutBits class >> new: fileStream [
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
      "stdout << 'bitIndex: ' << bitIndex."
      bitIndex = 8 ifFalse: [
         myStream nextPutByte: currentByte.
         self clearByte.
      ]
   ]
   writeBit: bit [ 
      "Transcript show: 'here'. Transcript cr."
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

   "Helper functions"
   isLeaf [^ true]

   binTreeDump: aStream [
      |num|
      aStream writeBit: 0.
      num := self char asInteger.
      8 to: 1 by: -1 do: [:index |
           |current|
           current := num bitAt: index.
           aStream writeBit: (num bitAt: index).
      ].
      num = 0 ifTrue: [
          aStream writeBit: 0.
      ].
      "Placeholder for EOF char"
      num = 256 ifTrue: [ 
          aStream writeBit: 1.
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

   "Helper functions" 
   isLeaf [^ false]
   left [^ left]
   right [^ right]

   binTreeDump: aStream [
      left binTreeDump: aStream.
      right binTreeDump: aStream.
      aStream writeBit: 1.
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

FileStream extend [
   |bitIndex currentChar|
   nextBit [
      |bit|
          bitIndex isNil ifTrue: [bitIndex := 0].
          bitIndex = 0 ifTrue: [
             bitIndex := 8.
             currentChar := self next.
          ].
          bit := currentChar value bitAt: bitIndex.
          bitIndex := bitIndex - 1.
          ^ bit. 
   ]
   atBitEnd [
      ^ (bitIndex isNil | (bitIndex = 0)) & self atEnd
   ]
   backBit [
      bitIndex := bitIndex + 1.
   ]
]

"------------------- Main program ---------------------"

Smalltalk arguments: '-c -t -d -u'
do: [:opt :arg | 
    "Compress File"
    ((opt = $c) | (opt = $t)) ifTrue: [
        '-c command' displayNl.
        
        "Parse command line files"
        inName  := Smalltalk getArgv: 2.
        outName := Smalltalk getArgv: 3.

        "No file specified, print to standard out"
        (outName isNil) ifTrue: [outName := stdout].

        inFile  := FileStream open: inName mode: FileStream read.

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

        "Place tree into encoded table" 
        tree := sortcol first.
        encoded := ZeroArray new: 257.
        tree inorder: [:char :string |
            index := char asInteger.
            encoded at: index put: string.
        ] prefix: ' '.

        outFile := FileStream open: outName. 
        
        (opt = $t) ifTrue: [
            encoded keysAndValuesDo: [:key :value | 
                |bits count lt127 gt32 empty|
                "The number of digits in count will determine the
                spacing between columns"
                
                "Helper Variables" 
                count := table at: key. 
                i := count printString size. 
                lt127 := key < 127.
                gt32 := key > 32.
                empty := count isNil. 
                
                "Print keys"
                (key == 256) ifTrue: [
                    outFile << 'EOF      ' << ' ' << count << ' '
                            << value << nl.
                ] ifFalse: [
                    (gt32 & lt127 & empty not) ifTrue: [
                        keyChar := key asCharacter.
                        outFile << ' ' << keyChar. 
                        1 to: (9-i) do: [ :x |
                            outFile << ' '. 
                        ]. 
                        outFile << count << ' ' << value << nl.
                    ]. 
                    ( gt32 not | lt127 not & empty not ) ifTrue: [
                        key1 := 
                            ((key asInteger printStringRadix: 16) copyFrom: 4).
                        outFile << 'x' << key1.
                        (key1 size == 1) ifTrue: [
                            outFile << '0'.
                        ].
                        "((key asInteger printStringRadix: 16) copyFrom: 4)." 
                        1 to: (8-i) do: [ :x |
                            outFile << ' '. 
                        ].                    
                        outFile << count << ' ' << value << nl.
                    ].
                ].
            ]. "End tree print" 
        ]."End opt t" 

        (opt = $c) ifTrue: [
            root := sortcol removeFirst.
            outBits := OutBits new: outFile. 
            tree binTreeDump: outBits. 
            outBits writeBit: 1.

            inFile reset. 
            [inFile atEnd not] whileTrue: [
                |ordChar|
                ordChar := inFile next asInteger.
                str := encoded at: ordChar.
                outBits writeString: str.
            ].

            eof := encoded at: 256.
            outBits writeString: eof.
            outBits flushByte.
            inFile close.
            outFile close.      
        ]. "End opt c" 
    ]. "End opt c/t"

    (opt = $u) ifTrue: [
        inName := Smalltalk getArgv: 2.
        inFile := FileStream open: inName mode: FileStream read.      

        outName := Smalltalk getArgv: 3.
        (outName isNil) ifTrue: [
            outFile := stdout.
        ] ifFalse: [
            outFile := FileStream open: outName.
        ]
        outFile << 'farts' << nl.

        "Helper variabes"
        byte := 0.
        singleTree := false.
        stack := OrderedCollection new.

        "Read input file and reconstruct bits" 
        [(singleTree not) & (inFile atBitEnd not)] whileTrue: [
            |bit|
            bit := inFile nextBit. 
            bit = 0 ifTrue: [ "Reconstruct byte" 
                byte := 0.
                8 to: 1 by: -1 do: [:index |
                   bit := inFile nextBit.
                   byte := byte bitAt: index put: bit. 
                ].
                byte = 0 ifTrue: [
                   bit := inFile nextBit.
                   bit = 1 ifTrue: [
                       byte := byte + 256.
                   ].
                ].
                node := Leaf new: byte asCharacter count: 1. 
                stack addFirst: node.                 
            ] ifFalse: [ "Else pop subtrees from stack" 
                stack size = 1 ifTrue: [
                    singleTree := true.
                ] ifFalse: [
                    oneSubTree := stack removeFirst.
                    zeroSubTree := stack removeFirst.
                    tree := Tree new: (oneSubTree char) count: 1
                            left: zeroSubTree right: oneSubTree.
                    stack addFirst: tree.
                ].
            ].
        ].

        "More helper variables" 
        root := stack removeFirst. 
        node := root.
        eof := false.
        outFile << 'reconstructed bits' << nl.
        "Loop over rest of input file"  
        [(eof not) & (inFile atBitEnd not)] whileTrue: [
            |bit|
            bit := inFile nextBit.

            "outFile << 'Read bit' << nl."
            
            "Traverse tree for children"
            node isLeaf ifTrue: [
                "Check if last node is eof placeholder"
                node char asInteger = 256 ifTrue: [
                    eof := true.
                ].
                outFile << 'found leaf' << nl.
            ] ifFalse: [
                outFile << 'traversing tree' << nl.

                "Write byte to file and reset pointer"
                bit = 0 ifTrue: [ "0-Link"
                    node := node left.
                ] ifFalse: [ "1-Link"
                    node := node right. 
                ].
                node isLeaf ifTrue: [
                    node char asInteger = 256 ifTrue: [
                        eof := true.
                    ] ifFalse: [
                        outFile << node char.
                        outFile << 'add node' << nl.  
                    ].
                ]. 
                node := root. 
            ].
        ]. "End of loop"
        
        outFile << 'finished' << nl.
    ]. "End opt u"
] ifError: [
    'Error: invalid option' displayN1.
]
