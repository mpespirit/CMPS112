#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
"$Id: bitstream.st,v 1.6 2016-10-13 13:08:26-07 - - $"
execname := '$RCSfile: bitstream.st,v $' =~ '.*: (.*),v.*' at: 1.

nl := Character nl.

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
          index asString = '0' ifTrue: [
              self writeBit: 0
          ] ifFalse: [
              self writeBit: 1
          ].
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

   binTreeDump: myStream [
      |ascii|
      myStream writeBit: 0.
      ascii := char asciiValue.
      8 to: 1 by: -1 do: [:index |
           |current|
           current := ascii bitAt: index.
           myStream writeBit: current.
      ].
      "Placeholder for EOF char"
      ascii = 256 ifTrue: [ 
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

   "Helper functions" 
   isLeaf [^ false]
   left [^ left]
   right [^ right]

   binTreeDump: myStream [
      left binTreeDump: myStream.
      right binTreeDump: myStream.
      myStream writeBit: 1.
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
]

"------------------- Main program ---------------------"

Smalltalk arguments: '-c -t -d -u'
do: [:opt :arg | 
    "Compress File"
    inName := Smalltalk getArgv: 2. 
    outName := Smalltalk getArgv: 3.

    ((opt = $c) | (opt = $t)) ifTrue: [
        inFile := FileStream open: inName mode: FileStream read. 

        "Count frequency by iterating through file"
        table := ZeroArray new: 257.
        [inFile atEnd not] whileTrue: [
            |ordChar|
            ordChar := inFile next asInteger.
            table incr: ordChar.
        ].
        table at: 256 put: 1. "EOF character" 

        "Generate leaf nodes for non-zero values"
        sortcol := SortedCollection new.
        table keysAndValuesDo: [:key :value | 
            |node|
            (value notNil and: [value > 0]) ifTrue: [
                node := Leaf new: key asCharacter count: value.  
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
        ] prefix: ''.
        
        (opt = $t) ifTrue: [
            (opt = $d) ifTrue: ['-t command' displayNl].
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
                        key1 := 
                            ((key asInteger printStringRadix: 16) 
                              copyFrom: 4).
                        stdout << 'x' << key1.
                        (key1 size == 1) ifTrue: [
                            stdout << '0'.
                        ].
                        1 to: (8-i) do: [ :x |
                            stdout << ' '. 
                        ].                    
                        stdout << count << ' ' << value << nl.
                    ].
                ].
            ]. "End tree print" 
        ]."End opt t" 

        (opt = $c) ifTrue: [
            (opt = $d) ifTrue: ['-c command' displayNl].
            
            "If no file specified, print to stdout" 
            outName isNil ifTrue: [
                outFile := stdout. 
            ] ifFalse: [
                outFile := FileStream open: outName.
            ].

            outBits := OutBits new: outFile.
            tree binTreeDump: outBits. 
            outBits writeBit: 1.

            inFile reset.
 
            [inFile atEnd not] whileTrue: [
                str := encoded at: inFile next asInteger.
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
        |stack|
        (opt = $d) ifTrue: ['-u command' displayNl].

        inFile := FileStream open: inName mode: FileStream read.      
        outName isNil ifTrue: [
            outFile := stdout.
        ] ifFalse: [
            outFile := FileStream open: outName.
        ].

        (opt = $d) ifTrue: [outFile << nl << 'begin' << nl].

        "Helper variabes"
        stack := OrderedCollection new.
        singleTree := false.
        aByte := 0.
        "Read input file and reconstruct bits" 
        [(singleTree not) & (inFile atBitEnd not)] whileTrue: [
            |aBit|
            aBit := inFile nextBit. 
            aBit = 0 ifTrue: [ "Reconstruct byte" 
                aByte := 0.
                8 to: 1 by: -1 do: [:index |
                   aBit := inFile nextBit.
                   aByte := aByte bitAt: index put: aBit. 
                ].
                aByte = 0 ifTrue: [
                   aBit := inFile nextBit.
                   aBit = 1 ifTrue: [
                       aByte := aByte + 256.
                   ].
                ].
                node := Leaf new: aByte asCharacter count: 1. 
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
        (opt = $d) ifTrue: [outFile << 'reconstructed bits' << nl].

        "Loop over rest of input file"  
        [(eof not) & (inFile atBitEnd not)] whileTrue: [
            |aBit|
            aBit := inFile nextBit. 
            
            "Traverse tree for children"
            node isLeaf ifTrue: [
                "Check if last node is eof placeholder"
                node char asInteger = 256 ifTrue: [
                    eof := true.
                ].
                (opt = $d) ifTrue: [outFile << 'found leaf' << nl].
            ] ifFalse: [
                (opt = $d) ifTrue: [outFile << 'traversing tree' << nl].

                "Write byte to file and reset pointer"
                aBit = 0 ifTrue: [ "0-Link"
                    node := node left.
                ] ifFalse: [ "1-Link"
                    node := node right. 
                ].
                node isLeaf ifTrue: [
                    node char asInteger = 256 ifTrue: [
                        eof := true.
                    ] ifFalse: [
                        outFile << node char.
                        (opt = $d) ifTrue: [outFile << 'add' << nl].  
                    ].
                    node := root.
                ]. 
            ].
        ]. "End of loop"
        
        (opt = $d) ifTrue: [outFile << 'finished' << nl].
    ]. "End opt u"
] ifError: [
    'Error: invalid option' displayN1.
]
