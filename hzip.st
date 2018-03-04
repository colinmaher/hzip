#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f

execname := '$RCSfile: hzip.st,v $' =~ '.*: (.*),v.*' at: 1.

nl := Character nl.

Character extend [
  isPrint [
    ^ (Character space < self) & (self <= $~)
  ]
  visible [
    self isPrint ifTrue: [^ self asString]
    ifFalse: [
      (self asInteger) = 256 ifTrue: [^ 'EOF']
      ifFalse: [
        |hexstr s hexint|
        hexstr := (self asInteger) printStringRadix: 16.
        s := hexstr size.
        hexstr := hexstr copyFrom: 4 to: s.
        (self asInteger < 16) ifTrue: [^'x0', hexstr.]
        ifFalse: [^'x', hexstr.].
      ].
    ].
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
      bitIndex = 8 ifFalse: [
         myStream nextPutByte: currentByte.
         self clearByte.
      ]
   ]
   writeBit: bit [
      currentByte := currentByte bitAt: bitIndex put: bit.
      bitIndex := bitIndex - 1.
      bitIndex = 0 ifTrue: [self flushByte].
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
  postorder: visitor [
    visitor value: char value: self.
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
  left [^left]
  right [^right]
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

  postorder: visitor [
    left postorder: visitor.
    right postorder: visitor.
    visitor value: char value: self.
  ]
]

freqtable := ZeroArray new: 257.
freqtable incr: 256.

t := false.
d := false.
u := false.
c := false.
argcount := 0.
|infilearg|
infile := FileStream new.
outfile := stdout.

Smalltalk arguments: '-d -t -u -c'
do: [:opt :arg | 
  opt = $t ifTrue: [ t := true ].
  opt = $d ifTrue: [ d := true ].
  opt = $u ifTrue: [ u := true ].
  opt = $c ifTrue: [ c := true ].

  (arg ~= nil) ifTrue: [
    argcount := argcount + 1.
    (argcount = 1) ifTrue: [[
      infilearg := arg.
      infile := FileStream open: arg mode: FileStream read.
    ] on: SystemExceptions.FileError do: [:signal |
        |errno|
        errno := File errno.
        stdout flush.
        stderr << execname << ': ' << arg << ': '
                << signal messageText << nl.
        stderr flush.
      ]
    ].
    argcount = 2 ifTrue: [[
      outfile := FileStream open: arg mode: FileStream write.
    ] on: SystemExceptions.FileError do: [:signal |
         |errno|
         errno := File errno.
         stdout flush.
         stderr << execname << ': ' << arg << ': '
                << signal messageText << nl.
         stderr flush.
    ]
    ].
  ]
]
ifError: [
  stdout << 'Error: invalid option' << nl.
].

sortcol := SortedCollection new.

(t | c) ifTrue: [ 
  [infile atEnd not] whileTrue: [
    ordChar := infile next asInteger.
    freqtable incr: ordChar.
  ].
  infile close.
  
  freqtable keysAndValuesDo: [:key :value |
    (value notNil and: [value > 0]) ifTrue: [
      sortcol add: (Leaf new: key count: value).
    ]
  ].  

  [sortcol notEmpty] whileTrue: [ 
    left := sortcol first.
    sortcol removeFirst.
    (sortcol notEmpty) ifTrue: [
      right := sortcol first.
      sortcol removeFirst.
      tree := Tree new: left char count: (left count + right count) 
        left: left right: right.
      sortcol add: tree.
    ]
    
    ifFalse: [
      htree := left.
    ].
  ].

  encodedbits := Dictionary new.

  htree inorder: [:char :string |
    encodedbits at: char put: string.    
  ] prefix: ''.

  (t) ifTrue: [
    freqtable keysAndValuesDo: [:key :value |
      (value notNil and: [value > 0]) ifTrue: [
        keystr := (key asCharacter visible).
        keystr asString size to: 1 do: [:skip| outfile << ' '].
        outfile << keystr.
        (keystr asString size = 1 ) ifTrue: [outfile << ' '].
        2 to: 9 - (value asString size) do: [:skip| outfile << ' '].
        outfile << value. 
        outfile << '  ' << (encodedbits at: key) << nl. 
      ]
    ].  
    outfile close.
  ].

  (c) ifTrue: [
    encodedstr := ''.
    
    htree postorder: [:char :this |
      (this isMemberOf: Tree) ifTrue: [ encodedstr := encodedstr, '1' ]
      ifFalse: [
        binstr := '0'.

        (char < 256) & (char > 0) ifTrue: [
          8 to: 1 by: -1 do: [:index |
            binstr := binstr, (char bitAt: index) asString.
          ].
        ].

        (char = 0) ifTrue: [
          8 to: 1 by: -1 do: [:index |
            binstr := binstr, (char bitAt: index) asString.
          ].
          binstr := binstr, '0'.
        ].

        (char = 256) ifTrue: [
          8 to: 1 by: -1 do: [:index |
            binstr := binstr, (char bitAt: index) asString.
          ].
          binstr := binstr, '1'.
        ].

        encodedstr := encodedstr, binstr.
      ].
    ].

    encodedstr := encodedstr, '1'. 

    infile := FileStream open: infilearg mode: FileStream read.

    [infile atEnd not] whileTrue: [
      infilestr := infile next asCharacter.
      encodedstr := encodedstr,
       (encodedbits at: (infilestr asInteger)).  
    ].
    infile close.
    
    encodedstr := encodedstr, (encodedbits at: 256).
    ((encodedstr size) \\ 8) > 0 ifTrue: [
       1 to: 8 - ((encodedstr size) \\ 8) do: [:a |
         encodedstr := encodedstr, '0'.
      ]. 
    ].

    outBytes := OutBits new: outfile.
    bitArray := OrderedCollection new.

    encodedstr do: [:index | 
      (index asCharacter) = $0 ifTrue: [bitArray add: 0].
      (index asCharacter) = $1 ifTrue: [bitArray add: 1].
    ].
    
    bitArray do: [:bit| outBytes writeBit: bit].
    outBytes flushByte.
    outfile close.
  ].
].

(u) ifTrue: [
  dtree := 0.
  bit := 0.
  complete := false.
  ptr := 0.
  stack := OrderedCollection new.
  outBytes := OutBits new: outfile.
  infileBits := BitStream new: infile.

  [infileBits atEnd not] whileTrue: [
    node := infileBits nextBit.
    char := 0.
    (complete not) ifTrue: [
      node = 0 ifTrue: [
        8 to: 1 by: -1 do: [:index |
          bit := infileBits nextBit.
          char := char bitAt: index put: bit.
        ].
        char = 0 ifTrue: [
          nextbit := infileBits nextBit.
          nextbit = 1 ifTrue: [
            char := char + 256.
          ].
        ].
        stack add: (Leaf new: char count: '').
      ].
      node = 1 ifTrue: [
        (stack size > 1) ifTrue: [
          rightNode := stack removeLast.
          leftNode := stack removeLast.
          stack add: (Tree new: '' count: '' 
            left: leftNode right: rightNode).
        ]
        ifFalse: [
          dtree := stack removeLast.
          ptr := dtree.
          complete := true.
        ].
      ].
    ]
    ifFalse: [
      (ptr isMemberOf: Tree) ifTrue: [
        node = 0 ifTrue: [
          ptr := ptr left.
          (ptr isMemberOf: Tree) not ifTrue: [
            (ptr char = 256) not ifTrue: [
              8 to: 1 by: -1 do: [:index |
                bit := (ptr char) bitAt: index.
                outBytes writeBit: bit.
              ].
            ptr := dtree.
            ].
          ].
        ] 
        ifFalse: [
          ptr := ptr right.
          (ptr isMemberOf: Tree) not ifTrue: [
            (ptr char = 256) not ifTrue: [
              8 to: 1 by: -1 do: [:index |
                bit := (ptr char) bitAt: index.
                outBytes writeBit: bit.
              ].
              ptr := dtree.
            ].
          ].
        ].
      ].
    ].
  ].
  infile close.
  outfile close.
].

