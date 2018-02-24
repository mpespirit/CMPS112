#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
"$Id: complex.st,v 1.1 2016-10-26 12:55:24-07 - - $"

 Number subclass: Complex [
    | realpart imagpart |
     
    Complex class >> new [
        ^ self error: 'use real:imag:'
    ]
    Complex class >> new: ignore [
        ^ self new
    ]
    Complex class >> real: r imag: i [
        ^ (super new) setReal: r setImag: i
    ]
     
    setReal: r setImag: i [
        realpart := r.
        imagpart := i.
        ^ self
    ]
     
    real [
        ^ realpart
    ]
    imag [
        ^ imagpart
    ]
     
    + val [
        ^ Complex real: (realpart + val real)
            imag: (imagpart + val imag)
    ]
    - val [
        ^ Complex real: (realpart - val real)
            imag: (imagpart - val imag)
    ]
    * val [
        ^ Complex real: (realpart * val real) - (imagpart * val imag)
            imag: (imagpart * val real) + (realpart * val imag)
    ]
    / val [
        | d r i |
        d := (val real * val real) + (val imag * val imag).
        r := ((realpart * val real) + (imagpart * val imag)).
        i := ((imagpart * val real) - (realpart * val imag)).
        ^ Complex real: r / d imag: i / d
    ]
     
    = val [
        ^ (realpart = val real) & (imagpart = val imag)
    ]

    abs [
        ^ (realpart squared + imagpart squared) sqrt
    ]

    sqrt [
        | r rc zrc abszrc |
        r := self abs.
        rc := Complex real: r imag: 0.
        zrc := self + rc.
        abszrc := Complex real: zrc abs imag: 0.
        ^ (Complex real: r sqrt imag: 0) * zrc / abszrc.
    ]
     
    "All other comparison methods are based on <"

    < val [
        ^ self shouldNotImplement
    ]
     
    printOn: aStream [
        realpart printOn: aStream.
        aStream nextPut: $+.
        imagpart printOn: aStream.
        aStream nextPut: $i
    ]
]

nl := Character nl.
i := Complex real: 0 imag: 1.
stdout << 'i = ' << i << nl.
neg1 := Complex real: -1 imag: 0.
stdout << 'neg1 = ' << neg1 << nl.
stdout << 'neg1 sqrt = ' << neg1 sqrt << nl.
