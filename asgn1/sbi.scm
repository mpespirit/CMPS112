#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.3 2016-09-23 18:23:20-07 - - $
;;
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.  Currently it is only printed.
;;

;; Hash Table stuff: variable table, lable table, function table
(define *variable-table* (make-hash))
(define (var-get key)
        (hash-ref *variable-table* key))
(define (var-put! key value)
        (hash-set! *variable-table* key value))

;; Separate into function and variable tables
;; Add relops, binops, 
(for-each
    (lambda (pair)
            (var-put! (car pair) (cadr pair)))
    `(

        (log10_2 0.301029995663981195213738894724493026768189881)
        (sqrt_2  1.414213562373095048801688724209698078569671875)
        (e       2.718281828459045235360287471352662497757247093)
        (pi      3.141592653589793238462643383279502884197169399)
        (div     ,(lambda (x y) (floor (/ x y))))
        (log10   ,(lambda (x) (/ (log x) (log 10.0))))
        (mod     ,(lambda (x y) (- x (* (div x y) y))))
        (quot    ,(lambda (x y) (truncate (/ x y))))
        (rem     ,(lambda (x y) (- x (* (quot x y) y))))
        (+       ,+)
        (^       ,expt)
        (ceil    ,ceiling)
        (exp     ,exp)
        (floor   ,floor)
        (log     ,log)
        (sqrt    ,sqrt)

     ))


; Label table holds string labels for those lines where they appear
; Key is the label, Value is the line number to which the label refers
(define *label-table* (make-hash))
(define (label-get key)
        (hash-ref *label-table* key))
(define (label-put! key value)
        (hash-set! *label-table* key value))

;; Function table holds functions and operators
(define *function-table* (make-hash))
(define (func-get key)
        (hash-ref *function-table* key))
(define (func-put! key value)
        (hash-set! *function-table* key value))

;; 
;; What category of object is this?
;; 
(define (what-kind value)
    (cond ((real? value) 'real)
             ((vector? value) 'vector)
                        ((procedure? value) 'procedure)
                                  (else 'other)))
;;
;; Heart of the program below
;;
(define *stderr* (current-error-port))

(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)

(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)

(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)

(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))

;;
;; Real work starts below
;;

;; Goes to a provided line in program and finds/isolates statement 
;; component of line for further processing by parse-statement
(define (parse-line program line-nr)
    (when (> (length program) line-nr)
        (let ((line (list-ref program line-nr)))
             ;(printf "~s~n" line)
             (cond ( (= (length line) 2) 
                     (parse-statement (cadr line)))
                   ( (= (length line) 3)
                     (parse-statement (caddr line))))
             (parse-line program (+ line-nr 1))
        )
    )
)

;; Figures out species of statement. Passes statement to appropriate function
;; 'print' or 'if' or 'input' or 'goto' or 'dim' or 'let'
(define (parse-statement statement)
    (cond ( (eq? 'print (car statement ))
            (do-print (cdr statement) )  )))

;; Recursively prints each statement
(define (do-print printable) 
    (display (car printable ) )
    (when (not (null? (cdr printable)))
          (do-print cdr printable) ) 
    (newline)
) 

;; Expression shit
(define (parse-expression expr)
    (cond ( ( ) ))
)

(define (write-program-by-line filename program)
    ;(printf "==================================================~n")
    ;(printf "~a: ~s~n" *run-file* filename)
    ;(printf "==================================================~n")
    ;(printf "(~n")
    ;(map (lambda (line) (printf "~s~n" line)) program)
    ;(map (lambda (line) (
    ;(printf ")~n")
    
    ;; goes to line 0 of input program
    (parse-line program 0)
)

;; orgiastic loop that came with sample code
(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile)))
              (write-program-by-line sbprogfile program))))

(main (vector->list (current-command-line-arguments)))