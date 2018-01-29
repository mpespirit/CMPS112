#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.3 2016-09-23 18:23:20-07 - - $
;;
;; Paula Espiritu mespirit@ucsc.edu
;; Spenser Estrada spmestra@ucsc.edu
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

;; Function table holds functions and operators
(define *function-table* (make-hash))
(define (func-get key)
		(hash-ref *function-table* key))
(define (func-put! key value)
		(hash-set! *function-table* key value))
			   
;; Separate into function and variable tables
;; Add relops, binops, 
(for-each
	(lambda (pair)
		(var-put! (car pair) (cadr pair)))
	`(
		(+       ,+)
		(-       ,-)
		(*       ,*)
		(/       ,(lambda (x y) (floor (/ (+ x 0.0) (+ y 0.0)))))
		(%       ,(lambda (x y) (- x (* (/ x y) y))))
		(>       ,>)
		(=       ,(lambda (x y) (eqv? x y)))
		(<=      ,(lambda (x y) (<=   x y)))
		(>=      ,(lambda (x y) (>=   x y)))
		(<>      ,(lambda (x y) (not (equal? x y))))
		(^       ,(lambda (x y) (expt x y)))
		(abs     ,abs)
		(acos    ,acos)
		(asin    ,asin)
		(atan    ,atan) ;NEEDS TO BE A LAMBDA FUNCTION
		(ceil    ,ceiling)
		(cos     ,cos)
		(exp     ,exp)
		(floor	 ,floor)
		(log	 ,log)
		(log10	 ,(lambda (x) (/ (log x) (log 10.0))))
		(log10_2 0.301029995663981195213738894724493026768189881)
		(quot	 ,(lambda (x y) (truncate (/ x y))))
		(rem	 ,(lambda (x y) (- x (* (quot x y) y))))
		(round   ,round)
		(sin     ,sin)
		(sqrt	 ,sqrt)	
		(sqrt_2	 1.414213562373095048801688724209698078569671875)
		(tan     ,tan)
		(trunc   ,truncate)
                (e       2.718281828459045235360287471352662497757247093)
                (pi      3.141592653589793238462643383279502884197169399)
	)
)

;(for-each
;	(lambda (pair)
;		(var-put! (car pair) (cadr pair)))
;	`(
;		(e      ,2.718281828459045235360287471352662497757247093)
;	      (pi     ,3.141592653589793238462643383279502884197169399)	)
;)

; Label table holds string labels for those lines where they appear
; Key is the label, Value is the line number to which the label refers
(define *label-table* (make-hash))
(define (label-get key)
        (hash-ref *label-table* key))
(define (label-put! key value)
        (hash-set! *label-table* key value))

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

(define (find-labels program line-nr)
    (when (> (length program) line-nr)
        (let ((line (list-ref program line-nr)))
   ;       ;(printf ";; find-labels ~s~n" line)
           (when (> (length line) 1)
   ;         ;(printf ";; find-labels: longer than 1 ~n")
              (when ( and (not (list? (cadr line) ) )
                          (not ( hash-has-key? *variable-table* (cadr line))) )
                 (label-put! (cadr line) line-nr )
            ;     (printf ";; find-lables: put to var as ~s:~s~n"
            ;         (cadr line) (label-get (cadr line)) )
              )
           )
           (find-labels program (+ line-nr 1))
        )
    )
)


;; Goes to a provided line in program and finds/isolates statement 
;; component of line for further processing by parse-statement
(define (parse-line program line-nr)
   (when (> (length program) line-nr)
      (let ((line (list-ref program line-nr)))
  ;         (printf ";; parse-line: ~s~n" line )
           (when (and (not (null? (cdr line)))
                      (eq? 'done (cadr line)))
                 (die `(""))
           )
           (cond ((and (= (length line) 2)
                      (list? (cadr line)) ) 
  ;               (printf ";;parse-line: cadr line: ~s~n" (cadr line))
  ;               (printf ";; parse-line; length 2, list? ~s~n" 
  ;                   (list? (cadr line)))
                  (parse-statement program (cadr line)) ) 
                 ((= (length line) 3)
  ;               (printf ";;parse-line: cadr line: ~s~n" (cadr line))
  ;               (printf ";; parse-line; length 3, list? ~s~n"
  ;                   (list? (cadr line)))
                  (parse-statement program (caddr line)) )
           )
      ) 
      (parse-line program (+ line-nr 1))
    )
)

;; Figures out species of statement. Passes statement to appropriate function
;; 'print' or 'if' or 'input' or 'goto' or 'dim' or 'let'
(define (parse-statement program statement)
 ;   (printf ";; parse-statement: ~s~n" statement)
    (cond (( and (eq? 'print (car statement))
                (not (null? (cdr statement))))
 ;              (printf ";; is print ~n")
                (do-print (cdr statement))  ) 
          ;; If car reads 'let' check that two args present
          (( and (eq? 'let (car statement))
                (not (null? (cdr statement))))
 ;              (printf ";; is let~n")
                (do-let (cdr statement))  )
          (( and (eq? 'dim (car statement))
                (list? (cdr statement)))
                (do-dim (cadr statement) ))
          ((eq? 'goto (car statement))
                (parse-line program (label-get (cadr statement)))
          )
    )
 ;  (printf ";; parse-statement: leaving~n")
)

;; Recursively prints each statement.
;; If it's a string just print that. If it's an expression, evaluate 
;; then print. It's otherwise assumed to be a variable.
(define (do-print printable)
 ;  (printf ";; do-print: ~s~n" printable)
    (cond ( (string? (car printable))
            (display (car printable ) ) )
          ( (and  (list? (car printable)) 
                  (vector? (var-get (caar printable))) )
              (display 
              (vector-ref   
                  (var-get (caar printable))
                  (exact-round (parse-expr (cadar printable)))
              )
            )
          )
         (else
            (display (parse-expr (car printable) )) 
         ))
         ;if there are still more printables, print those too
    (when (not (null? (cdr printable)))
       (do-print (cdr printable) ) )
    (newline)
)

;; initiates array, puts in var table
(define (do-dim arr)
 ;  (printf ";; do-dim: ~s~n" arr )
   ;; makes a vector with the given car as key, the array itself as 
   ;; the vector. 
   (var-put! 
      (car arr) (make-vector (exact-round (parse-expr (cadr arr)))))
 ; (printf ";; do-dim: put in var as ~s:~s ~n" (car arr)(var-get (car arr)) )    
)

;; Evaluate let expresson and push value to variable table
(define (do-let var)
   ;(display (list? (car var)))(newline)
 ;  (printf ";; do-let: ~s~n" var)
    (if (list? (car var) )
     ;  ( begin (printf ";; is let of array~n")
        (vector-set! 
           (var-get (caar var)) 
           (exact-round (parse-expr (cadar var))) 
           (cadr var) 
 ;) 
 ;       (printf "do-let: var-put as ~s[~s]:~s ~n"
 ;          (caar var) 
 ;          (exact-round (parse-expr (cadar var)))
 ;          (vector-ref  
 ;            (var-get (caar var)) (exact-round (parse-expr (cadar var))))
 ;          )
        )  
        ;else
   ;     ( begin (printf ";; is not array~n")
          (var-put! (car var) (parse-expr (cadr var))) 
 ;         (printf ";; var-put as ~s:~s~n" (car var)(var-get (car var))) 
    ;    )
    )
 ;   (printf ";; do-let: leaving ~n")
)

;; Recursively analyze expressions
(define (parse-expr expr)
 ;   (printf ";; parse-expr: ~s~n" expr)
    (if (symbol? expr)
        ;then
        (if (hash-has-key? *variable-table* expr)
            (hash-ref *variable-table* expr)
            (void)
  ;         (printf ";; ~s is in var table as ~s~n" expr (var-get expr) )
        ) 
        ;else
        ;add 0.0 to the number to ensure it is a real number
        (if (number? expr)
            (+ expr 0.0) 
            ;apply the operator to each list item
            ;Ex: (map + '(1 2))
            ;    (+ 1)(+ 2)
            ;    (apply + (+ 1) (+ 2)) -> 3
            ;If not a number, recursively analyze the list
            (apply (var-get (car expr)) (map parse-expr (cdr expr)))
        )
    )
)      

(define (write-program-by-line filename program)
    ;(printf "==================================================~n")
    ;(printf "~a: ~s~n" *run-file* filename)
    ;(printf "==================================================~n")
    ;(printf "(~n")
    ;(map (lambda (line) (printf "~s~n" line)) program)
    ;(map (lambda (line) (
    ;(printf ")~n")
    (find-labels program 0)    
    ;; goes to line 0 of input program
    (parse-line program 0)
    ;(find-labels program 0)
)

;; orgiastic loop that came with sample code
(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile)))
              (write-program-by-line sbprogfile program))))

(main (vector->list (current-command-line-arguments)))
