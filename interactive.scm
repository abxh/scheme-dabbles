#!/usr/bin/env lips

; Following:
; https://lips.js.org/docs/scheme-intro/what-is-lisp

"hello world!"

(+ 1 2 3)

(+ (* 3 (/ 1 2)) (+ 1 2))

(* 1+1i 0+1i)

(string->number "1000" 2)

(number->string 8 2)

#t ; true
#f ; false

"Hello \"World\"!"

(string #\H #\e #\l #\l #\o)

(string->list "hello")

; (string->vector "hello")  this apparently requires a library in (C-based) 'chicken':
                          ; https://wiki.call-cc.org/eggref/5/vector-lib

(string->symbol "hello")

(symbol->string 'hello) ; ' is to tell lisp to not evaluate the symbol

; // single line comment
#; () ; quote a single s-expression -- stuff inside ()
#|
multline comment. yay
|#

'() ; empty object

(cons 1 2) ; the pair 1 2

;; (1 . 2) seems like this syntax is not supported in chicken lisp

;; evaluated to the same thing:
(cons 1 (cons 2 (cons 3 '())))
(list 1 2 3)
'(1 2 3)

;; improper list -- final element doesn't have |tail| is not '()
(cons 1 (cons 2 3))
(cons (list 1 2))
;; (list 1 2 . 3) ; not supported in chicken (because it's compiled?)
;; '(1 2 . 3)     ; --- || ---

;; define convienience macros
(define |head| car)    ; "head" is the first elm of the list
(define |tail| cdr)    ; "tail" is the rest of list
(define |third| caddr) ; same as (|head| (|tail| (|tail| l)))

(|head| '(1 2 3 4))
(|tail| '(1 2 3 4))
(|third| '(1 2 3 4))
(|head| (|tail| (|tail| '(1 2 3 4))))
    
(set-car! '(1 2 3 4))

; circular list

(cons (list 1 2) 3)

;; #(1 2 3 4) // not supported in chicken -- unless you have (https://wiki.call-cc.org/eggref/5/vector-lib)
;; (vector 1 2 3)

;; see:
;; https://stackoverflow.com/questions/41386527/scheme-unbound-variable-unquote
;; ` : quasiquote
;; , : unquote
;; ' : quote

(quote (unquote (+ 3 4)))

;; ` to only evaluate stuff with , prefix
`(1 2 ,(+ 2 1))

;; ,@ to splice inner list to outer list
`(1 2 3 ,@(list 4 5 6))

'(list 1 2 3)

(numerator 1/2)
(denominator 1/2)
(imag-part 10+2i)
(real-part 10+2i)
(angle 10+10i)
(magnitude 10+10i)

(eq? 1 2) ; basic type cmp
(eqv? 1 1) ; value rep cmp
(equal? (list 1 1) (list 1 1)) ; compare data structures

(string=? "Hello" "Hello")
(string-ci=? "Hello" "hello") ; case-insensitive
(char=? #\H #\H)

(define number 10)

(list number number)

; (symbol=? 'number 'number) ;; not defined in chicken?

(define number 10)
(set! number (+ number 1))
(display number)
(display (number->string number))

(let ((x 10) (y 20)) ; define local variables
    (+ x y))

(let* ((x 10) (y (* x x))) ; let x = 10, y = x * x
    (+ x y))

(letrec ((sum (lambda (list)
                (if (null? list) 0
                  ; else:
                  (+ (car list) (sum (cdr list)))))))
    (sum '(1 2 3 4)))
