; #!/usr/bin/env csi -s

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

(display (vector 1 2 3)) ; is supported

;; (string->vector "hello") ; not supported with 'chicken' for some reason

(string->symbol "hello")

(symbol->string 'hello) ; ' is to tell lisp to not evaluate the symbol

; // single line comment
#; () ; quote a single s-expression -- stuff inside ()
#|
multline comment. yay
|#

'() ; empty object

(cons 1 2) ; the pair 1 2

'(1 . 2)

;; evaluated to the same thing:
(cons 1 (cons 2 (cons 3 '())))
(list 1 2 3)
'(1 2 3)

;; improper list -- final element doesn't have |tail| is not '()
(cons 1 (cons 2 3))
(cons (list 1 2))
;; (list 1 2 . 3) ;; not supported?
'(1 2 . 3)

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

'#(1 2 3 4)
(vector 1 2 3 4)

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

; define procedures:
(define square (lambda (x) (* x x)))
(square 2)

(define (cube x)
        (* x x))
(define (sum a b)
        (+ a b))
(sum 10 20)

(define (quadruple x)
  (define (square x)
    (* x x))
  (* (square x) (square x)))  
 
(quadruple 10)

((lambda (x) (* x x)) 10)

(define sum (lambda args (apply + args)))
(define mul (lambda args (apply * args)))
                          
(sum 1 2 3 4)
(mul 2 2 2 2)

(define expression (lambda (first . rest) (/ first (apply + rest))))
(expression 1 2 3 4)

(define (rational first . rest)
  (let ((second (if (null? rest) 1 (car rest))))
    (/ first second)))

(rational 5)
(rational 5 10)

(define (factorial n)
    (if (<= n 1)
        1
        (* n (factorial (- n 1)))))

(factorial 10)

; note this uses `let` - `rec`, allowing a variable to reference itself
(letrec ((sum (lambda (x)
                (if (zero? x)
                  0
                  (+ x (sum (- x 1)))))))
  (sum 10))
     
; https://stackoverflow.com/questions/310974/what-is-tail-call-optimization

(define (fac x)
   (if (= x 0) 1
    (* x (fac (- x 1)))))
(fac 5)

(define (fact x)
  (define (fact-tail x accum)
    (if (= x 0) accum
      (fact-tail (- x 1) (* x accum))))
  (fact-tail x 1))
(fact 5)

; some thing to do with do-syntax... don't understand.

; association list, alist:
(list (cons "x" 10) (cons "y" 20) (cons "z" 30))

; taken example:
(define countup (let ((count 0))
                  (lambda ()
                    (set! count (+ count 1))
                    count)))
(countup)

;taken example:
(define (make-person name age)
  (lambda (action . rest)
    (case action
      ((name) name)
      ((age) age)
      ((set-name) (set! name (car rest)))
      ((set-age) (set! age (car rest))))))

(let ((jack (make-person "Jack" 22)))
  (display (jack 'name))
  (newline)
  (jack 'set-name "Mark")
  (jack 'set-age 24)
  (display (jack 'name))
  (display " is ")
  (display (jack 'age))
  (display " years old"))

; some interesting some with closures i am gonna ignore.

; macros... ok i will stop here...
