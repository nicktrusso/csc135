;***********************************
;* Programmed by: Nick Trusso      *
;* 10/30/15                        *
;***********************************


;****************** PROBLEM A *****************************
;takes two integers, and returns a 4-digit integer constructed of the leftmost 2 digits of the
;first input, and the rightmost 2 digits of the second input.       
(define(buildLR i1 i2)
  (cond((null? i1)'())
       ((not(isTwoDigit i1)) -1)
       ((not(isTwoDigit i2)) -1)
       (else(+ (* 100 (firstDigits i1))(lastDigits i2)))))
       
;****************** PROBLEM B **************************** 
;listMins takes two equal-length lists of numbers, and returns a single list 
;consisting of the smaller ofthe two lists, position by position.
(define(listMins l1 l2)
  (cond((null? l1) '())
       ((>(car l1)(car l2))(cons(car l2)(listMins(cdr l1)(cdr l2))))
       (else(cons(car l1)(listMins(cdr l1)(cdr l2))))))
       
;****************** PROBLEM C *****************************
;takes a list of atoms and returns the unwinded version of the list.
(define (unwind lst)
 (if (null? lst) '()
    (cons (getMidIndex lst) (unwind (remove (getMidIndex lst) lst)))))
 
;****************** PROBLEM D *****************************       
;takes two Boolean functions F and G, and a list L. It returns the number 1 if
;function F is the "winner", and the number 2 if function G is the "winner", 
;and the number 0 if the two functions are tied. The winning function is the one 
;which has the most "true" answers for each value in the list.       
(define(functionWinner F G L)
  (cond((null? L)'())
       ((>(boolCount F L)(boolCount G L)) 1)
       ((>(boolCount G L)(boolCount F L)) 2)
       (else 0)))

;****************** PROBLEM E *****************************
;takes a list of integers, possibly including nested lists, and returns the total number
;of integers in the entire nested list.
(define(getNestedCount l)
  (cond((null? l) 0)
       ((list? (car l))(+ (getNestedCount(car l))(getNestedCount(cdr l))))
       (else(+ 1(getNestedCount(cdr l))))))
       

;****************** HELPER FUNCTIONS **********************
(define(isTwoDigit n)
  (or (> n 9)(< n -9))) 

(define(firstDigits n)
  (cond((> 0 n)(firstDigits (abs n)))
       ((> n 10)(firstDigits (/ n 10)))
       ((< n 10)(floor(* n 10)))))

(define(lastDigits n)(modulo (abs n) 100))

(define(boolCount F L)
  (cond((null? L) 0)
       ((F (car L))(+ 1(boolCount F (cdr L))))
       (else (boolCount F (cdr L)))))

(define(isNeg n)(< n 0))

(define(isEven n)(= 0 (modulo n 2)))

(define (isOdd n)(not (isEven n)))

(define (getMidIndex lst)
	(if (isEven (length lst)) (list-ref lst (- (quotient (length lst) 2) 1)) 
		(list-ref lst (quotient (length lst) 2))))
