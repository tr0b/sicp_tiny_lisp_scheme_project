#lang sicp

(define (square x) (* x x))
(define (sum-of-squares x y)   (+ (square x) (square y)))

(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (sum-two-largest x y z)
  (cond
    ((and ( <= z x ) (<= z y)) (+ x y ))
    ((and ( <= x y ) (<= x z)) (+ y z ))
    (else ( + x z ))))

(define (a-plus-abs-b a b)
  ((if ( > b 0 ) + - ) a b))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)

  (define (good-enough? current-guess next-guess)
    (< (abs ( / (- current-guess next-guess) current-guess )) 0.00000000000000001))

  (define (improve guess)
    (average guess (/ x guess)))
  
  (define (sqrt-iter guess)
    (if (good-enough? guess (improve guess))
        guess
        (sqrt-iter (improve guess))))
  
  (sqrt-iter 1.0)
  )

(define (crt-iter guess x)
  (define (good-enough? current-guess next-guess)
    (< (abs ( / (- current-guess next-guess) current-guess )) 0.00000000000000001))
  (if (good-enough? guess (improve-crt guess x))
      guess
      (crt-iter (improve-crt guess x)
                x)))

(define (improve-crt guess x) ( / ( + ( / x (square guess) ) ( * 2 guess ) ) 3 ))

(define (crt x)   (crt-iter 1.0 x))

; See, Exercise 1.31
;(define (factorial n)
;  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

(define (A x y)
  (cond
    ((= y 0) 0)
    ((= x 0) (* 2 y))
    ((= y 1) 2)
    (else (A (- x 1)
             (A x ( - y 1))))))

;(define (fib n)
;  (fib-iter 1 0 n))

;(define (fib-iter a b count)
;  (if (= count 0)
;      b
;      (fib-iter (+ a b) a (- count 1))))

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(define (pascal n)
  (pascal-iter 0 n #f))

; I want to get rid of this logger 
(define (pascal-iter current max-count logger)
  (newline)
  (if (< max-count current)
      #t
      (pascal-iter (+ current 1)  max-count (pascal-row-iter 0 current))))

(define (pascal-row-iter current max-count)
  (display (combination max-count current))
  (display " ")
  (if (< max-count current)
      max-count
      (pascal-row-iter (+ current 1) max-count)))

; C(n,r) combination
(define (combination n r)
  (if (< n r )
      ""
      (/ (factorial n) (* (factorial r) (factorial (- n r))))))


; recursive

(define (pascal-r row col)
  (cond ((= row 1) 1)
        ((= row col) 1)
        ((= col 1) 1)
        (else (+
               (pascal-r (- row 1) (- col 1))
               (pascal-r (- row 1) col)))))

;Ex 1.15

(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

;1.2.4

;recursion
;(define (expt b n)
;  (if (= n 0)
;     1
;     (* b ( expt b (- n 1)))))

(define  (expt b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b (- counter 1) (* product b))))

;(define (fast-expt b n)
;  (cond ((= n 0) 1)
;        ((even? n) (square (fast-expt b (/ n 2))))
;        (else ( * b (fast-expt b ( - n 1))))))

(define (even? n)
  (= (remainder n 2) 0))

(define golden-ratio (/ ( + 1 (sqrt 5)) 2))
(define anti-golden-ratio (/ ( + 1 (- (sqrt 5))) 2))

;(define (fast-expt b n)
;  (fast-expt-iter b n 1))

; Ex 1.16

(define (fast-expt b n)
  (fast-expt-iter b n 1))

(define (fast-expt-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (fast-expt-iter (square b) (/ n 2) a))
        (else (fast-expt-iter b (- n 1) (* a b)))))

; Ex 1.17

(define (fast-mult-recur a b)
  (cond ((= b 0) 0)
        ((even? b) (double (fast-mult-recur a (halve b))))
        (else (+ a (fast-mult-recur a (- b 1))))))

(define (double n)
  (+ n n))

(define (halve n)
  (/ n 2))

; Ex 1.18

(define (fast-mult a b)
  (fast-mult-iter a b 0))

(define (fast-mult-iter base times sum)
  (cond ((= times 0) sum)
        ((even? times) (fast-mult-iter (double base) (/ times 2) sum))
        (else (fast-mult-iter base (- times 1) (+ sum base)))))

; Ex 1.19

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b )
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))
                   (+ (* 2 p q) (square q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) ( * a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

; 1.2.5 Greatest Common Divisors

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; 1.2.6 Testing for Primality

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        ;(else (find-divisor n (+ test-divisor 1)))))
        (else (find-divisor n (next test-divisor))))) ; Exercise 1.23

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))


; Fermat's little theorem

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

; Exercise 1.22

(define (timed-prime-test? n) ; I slightly changed the code from the book to return a #t or #f for increasing the count in search-for-primes-iter
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 100) ; Exercise 1.24
      ;(if (prime? n)
      (report-prime (- (runtime) start-time) n)
      #f))

(define (report-prime elapsed-time n)
  (newline)
  (display n)
  (display " *** ")
  (display (/ elapsed-time 1000)))

(define (search-for-primes min)
  (define (search-for-primes-iter count max curr)
    (cond ((= count max) #t)
          ((timed-prime-test? curr) (search-for-primes-iter (+ count 1) max ( + curr 2)))
          (else (search-for-primes-iter count max (+ curr 2)))))
  (if (even? min)
      (search-for-primes-iter 0 3 (+ min 1))
      (search-for-primes-iter 0 3 (+ min 2))))

; Exercise 1.23

(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

; 1.3 Formulating Abstractions with Higher-Order Procedures

#| (define (sum-integers a b)
     (if (> a b)
         0
         (+ a (sum-integers (+ a 1) b))))
   
   (define (sum-cubes a b)
     (if (> a b)
         0
         (+ (cube a) (sum-cubes (+ a 1) b))))
   
   (define (pi-sum a b)
     (if ( > a b)
         0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b)))) |#

; Slots and template of this pattern above. Sigma notation

#| (define (<name> a b)
     (if (> a b)
         0
         (+ (<term> a)
         (<name> (<next> a) b)))) |#

; Exercise 1.30 re-creates this procedure using an iterative process
#| (define (sum term a next b)
     (if (> a b)
         0
         (+ (term a)
         (sum term (next a) next b)))) |#

(define (inc n) (+ n 1))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))


(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

#| (define (integral f a b dx)
     (define (add-dx x) (+ x dx))
     (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx)) |#

; Exercise 1.29 (NEED TO REVIEW AND RE-DO WITH CALCULUS TRAINING IN THE FUTURE)

(define (integral-simpson-rule f a b n)
  (define h (/ (- b a) n))
  (define (yk k count) (* count (f (+ a (* k h))))) ; yk = f(a + kh). Also yk changes depending on y kth-element (e.g. y0 count is 1, y-even is 2, y-odd is 4)
  (define (term k)
    (cond ((or (= k 0) (= k n)) (yk k 1))
          ((even? k) (yk k 2))
          (else (yk k 4))))
  (* (/ h 3) (sum term 0 inc n)))

; Exercise 1.30

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

; Exercise 1.31

; The recursive definition of sum
#| (define (<name> a b)
     (if (> a b)
         0
         (+ (<term> a)
         (<name> (<next> a) b)))) |#

#| (define (sum term a next b)
     (if (> a b)
         0
         (+ (term a)
         (sum term (next a) next b)))) |#

; The recursive procedure definition of product

#| (define (product term a next b)
     (if (> a b)
         1
         (* (term a)
         (product term (next a) next b)))) |#

(define (factorial n)
  (product identity 1 inc n))

(define (pi-approximation n)
  (product wallis-formula 1 inc n))

(define (wallis-formula k)
  (define 2k (* 2 k))
  (*
   (/ 2k
      (- 2k 1))
   (/ 2k
      (+ 2k 1))))
  

; The iterative procedure definition of product

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a) ))))
  (iter a 1))

; Exercise 1.32

; The recursive definition of sum
#| (define (<name> a b)
     (if (> a b)
         0
         (+ (<term> a)
         (<name> (<next> a) b)))) |#

; The recursive definition of product

#| (define (<name> a b)
     (if (> a b)
         0
         (* (<term> a)
         (<name> (<next> a) b)))) |#

; The higher abstraction accumulate

; The recursive definition of accumulate

#| (define (accumulate combiner null-value term a next b)
        (if (> a b)
            null-value
            (combiner (term a)
         (accumulate combiner null-value term (next a) next b)))) |#

; The iterative definition of accumulate

(define (accumulate combiner null-value term a next b)
     (define (iter a result)
       (if (> a b)
           result
           (iter (next a) (combiner (term a) result))))
  (iter a null-value))

; Exercise 1.33

; Recursive definition of accumulate-filter

#| (define (accumulate-filter predicate? combiner null-value term a next b)
  (cond ((> a b) null-value)
        ((filter? a) (accumulate-filter predicate?
                                        combiner
                                        null-value
                                        term
                                        (next a)
                                        next b))
        (else (combiner (term a) (accumulate-filter predicate?
                                                    combiner
                                                    null-value
                                                    term
                                                    (next a)
                                                    next b))))) |#

; Iterative definition of accumulate-filter

(define (accumulate-filter predicate? combiner null-value term a next b)
        (define (iter a result)
          (cond ((> a b) result)
                ((predicate? a) (iter (next a)
                                   (combiner (term a)
                                             result)))
                (else (iter (next a)
                                   result))))
  (iter a null-value))