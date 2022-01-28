#lang racket

;; https://matt.might.net/articles/intro-static-analysis

; simple-eval : expr -> {set int}
; interpreter for our language.
(define (simple-eval exp)
  (match exp
    [(? integer?)
     exp]
    [`(*, exp1, exp2)
      (* (simple-eval exp1)
         (simple-eval exp2))]
    [`(+, exp1, exp2)
      (+ (simple-eval exp1)
         (simple-eval exp2))]
    [`(=, exp1, exp2)
      (if (= (simple-eval exp1)
             (simple-eval exp2))
       1 0)]
    [`(/, exp1, exp2)
      (/ (simple-eval exp1)
         (simple-eval exp2))
      ]
  )
)

; test
(simple-eval `(+ 10 (* -3 2)))
(simple-eval `(+ 10 12))
(simple-eval `(/ 3 1))
(simple-eval `(/ 10 3))


; alpha : int -> abs-int
; turns an integer into its abstract integer representation.
(define (alpha int)
  (cond
    [(< int 0) {set `-}]
    [(= int 0) {set `0}]
    [(> int 0) {set `+}]))

; test
;(alpha 10)
;(alpha 0)
;(alpha -200)


; simple-eval^ : expr -> abs-int
; abs-int is a set of signs that can represent an int.
; abstract sign analyzer for our language.
(define (simple-eval^ exp)
  (match exp
    [(? integer?)
     (alpha exp)]
    [`(*, exp1, exp2)
      (*^ (simple-eval^ exp1)
         (simple-eval^ exp2))]
    [`(+, exp1, exp2)
      (+^ (simple-eval^ exp1)
         (simple-eval^ exp2))]
    [`(=, exp1, exp2)
      (=^ (simple-eval^ exp1)
          (simple-eval^ exp2))]
    [`(/, exp1, exp2)
      (/^ (simple-eval^ exp1)
          (simple-eval^ exp2))]
  )
)


(define (set-equal? A B)
  (and (subset? A B)
       (subset? B A)))

(define (=^ a b)
  (if
   (set-empty? (set-intersect a b))
   {set '0}
   (if
    (and (set-equal? a (set 0)) (set-equal? b (set 0)))
    {set '+}
    {set '0 '+})))
"abstract equals"
"should return {0}" (=^ (set 1 2 3) (set 4 5))
"should return {0 +}" (=^ (set 1 2 3) (set 2 1))


; /^/sign : sign * sign -> abs-int
; pairwise conversion from sign to abs-int
(define (/^/sign a b)
  (match* {a b}
    [{'+ '+} {set '+}]
    [{'+ '0} {set '0}]
    [{'+ '-} {set '-}]
    [{'0 _}  {set '0}]
    [{'- '+} {set '-}]
    [{'- '0} {set '0}]
    [{'- '-} {set '+}]))

; more precise version
; /^ : abs-int * abs-int -> abs-int
(define (/^ a b)
  (apply set-union
         (for*/list ([s1 a]
                     [s2 b])
           (/^/sign s1 s2))))


; *^/sign : sign * sign -> abs-int
; pairwise conversion from sign to abs-int
(define (*^/sign a b)
  (match* {a b}
    [{'+ '+} {set '+}]
    [{'+ '0} {set '0}]
    [{'+ '-} {set '-}]
    [{'0 _}  {set '0}]
    [{'- '+} {set '-}]
    [{'- '0} {set '0}]
    [{'- '-} {set '+}]))

; worst possible version
;(define (*^ a b)
;  {set '- '0 '+})

; more precise version
; *^ : abs-int * abs-int -> abs-int
(define (*^ a b)
  (apply set-union
         (for*/list ([s1 a]
                     [s2 b])
           (*^/sign s1 s2))))

; test
;(*^ {set '- '+} {set 0})


(define (+^/sign a b)
  (match* {a b}
    [{'+ '+} {set '+}]
    [{'+ '0} {set '+}]
    [{'+ '-} {set '+ 0 '-}]
    [{'0 '+} {set '+}]
    [{'0 '0} {set '0}]
    [{'0 '-} {set '-}]
    [{'- '+} {set '+ 0 '-}]
    [{'- '0} {set '-}]
    [{'- '-} {set '-}]))

; worst possible version
;(define (+^ a b)
;  {set '- '0 '+})

; more precise version
; +^ : abs-int * abs-int -> abs-int
(define (+^ a b)
  (apply set-union
         (for*/list ([s1 a]
                     [s2 b])
           (+^/sign s1 s2))))

; test
;(+^ {set '-} {set '+})

; test
(simple-eval^ `(+ 2 (+ 1 2)))
(simple-eval^ `(+ -2 (/ 1 2)))
(simple-eval^ `(+ -2 (/ -1 2)))

(simple-eval `(= -1 1))
(simple-eval^ `(= -1 1))
(simple-eval^ `(= 11 1))
(simple-eval^ `(= 0 0))