#lang racket

;; https://matt.might.net/articles/intro-static-analysis

; <prog> ::= <stmt> ...

; <stmt> ::= (label <label>)
;         |  (goto <label>)
;         |  (:= <var> <exp>)
;         |  (if <exp> goto <label>)

; <exp> ::= (+ <exp> <exp>)
;        |  (* <exp> <exp>)
;        |  (= <exp> <exp>)
;        |  <int>
;        |  <var>

; add a while construct to Racket
(define-syntax while 
  (syntax-rules ()
    [(_ cond body ...)
     ;=>
     (letrec [(loop (λ ()
                      (when cond
                        body ...
                        (loop))))]
       (loop))]))

(struct state {stmts env})

; stmt-map : label => stmt*
(define stmt-map (make-hasheq))

; constructs the label-to-statements map:
(define (preprocess stmts)
  (match stmts
    [(cons `(label ,label) rest)
     (hash-set! stmt-map label stmts)
     (preprocess rest)]
    
    [(cons _ rest)
     (preprocess rest)]
    
    ['()
     (void)]))

; an environment maps variables to integers:
(define env0 (make-immutable-hasheq))

; inject : prog -> state
(define (inject prog)
  (state prog env0))

(define (run prog)
  (preprocess prog)
  (define current-state (inject prog))
  (while (state? current-state)
    (set! current-state (step current-state)))
  current-state)

; step : state -> (state + env)
(define (step state0)
  
  (define stmts (state-stmts state0))
  
  (define env (state-env state0))
  
  (match stmts
    ['()
     env]
    
    [(cons `(label ,l) rest)
     (state rest env)]
    
    [(cons `(:= ,var ,exp) rest)
     (define env* (hash-set env var (simple-eval exp env)))
     (state rest env*)]
    
    [(cons `(if ,exp goto ,label) rest)
     (define condition (simple-eval exp env))
     (if (not (= condition 0))
         (state (hash-ref stmt-map label) env)
         (state rest env))]
    
    [(cons `(goto ,label) rest)
     (state (hash-ref stmt-map label) env)]
         
    [else
     (error (format "unknown instruction: ~a!" (car stmts)))]))

; simple-eval : expr -> int
; interpreter for our language.
(define (simple-eval exp env)
  (match exp
    [(? symbol?)
     (hash-ref env exp)]
    [(? integer?)
     exp]
    [`(*, exp1, exp2)
      (* (simple-eval exp1 env)
         (simple-eval exp2 env))]
    [`(+, exp1, exp2)
      (+ (simple-eval exp1 env)
         (simple-eval exp2 env))]
    [`(=, exp1, exp2)
      (if (= (simple-eval exp1 env)
             (simple-eval exp2 env))
       1 0)]
    [`(/, exp1, exp2)
      (/ (simple-eval exp1 env)
         (simple-eval exp2 env))
      ]
  )
)




;;;;;;; abstract operators
; alpha : int -> abs-int
; turns an integer into its abstract integer representation.
; abs-int is a set of signs that can represent an int.
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
; abstract sign analyzer for our language.
(define (simple-eval^ exp env^)
  (match exp
    [(? integer?)
     (alpha exp)]
    [(? symbol?)
     (hash-ref env^ exp)]
    [`(*, exp1, exp2)
      (*^ (simple-eval^ exp1 env^)
         (simple-eval^ exp2 env^))]
    [`(+, exp1, exp2)
      (+^ (simple-eval^ exp1 env^)
         (simple-eval^ exp2 env^))]
    [`(=, exp1, exp2)
      (=^ (simple-eval^ exp1 env^)
          (simple-eval^ exp2 env^))]
    [`(/, exp1, exp2)
      (/^ (simple-eval^ exp1 env^)
          (simple-eval^ exp2 env^))]
  )
)

; test
;(simple-eval^ (+ 0 1) '())

; set-equal operator
(define (set-equal? A B)
  (and (subset? A B)
       (subset? B A)))

; =^: abs-int abs-int -> {0 1}
(define (=^ a b)
  (if
   (set-empty? (set-intersect a b))
   {set '0}
   (if
    (and (set-equal? a (set 0)) (set-equal? b (set 0)))
    {set '+}
    {set '0 '+})))

; more precise version
; *^ : abs-int * abs-int -> abs-int
(define (*^ a b)
  ; pairwise
  (define (*^/sign a b)
    (match* {a b}
      [{'+ '+} {set '+}]
      [{'+ '0} {set '0}]
      [{'+ '-} {set '-}]
      [{'0 _}  {set '0}]
      [{'- '+} {set '-}]
      [{'- '0} {set '0}]
      [{'- '-} {set '+}]))
  ; setwise
  (apply set-union
         (for*/list ([s1 a]
                     [s2 b])
           (*^/sign s1 s2))))

; +^ : abs-int + abs-int -> abs-int
(define (+^ a b)
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
  (apply set-union
         (for*/list ([s1 a]
                     [s2 b])
           (+^/sign s1 s2))))

; /^ : abs-int * abs-int -> abs-int
(define (/^ a b)
  (define (/^/sign a b)
    (match* {a b}
      [{'+ '+} {set '+}]
      [{'+ '0} {set '0}]
      [{'+ '-} {set '-}]
      [{'0 _}  {set '0}]
      [{'- '+} {set '-}]
      [{'- '0} {set '0}]
      [{'- '-} {set '+}]))
  (apply set-union
         (for*/list ([s1 a]
                     [s2 b])
           (/^/sign s1 s2))))

; test
;(*^ {set '- '+} {set '+})
;(+^ {set '+} {set '+})
;(+^ {set '+} {set '+ '-})
;(/^ {set '- '+} {set '+})

; states pair statements with environments:

; an environment maps variables to integers:
(define env^0 (make-immutable-hasheq))

; inject creates an initial state for a program:
(define (inject^ prog)
  (state^ prog env^0))

; abstract state
(struct state^ {stmts env^}
  #:transparent)

; step^ : state^ -> state^*
(define (step^ state^-in)
  
  (define stmts (state^-stmts state^-in))
  
  (define env^ (state^-env^ state^-in))
  
  (match stmts
    ['()
     env^]
    
    [(cons `(label ,l) rest)
     (list (state^ rest env^))]
    
    [(cons `(:= ,var ,exp) rest)
     (define env^* (hash-set env^ var (simple-eval^ exp env^)))
     (list (state^ rest env^*))]
    
    [(cons `(if ,exp goto ,label) rest)
     (define condition (simple-eval^ exp env^))
     (list
      (state^ (hash-ref stmt-map label) env^)
      (state^ rest env^))]
    
    [(cons `(goto ,label) rest)
     (list 
      (state^ (hash-ref stmt-map label) env^))]
         
    [else
     (error (format "unknown instruction: ~a!" (car stmts)))]))

(define (analyze prog)
  
  (preprocess prog)
  
  ; the initial abstract state:
  (define state^0 (inject^ prog))

  ; state^-envs : state^ #> abs-env
  (define state^-envs (make-hasheq))
  ; map a state to the current env
  (define (push-state-env state^-in)
    
    (define stmts (state^-stmts state^-in))
  
    (define env^ (state^-env^ state^-in))

    (hash-set! state^-envs (car stmts) env^)
    )
  
  ; the set of all states ever seen:
  ; visited : (set state^)
  (define visited (set))
  
  ; the neighbor maps
  ; neighbors : state^ #> (list state^)
  (define neighbors (make-hasheq))
  
  ; mark the neigbors of a state.
  (define (mark-neighbors! state^ succs)
    (hash-set! neighbors state^ succs))
  
  ; marks a state as seen:
  (define (mark-seen! state^)
    (set! visited (set-add visited state^)))
  
  ; checks if a state is seen
  (define (seen? state^)
    (set-member? visited state^))
  
  ; states to explore next:
  (define todo (list state^0))
  
  ; adds states to be explored:
  (define (push-todos states^)
    (set! todo (append states^ todo)))
  
  ; grabs the next state to be explored:
  (define (pop-todo)
    (define next (car todo))
    (set! todo (cdr todo))
    next)
  
  (while (not (null? todo))
    (define curr (pop-todo))
    (when (not (seen? curr))
      (mark-seen! curr)
      (define succs (step^ curr))
      (when (list? succs)
        (push-state-env curr)
        (mark-neighbors! curr succs)
        (push-todos succs))))
  
  ; return all visited states:
  state^-envs)







;;;;;;;; program
; Example program
(define prog 
  '(
    
    (:= n 5)
    
    (:= a 1)
    
    (label top)
    
    (if (= n 0) goto done)
    
    (:= a (* a n))
    
    (:= n (+ n -1))
    
    (goto top)
    
    (label done)
    
    ))
(run prog)
(analyze prog)

; Loop from 0 to 10
(define prog2
  '(
       (:= n 0)
       (:= m 0)
       (label dummy)
       (label loop)
       (if (= n 10) goto done)
       (:= n (+ n 1))
       (:= m (+ m -1))
       (goto loop)
       (label done)
   ))
(run prog2)
(analyze prog2)