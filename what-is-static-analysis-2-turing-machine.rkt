#lang racket

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
(define stmt-map (make-hash))

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

; inject : prog -> state
(define (inject prog)
  (state prog #hasheq()))

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
     ;=>
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


;;;;; Program

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

; Loop from 0 to 10
(run '(
       (:= n 0)
       (label loop)
       (if (= n 10) goto done)
       (:= n (+ n 1))
       (goto loop)
       (label done)
       ))