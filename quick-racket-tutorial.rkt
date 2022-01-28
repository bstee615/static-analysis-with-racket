#lang slideshow
; https://docs.racket-lang.org/quick/

"circle"
(define c (circle 10))
c

"rectangle"
(define r (rectangle 10 20))
r

"square"
(define (square n)
  (filled-rectangle n n))
(square 10)

(define (four p)
  (define two-p (hc-append p p))
  (vc-append two-p two-p))
(four c)
(define (four-d p d)
  (define two-p (hc-append d p p))
  (vc-append d two-p two-p))
(four-d c 10)

; using let
(define (checker p1 p2)
  (let ([p12 (hc-append p1 p2)]
        [p21 (hc-append p2 p1)])
    (vc-append p12 p21)))
(checker (square 10) (rectangle 10 10))
; using let*
(define (checkerboard p)
  (let* ([rp (colorize p "red")]
         [bp (colorize p "black")]
         [c (checker rp bp)]
         [c4 (four c)])
    (four c4)))
(checkerboard (square 10))

;functions are values
(define (series mk)
  (hc-append 4 (mk 5) (mk 10) (mk 20)))
(series circle)

(series (lambda (size) (checkerboard (square size))))

;function form of define is just shorthand for binding a name to a lambda
(define series-lambda
  (lambda (mk)
    (hc-append 4 (mk 5) (mk 10) (mk 20))))

; function just returns its value
(define return-zero 0)

;passing more functions as parameters
(define (rgb-series mk)
  (vc-append
   (series (lambda (sz) (colorize (mk sz) "red")))
   (series (lambda (sz) (colorize (mk sz) "green")))
   (series (lambda (sz) (colorize (mk sz) "blue")))))
(rgb-series square)

; functions are evaluated in a different order
(define (rgb-maker mk)
  (lambda (sz)
    (vc-append (colorize (mk sz) "red")
               (colorize (mk sz) "green")
               (colorize (mk sz) "blue"))))
(series (rgb-maker circle))

(list (circle 10) (square 10))

;map and apply
(define (rainbow p)
  (map (lambda (color)
         (colorize p color))
       (list "red" "orange" "yellow" "green" "blue" "purple")))
(rainbow (circle 10))
(apply hc-append (rainbow (circle 10)))

(apply vc-append (rainbow (square 5)))

;require
(require pict/flash)
(filled-flash 40 30)

;macros
(require slideshow/code)
;code is not a function but a macro - it doesn't evaluate its arguments on call
(code (circle 10))

;define a new syntactic form
(define-syntax pict+code
  (syntax-rules ()
    [(pict+code expr)
     (hc-append 10
                expr
                (code expr))]
    [(pict+code dist expr)
     (hc-append dist
                expr
                (code expr))]))
(pict+code (circle 10))
(pict+code 20 (circle 10))

;learning about objects through GUI
(require racket/class ; implements class system
         racket/gui/base ; implements GUI and drawing classes
         )
(define foo-frame (new frame% [label "My Art"]
                      [width 300]
                      [height 300]
                      [alignment '(center center)]))
(send foo-frame show #t) ; #t is the boolean constant "true"

#t
't
`t

(define (add-drawing p)
  (let ([drawer (make-pict-drawer p)])
    (new canvas% [parent foo-frame]
                 [style '(border)]
                 [paint-callback (lambda (self dc)
                                   (drawer dc 0 0))])))
(add-drawing (pict+code (circle 10)))
(add-drawing (colorize (filled-flash 50 30) "yellow"))
(add-drawing (colorize (rectangle 50 30) "purple"))