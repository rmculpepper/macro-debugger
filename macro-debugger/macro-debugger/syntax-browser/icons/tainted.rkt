#lang racket/base
(require racket/class
         racket/format
         racket/list
         racket/snip
         racket/draw
         racket/math
         "base.rkt")
(provide tainted-icon-snip%
         snip-class)

(define collision-icon-points
  (let ()
    (define NSPIKES 11)
    (define INNER-RADIUS 0.25)
    (define OUTER-RADIUS 0.4)
    (define SCALE 0.1)
    (define THETA0 0.2)
    (define radii ;; length is 1 + 2*NSPIKES
      (for/fold ([acc (list INNER-RADIUS)])
                ([i (in-range NSPIKES)])
        (list* INNER-RADIUS (+ OUTER-RADIUS (* SCALE (abs (sin i)))) acc)))
    (define points
      (for/list ([r (in-list radii)] [i (in-naturals)])
        (define theta (+ THETA0 (* pi i (/ NSPIKES))))
        (cons (* r (cos theta)) (* r (sin theta)))))
    points))

(define tainted-icon-snip%
  (class one-cell-snip%
    (inherit set-snipclass get/cache-extent)
    (super-new)
    (set-snipclass snip-class)
    (send (get-the-snip-class-list) add snip-class)

    (define/override (draw dc x y left top right bottom dx dy draw-caret?)
      (when #t
        (define-values (w h d _a) (get/cache-extent dc))
        (define (SX u) (* (+ u 0.5) w))
        (define (SY u) (* (+ u 0.5) (min w (- h d))))
        (define (X u) (+ x (SX u)))
        (define (Y u) (+ y (SY u) (max 0 (/ (- h d w) 2))))
        (define points*
          (for/list ([p (in-list collision-icon-points)])
            (cons (X (car p)) (Y (cdr p)))))
        (let ([saved-pen (send dc get-pen)]
              [saved-brush (send dc get-brush)])
          (send dc set-pen
                (make-pen #:color (send saved-pen get-color)
                          #:join 'miter))
          (send dc set-brush (send saved-pen get-color) 'solid)
          (define p (new dc-path%))
          (let ([point0 (car points*)])
            (send p move-to (car point0) (cdr point0)))
          (send p lines (cdr points*))
          (send p close)
          (send dc draw-path p)
          (send dc set-brush saved-brush)
          (send dc set-pen saved-pen))))

    (define/override (get-whole-text) "💥")
    ))

(define tainted-icon-snip-class%
  (class snip-class%
    (inherit set-classname set-version)
    (super-new)

    (set-classname (~s '(lib "tainted.rkt" "macro-debugger" "syntax-browser" "icons")))
    (set-version 1)

    (define/override (read f)
      (new tainted-icon-snip%))
    ))

(define snip-class (new tainted-icon-snip-class%))
