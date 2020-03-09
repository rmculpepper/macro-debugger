#lang racket/base
(require racket/class
         racket/format
         racket/list
         racket/snip
         racket/draw
         racket/math
         "base.rkt")
(provide lock-icon-snip%
         snip-class)

(define lock-icon-snip%
  (class one-cell-snip%
    (inherit set-snipclass get/cache-extent)
    (super-new)
    (set-snipclass snip-class)
    (send (get-the-snip-class-list) add snip-class)

    (define/override (draw dc x y left top right bottom dx dy draw-caret?)
      (when draw-caret?
        (let ([saved-pen (send dc get-pen)])
          (define-values (w h d _a) (get/cache-extent dc))
          (define (SX u) (* u w (/ 8)))
          (define (SY u) (* u (- h d) (/ 8)))
          (define (X u) (+ x (SX u)))
          (define (Y u) (+ y (SY u)))
          (send dc set-pen (make-pen #:color (send saved-pen get-color)
                                     #:width (max 2 (/ w 8))))
          (let ([saved-brush (send dc get-brush)])
            ;; Draw loop part
            (send dc set-brush "black" 'transparent)
            (send dc draw-rounded-rectangle (X 2) (Y 1) (SX 4) (SY 6) -0.4)
            ;; Draw solid part
            (send dc set-brush (send saved-pen get-color) 'solid)
            (send dc draw-rectangle (X 1) (Y 4) (SX 6) (SY 4))
            (send dc set-brush saved-brush))
          (send dc set-pen saved-pen))))

    (define/override (get-whole-text) "🔒")
    ))

(define lock-icon-snip-class%
  (class snip-class%
    (inherit set-classname set-version)
    (super-new)

    (set-classname (~s '(lib "lock.rkt" "macro-debugger" "syntax-browser" "icons")))
    (set-version 1)

    (define/override (read f)
      (new lock-icon-snip%))
    ))

(define snip-class (new lock-icon-snip-class%))
