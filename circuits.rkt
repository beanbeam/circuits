#lang racket

(require 2htdp/image)

;; CONSTANTS
;; =========

(define BACKGROUND (empty-scene 500 500))
(define WIRE-PEN (pen 'black 3 'solid 'projecting 'bevel))

(define RESISTOR-SECTION-LENGTH 5)
(define RESISTOR-WIDTH 12)
(define MINIMUM-RESISTOR-LEAD-LENGTH 8)

;; DATA DEFINITIONS
;; ================

(struct circuit (nodes elements))
;; A Circuit is (circuit [Listof Node] [Listof Element])

(struct node (x y))
;; A Node is (node Number Number)

;; An Element is one of:
;; - Wire
;; - Resistor
;; - DCSource
;; ...

(define wire%
  (class object%
    (init start end)
    
    (define e-start start)
    (define e-end end)
    
    (super-new)
    
    (define/public (get-start) e-start)
    (define/public (get-end) e-end)
    
    (define/public (draw-at s e i)
      (scene+line i
                    (node-x s) (node-y s)
                    (node-x e) (node-y e)
                    WIRE-PEN))
    
    (define/public-final (draw-on i nodes)
      (send this draw-at
            (vector-ref nodes e-start)
            (vector-ref nodes e-end)
            i))))

;; foldn : Natural (Natural X -> X) X -> X
;; folds over numbers 0-n
(define (build n f base)
  (local [(define (build-acc n f base c)
            (if (<= n c)
                base
                (f c (build-acc n f base (add1 c)))))]
    (build-acc n f base 0)))

(define resistor%
  (class wire%
    (init start end resistance)   
    (define res resistance)    
    (super-new [start start] [end end])
    
    (define/public (get-resistance) res)
    
    (define/override (draw-at s e i)
      (let* ([d (sqrt (+ (sqr (- (node-x e) (node-x s)))
                         (sqr (- (node-y e) (node-y s)))))]
             [dx (/ (- (node-x e) (node-x s)) d)]
             [dy (/ (- (node-y e) (node-y s)) d)]
             [n (truncate (/ (- d (* 2 MINIMUM-RESISTOR-LEAD-LENGTH)) RESISTOR-SECTION-LENGTH))]
             [lead-length (/ (- d (* n RESISTOR-SECTION-LENGTH)) 2)])
        (scene+line
         (scene+line
          (build (- n 1) (λ (num img)
                           (let ([sx (+ (node-x s) (* (+ num .5) RESISTOR-SECTION-LENGTH dx) (* lead-length dx))]
                                 [sy (+ (node-y s) (* (+ num .5) RESISTOR-SECTION-LENGTH dy) (* lead-length dy))]
                                 [evn (λ (n) (if (even? num) n (- n)))])
                             (scene+line
                              img
                              (+ sx (- (evn (* 1/2 RESISTOR-WIDTH dy))))
                              (+ sy (evn (* 1/2 RESISTOR-WIDTH dx)))
                              (+ (* RESISTOR-SECTION-LENGTH dx) sx
                                 (evn (* 1/2 RESISTOR-WIDTH dy)))
                              (+ (* RESISTOR-SECTION-LENGTH dy) sy
                                 (- (evn (* 1/2 RESISTOR-WIDTH dx))))
                              WIRE-PEN)))
                 i)
          (- (node-x e) (* lead-length dx))
          (- (node-y e) (* lead-length dy))
          (node-x e) (node-y e)
          WIRE-PEN)
         (node-x s) (node-y s)
         (+ (node-x s) (* lead-length dx))
         (+ (node-y s) (* lead-length dy))
         WIRE-PEN)))))

(define dc-source%
  (class wire%
    (init low high difference)   
    (define diff difference)    
    (super-new [start low] [end high])
    
    (define/public (get-low) (send this get-start))
    (define/public (get-high) (send this get-end))
    (define/public (get-difference) diff)))


;; draw-circuit : Circuit -> Image
;; Returns an image representation of c
(define (draw-circuit c)
  (foldr (λ (e i) (send e draw-on i (circuit-nodes c)))
         BACKGROUND
         (circuit-elements c)))

(define test-circuit
  (circuit (vector (node 100 300) (node 230 200)
                   (node 270 200) (node 400 300))
           (list (new wire% [start 0] [end 1])
                 (new resistor% [start 1] [end 2] [resistance 5])
                 (new wire% [start 2] [end 3]))))

(draw-circuit test-circuit)
