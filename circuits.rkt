#lang racket

(require 2htdp/image)

;; CONSTANTS
;; =========

(define BACKGROUND (empty-scene 500 500))
(define WIRE-PEN (pen 'black 2 'solid 'projecting 'bevel))

(define RESISTOR-SECTION-LENGTH 5)
(define RESISTOR-WIDTH 10)
(define MINIMUM-RESISTOR-LEAD-LENGTH 8)

(define DC-SOURCE-SEPERATION 6)
(define DC-SOURCE-HIGH-WIDTH 18)
(define DC-SOURCE-LOW-WIDTH 10)

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
             [lead-length (/ (- d (* n RESISTOR-SECTION-LENGTH)) 2)]
             [evn (λ (num) (if (even? n) num (- num)))])
        (scene+line
         (scene+line
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
            (+ (node-x e) (- (* lead-length dx))
               (evn (* 1/2 RESISTOR-WIDTH dy))
               (- (* 1/2 RESISTOR-SECTION-LENGTH dx)))
            (+ (node-y e) (- (* lead-length dy))
               (- (evn (* 1/2 RESISTOR-WIDTH dx)))
               (- (* 1/2 RESISTOR-SECTION-LENGTH dy)))
            (- (node-x e) (* lead-length dx))
            (- (node-y e) (* lead-length dy))
            WIRE-PEN)
           (+ (node-x s) (* lead-length dx)
              (- (* 1/2 RESISTOR-WIDTH dy))
              (* 1/2 RESISTOR-SECTION-LENGTH dx))
           (+ (node-y s) (* lead-length dy)
              (* 1/2 RESISTOR-WIDTH dx)
              (* 1/2 RESISTOR-SECTION-LENGTH dy))
           (+ (node-x s) (* lead-length dx))
           (+ (node-y s) (* lead-length dy))
           WIRE-PEN)
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
    (define/public (get-difference) diff)
    (define/override (draw-at low high i)
      (let* ([d (sqrt (+ (sqr (- (node-x high) (node-x low)))
                         (sqr (- (node-y high) (node-y low)))))]
             [dx (/ (- (node-x high) (node-x low)) d)]
             [dy (/ (- (node-y high) (node-y low)) d)]
             [lead-length (/ (- d DC-SOURCE-SEPERATION) 2)])
        (scene+line
         (scene+line
          (scene+line
           (scene+line
            i
            (node-x low) (node-y low)
            (+ (node-x low) (* lead-length dx))
            (+ (node-y low) (* lead-length dy))
            WIRE-PEN)
           (node-x high) (node-y high)
           (- (node-x high) (* lead-length dx))
           (- (node-y high) (* lead-length dy))
           WIRE-PEN)
          (+ (node-x low) (* lead-length dx)
             (* 1/2 DC-SOURCE-LOW-WIDTH dy))
          (+ (node-y low) (* lead-length dy)
             (* -1/2 DC-SOURCE-LOW-WIDTH dx))
          (+ (node-x low) (* lead-length dx)
             (* -1/2 DC-SOURCE-LOW-WIDTH dy))
          (+ (node-y low) (* lead-length dy)
             (* 1/2 DC-SOURCE-LOW-WIDTH dx))
          WIRE-PEN)
         (+ (node-x high) (- (* lead-length dx))
             (* 1/2 DC-SOURCE-HIGH-WIDTH dy))
         (+ (node-y high) (- (* lead-length dy))
             (* -1/2 DC-SOURCE-HIGH-WIDTH dx))
         (+ (node-x high) (- (* lead-length dx))
             (* -1/2 DC-SOURCE-HIGH-WIDTH dy))
         (+ (node-y high) (- (* lead-length dy))
             (* 1/2 DC-SOURCE-HIGH-WIDTH dx))
         WIRE-PEN)))))
        

;; draw-circuit : Circuit -> Image
;; Returns an image representation of c
(define (draw-circuit c)
  (foldr (λ (e i) (send e draw-on i (circuit-nodes c)))
         BACKGROUND
         (circuit-elements c)))

(define test-circuit
  (circuit (vector (node 200 300) (node 200 200)
                   (node 350 250) (node 300 300))
           (list (new wire% [start 0] [end 1])
                 (new resistor% [start 1] [end 2] [resistance 5])
                 (new dc-source% [low 2] [high 3] [difference 9])
                 (new resistor% [start 3] [end 0] [resistance 4]))))

(draw-circuit test-circuit)