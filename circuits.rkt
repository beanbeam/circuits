#lang racket

(require 2htdp/image)

;; CONSTANTS
;; =========

(define BACKGROUND (empty-scene 500 500))
(define WIRE-PEN (pen 'black 3 'solid 'projecting 'bevel))

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
    
    (define/public (draw-on i nodes)
      (let ([s (vector-ref nodes e-start)]
            [e (vector-ref nodes e-end)])
        (scene+line i
                    (node-x s) (node-y s)
                    (node-x e) (node-y e)
                    WIRE-PEN)))))

(define resistor%
  (class wire%
    (init start end resistance)   
    (define res resistance)    
    (super-new [start start] [end end])
    
    (define/public (get-resistance) res)))

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
#;
(define (draw-circuit c)
  (foldr (λ (e) (draw-element e (circuit-nodes c)))
         BACKGROUND
         (circuit-elements c)))